#include "erlang_generator.h"

#include <google/protobuf/descriptor.h>
#include <google/protobuf/io/printer.h>
#include <google/protobuf/io/zero_copy_stream.h>

#include <sstream>
#include <stdio.h>

using namespace google::protobuf;
using namespace google::protobuf::compiler;


namespace {

// Some useful private string manipulation functions from libproto.
inline void StripSuffix(const string& suffix, string* s) {
  const int pos = s->rfind(suffix);
  if (pos != string::npos && pos == s->size() - suffix.size()) {
    s->resize(pos);
  }
}

inline void LowerString(string* s) {
  string::iterator end = s->end();
  for (string::iterator i = s->begin(); i != end; ++i) {
    // tolower() changes based on locale.  We don't want this!
    if ('A' <= *i && *i <= 'Z') *i += 'a' - 'A';
  }
}

inline void UpperString(string* s) {
  string::iterator end = s->end();
  for (string::iterator i = s->begin(); i != end; ++i) {
    // toupper() changes based on locale.  We don't want this!
    if ('a' <= *i && *i <= 'z') *i += 'A' - 'a';
  }
}

template <typename T>
inline string ToString(T value) {
  std::stringstream out;
  out << value;
  return out.str();
}

inline void ReplaceChar(char from, char to, string* s) {
  for (string::iterator it = s->begin() ; it != s->end() ; ++it) {
    if (*it == from) {
      *it = to;
    }
  }
}

int CEscapeInternal(const char* src, int src_len, char* dest,
                    int dest_len, bool use_hex, bool utf8_safe) {
  const char* src_end = src + src_len;
  int used = 0;
  bool last_hex_escape = false; // true if last output char was \xNN

  for (; src < src_end; src++) {
    if (dest_len - used < 2)   // Need space for two letter escape
      return -1;

    bool is_hex_escape = false;
    switch (*src) {
      case '\n': dest[used++] = '\\'; dest[used++] = 'n';  break;
      case '\r': dest[used++] = '\\'; dest[used++] = 'r';  break;
      case '\t': dest[used++] = '\\'; dest[used++] = 't';  break;
      case '\"': dest[used++] = '\\'; dest[used++] = '\"'; break;
      case '\'': dest[used++] = '\\'; dest[used++] = '\''; break;
      case '\\': dest[used++] = '\\'; dest[used++] = '\\'; break;
      default:
        // Note that if we emit \xNN and the src character after that is a hex
        // digit then that digit must be escaped too to prevent it being
        // interpreted as part of the character code by C.
        if ((!utf8_safe || static_cast<uint8>(*src) < 0x80) &&
            (!isprint(*src) ||
             (last_hex_escape && isxdigit(*src)))) {
          if (dest_len - used < 4) // need space for 4 letter escape
            return -1;
          sprintf(dest + used, (use_hex ? "\\x%02x" : "\\%03o"),
                  static_cast<uint8>(*src));
          is_hex_escape = use_hex;
          used += 4;
        } else {
          dest[used++] = *src; break;
        }
    }
    last_hex_escape = is_hex_escape;
  }

  if (dest_len - used < 1)   // make sure that there is room for \0
    return -1;

  dest[used] = '\0';   // doesn't count towards return value though
  return used;
}

string CEscape(const string& src) {
  const int dest_length = src.size() * 4 + 1; // Maximum possible expansion
  scoped_array<char> dest(new char[dest_length]);
  const int len = CEscapeInternal(src.data(), src.size(),
                                  dest.get(), dest_length, false, false);
  return string(dest.get(), len);
}


// Returns the Erlang module base name expected for a given .proto filename.
// This is used as the Erlang module name.
string ErlangFilename(const FileDescriptor* file) {
  string basename = file->name();
  StripSuffix(".proto", &basename);

  // Strip everything before the final slash, if there is one.
  const size_t slash_pos = basename.rfind('/');
  if (slash_pos != string::npos) {
    basename = basename.substr(slash_pos);
  }

  return basename;
}

// Returns the name of the Erlang type for a protobuf object.
string ErlangThingName(const string& full_name) {
  string ret = full_name + "_pb";
  ReplaceChar('.', '_', &ret);
  LowerString(&ret);
  return ret;
}

string ErlangThingName(const Descriptor* message) {
  return ErlangThingName(message->full_name());
}

string ErlangThingName(const EnumDescriptor* enum_des) {
  return ErlangThingName(enum_des->full_name());
}

} // namespace


ErlangGenerator::ErlangGenerator() {
}

ErlangGenerator::~ErlangGenerator() {
}

bool ErlangGenerator::Generate(const FileDescriptor* file,
                         const string& parameter,
                         GeneratorContext* context,
                         string* error) const {

  string basename = ErlangFilename(file);
  string erl_filename = basename + ".erl";
  string hrl_filename = basename + ".hrl";

  scoped_ptr<io::ZeroCopyOutputStream> erl(context->Open(erl_filename));
  scoped_ptr<io::ZeroCopyOutputStream> hrl(context->Open(hrl_filename));

  io::Printer erl_printer(erl.get(), '$');
  io::Printer hrl_printer(hrl.get(), '$');

  // Print the common stuff at the top of the erl file.
  erl_printer.Print("-module($basename$).\n"
                    "-include(\"protobuf.hrl\").\n"
                    "-include(\"$basename$.hrl\").\n"
                    "\n",
                    "basename", basename);
  GenerateErlExports(file, &erl_printer);
  erl_printer.Print("\n");

  // Print enum types
  for (int i=0 ; i<file->enum_type_count() ; ++i) {
    const EnumDescriptor* enum_des = file->enum_type(i);
    GenerateEnum(enum_des, &erl_printer, &hrl_printer);
  }

  // Print messages
  for (int i=0; i<file->message_type_count(); ++i) {
    const Descriptor* message = file->message_type(i);
    GenerateMessage(message, &erl_printer, &hrl_printer);
  }

  return true;
}

void ErlangGenerator::GenerateErlExports(const FileDescriptor* file, Printer* erl) const {
  for (int i=0; i<file->enum_type_count(); ++i) {
    GenerateErlExports(file->enum_type(i), erl);
  }
  for (int i=0; i<file->message_type_count(); ++i) {
    GenerateErlExports(file->message_type(i), erl);
  }
}

void ErlangGenerator::GenerateErlExports(const Descriptor* message, Printer* erl) const {
  erl->Print("-export([decode_$name$/1, encode_$name$/1]).\n",
             "name", ErlangThingName(message));

  // Embedded types
  for (int i=0 ; i<message->enum_type_count() ; ++i) {
    GenerateErlExports(message->enum_type(i), erl);
  }
  for (int i=0 ; i<message->nested_type_count() ; ++i) {
    GenerateErlExports(message->nested_type(i), erl);
  }
}

void ErlangGenerator::GenerateErlExports(const EnumDescriptor* enum_des, Printer* erl) const {
  erl->Print("-export([$name$_name/1, $name$_value/1]).\n",
             "name", ErlangThingName(enum_des));
}

void ErlangGenerator::GenerateEnum(const EnumDescriptor* des,
                                   Printer* erl,
                                   Printer* hrl) const {
  string enumname = ErlangThingName(des);
  string enumname_upper = enumname;
  UpperString(&enumname_upper);

  map<string, string> variables;
  variables["name"] = des->name();
  variables["enumname"] = enumname;
  variables["enumname_upper"] = enumname_upper;

  hrl->Print(variables, "-define($enumname_upper$_VALUES, [\n");

  for (int i=0 ; i<des->value_count() ; ++i) {
    if (i != 0) {
      hrl->Print(",\n");
    }

    const EnumValueDescriptor* value = des->value(i);
    variables["valuename"] = value->name();
    variables["valuenumber"] = ToString(value->number());

    hrl->Print(variables, "  {'$valuename$', $valuenumber$}");
  }

  hrl->Print("\n]).\n\n");

  erl->Print(variables,
             "$enumname$_name(Value) ->\n"
             "  protobuf:enum_name(Value, ?$enumname_upper$_VALUES).\n"
             "\n"
             "$enumname$_value(Name) ->\n"
             "  protobuf:enum_value(Name, ?$enumname_upper$_VALUES).\n"
             "\n"
             "\n");
}

void ErlangGenerator::GenerateMessage(const Descriptor* message,
                                      Printer* erl,
                                      Printer* hrl) const {
  string messagename = ErlangThingName(message);
  string messagename_upper = messagename;
  UpperString(&messagename_upper);

  map<string, string> variables;
  variables["name"] = message->full_name();
  variables["messagename"] = messagename;
  variables["messagename_upper"] = messagename_upper;

  // Start the record type
  hrl->Print(variables, "-record($messagename$, {\n");

  // Output each field into the record type
  for (int i=0 ; i<message->field_count() ; ++i) {
    if (i != 0) {
      hrl->Print(",\n");
    }

    const FieldDescriptor* field = message->field(i);

    variables["field_name"] = field->name();
    variables["default_assignment"] = FieldDefaultValue(field) == "undefined" ?
          "" : " = " + FieldDefaultValue(field);

    hrl->Print(variables, "  '$field_name$'$default_assignment$");
  }

  // Finish the record type
  hrl->Print("\n}).\n\n");

  // Start the definition
  hrl->Print(variables,
             "-define($messagename_upper$_DEFINITION, #message_definition{\n"
             "  name = <<\"$name$\">>,\n"
             "  fields = [\n");

  // Output each field into the definition
  for (int i=0 ; i<message->field_count() ; ++i) {
    if (i != 0) {
      hrl->Print(",\n");
    }

    const FieldDescriptor* field = message->field(i);

    variables["field_name"] = field->name();
    variables["field_type"] = FieldTypeAtom(field);
    variables["field_number"] = ToString(field->number());
    variables["label_atom"] = FieldLabelAtom(field);
    variables["nested_type"] = FieldNestedTypeTuple(field);
    variables["default_value"] = FieldDefaultValue(field);
    variables["enum_functions"] = FieldEnumFunctionsTuple(field);

    hrl->Print(variables,
        "    #field_definition{\n"
        "      name = <<\"$field_name$\">>,\n"
        "      type = '$field_type$',\n"
        "      number = $field_number$,\n"
        "      label = '$label_atom$',\n"
        "      nested_type = $nested_type$,\n"
        "      default_value = $default_value$,\n"
        "      enum_functions = $enum_functions$\n"
        "    }");
  }

  // Finish definition
  hrl->Print("\n  ]\n}).\n\n");


  // Output the decoding function
  erl->Print(variables,
      "decode_$messagename$(Binary) when is_binary(Binary) ->\n"
      "  Items = protobuf:decode_items(Binary, ?$messagename_upper$_DEFINITION),\n"
      "  #$messagename${\n"
      );

  for (int i=0 ; i<message->field_count() ; ++i) {
    if (i != 0) {
      erl->Print(",\n");
    }

    const FieldDescriptor* field = message->field(i);

    variables["field_name"] = field->name();
    variables["field_number"] = ToString(field->number());

    erl->Print(variables,
        "    '$field_name$' = protobuf:find_field("
                 "Items, $field_number$, ?$messagename_upper$_DEFINITION)"
        );
  }

  erl->Print("\n  }.\n\n");


  // Output the encoding function
  erl->Print(variables,
      "encode_$messagename$(Record) when is_record(Record, $messagename$) ->\n"
      "  << ");

  for (int i=0 ; i<message->field_count() ; ++i) {
    if (i != 0) {
      erl->Print(",\n     ");
    }

    const FieldDescriptor* field = message->field(i);

    variables["field_name"] = field->name();
    variables["field_number"] = ToString(field->number());

    erl->Print(variables,
        "(protobuf:encode_field(Record#$messagename$.$field_name$, "
                               "$field_number$, "
                               "?$messagename_upper$_DEFINITION))"
        "/binary");
  }

  erl->Print(" >>.\n\n");


  // Handle embedded types
  for (int i=0 ; i<message->enum_type_count() ; ++i) {
    GenerateEnum(message->enum_type(i), erl, hrl);
  }
  for (int i=0 ; i<message->nested_type_count() ; ++i) {
    GenerateMessage(message->nested_type(i), erl, hrl);
  }
}

string ErlangGenerator::FieldTypeAtom(const FieldDescriptor* field) const {
  switch (field->type()) {
    case FieldDescriptor::TYPE_DOUBLE:   return "double";
    case FieldDescriptor::TYPE_FLOAT:    return "float";
    case FieldDescriptor::TYPE_INT64:    return "int64";
    case FieldDescriptor::TYPE_UINT64:   return "uint64";
    case FieldDescriptor::TYPE_INT32:    return "int32";
    case FieldDescriptor::TYPE_FIXED64:  return "fixed64";
    case FieldDescriptor::TYPE_FIXED32:  return "fixed32";
    case FieldDescriptor::TYPE_BOOL:     return "bool";
    case FieldDescriptor::TYPE_STRING:   return "string";
    case FieldDescriptor::TYPE_GROUP:    return "group";
    case FieldDescriptor::TYPE_MESSAGE:  return "message";
    case FieldDescriptor::TYPE_BYTES:    return "bytes";
    case FieldDescriptor::TYPE_UINT32:   return "uint32";
    case FieldDescriptor::TYPE_ENUM:     return "enum";
    case FieldDescriptor::TYPE_SFIXED32: return "sfixed32";
    case FieldDescriptor::TYPE_SFIXED64: return "sfixed64";
    case FieldDescriptor::TYPE_SINT32:   return "sint32";
    case FieldDescriptor::TYPE_SINT64:   return "sint64";
  }
  return "undefined";
}

string ErlangGenerator::FieldLabelAtom(const FieldDescriptor* field) const {
  switch (field->label()) {
    case FieldDescriptor::LABEL_OPTIONAL: return "optional";
    case FieldDescriptor::LABEL_REPEATED: return "repeated";
    case FieldDescriptor::LABEL_REQUIRED: return "required";
  }
  return "undefined";
}

string ErlangGenerator::FieldNestedTypeTuple(const FieldDescriptor* field) const {
  if (field->type() != FieldDescriptor::TYPE_MESSAGE) {
    return "undefined";
  }

  string module = ErlangFilename(field->message_type()->file());
  string decode = "decode_" + ErlangThingName(field->message_type());
  string encode = "encode_" + ErlangThingName(field->message_type());
  return "{'" + module + "', '" + decode + "', '" + encode + "'}";
}

string ErlangGenerator::FieldDefaultValue(const FieldDescriptor* field) const {
  if (!field->has_default_value()) {
    return "undefined";
  }

  switch (field->cpp_type()) {
    case FieldDescriptor::CPPTYPE_BOOL:
      return field->default_value_bool() ? "true" : "false";
    case FieldDescriptor::CPPTYPE_INT32:
      return ToString(field->default_value_int32());
    case FieldDescriptor::CPPTYPE_INT64:
      return ToString(field->default_value_int64());
    case FieldDescriptor::CPPTYPE_UINT32:
      return ToString(field->default_value_uint32());
    case FieldDescriptor::CPPTYPE_UINT64:
      return ToString(field->default_value_uint64());
    case FieldDescriptor::CPPTYPE_FLOAT:
      return ToString(field->default_value_float());
    case FieldDescriptor::CPPTYPE_DOUBLE:
      return ToString(field->default_value_double());
    case FieldDescriptor::CPPTYPE_STRING:
      return "<<\"" + CEscape(field->default_value_string()) + "\">>";
    case FieldDescriptor::CPPTYPE_ENUM:
      return "'" + field->default_value_enum()->name() + "'";
    default:
      break;
  }

  return "undefined";
}

string ErlangGenerator::FieldEnumFunctionsTuple(const FieldDescriptor* field) const {
  if (field->type() != FieldDescriptor::TYPE_ENUM) {
    return "undefined";
  }

  string module     = ErlangFilename(field->containing_type()->file());
  string name_func  = ErlangThingName(field->enum_type()) + "_name";
  string value_func = ErlangThingName(field->enum_type()) + "_value";

  return "{'" + module + "', '" + name_func + "', '" + value_func + "'}";
}

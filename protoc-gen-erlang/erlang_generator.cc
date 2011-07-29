#include "erlang_generator.h"

#include <google/protobuf/descriptor.h>
#include <google/protobuf/io/printer.h>
#include <google/protobuf/io/zero_copy_stream.h>

#include <sstream>

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

inline string SimpleItoa(int value) {
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
    variables["valuenumber"] = SimpleItoa(value->number());

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
  variables["name"] = message->name();
  variables["messagename"] = messagename;
  variables["messagename_upper"] = messagename_upper;

  // Start the record type
  hrl->Print(variables, "-record($messagename$, {\n");

  // Output each field into the record type
  for (int i=0 ; i<message->field_count() ; ++i) {
    if (i != 0) {
      hrl->Print(",\n");
    }
    variables["field_name"] = message->field(i)->name();
    hrl->Print(variables, "  '$field_name$'");
  }

  // Finish the record type
  hrl->Print("\n}).\n\n");

  // Start the definition
  hrl->Print(variables,
             "-define($messagename_upper$_DEFINITION, #message_definition{\n"
             "  name = \"$name$\",\n"
             "  fields = [\n");

  // Output each field into the definition
  for (int i=0 ; i<message->field_count() ; ++i) {
    if (i != 0) {
      hrl->Print(",\n");
    }

    const FieldDescriptor* field = message->field(i);

    variables["field_name"] = field->name();
    variables["field_type"] = FieldTypeAtom(field);
    variables["field_number"] = SimpleItoa(field->number());
    variables["label_atom"] = FieldLabelAtom(field);
    variables["nested_type"] = FieldNestedTypeTuple(field);
    variables["enum_functions"] = FieldEnumFunctionsTuple(field);

    hrl->Print(variables,
        "    #field_definition{\n"
        "      name = <<\"$field_name$\">>,\n"
        "      type = '$field_type$',\n"
        "      number = $field_number$,\n"
        "      label = '$label_atom$',\n"
        "      nested_type = $nested_type$,\n"
        "      enum_functions = $enum_functions$\n"
        "    }");
  }

  // Finish definition
  hrl->Print("\n  ]\n}).\n\n");


  // Output the decoding function
  erl->Print(variables,
      "decode_$messagename$(Binary) ->\n"
      "  Items = protobuf:decode_items(Binary, ?$messagename_upper$_DEFINITION),\n"
      "  #$messagename${\n"
      );

  for (int i=0 ; i<message->field_count() ; ++i) {
    if (i != 0) {
      erl->Print(",\n");
    }

    const FieldDescriptor* field = message->field(i);

    variables["field_name"] = field->name();
    variables["field_number"] = SimpleItoa(field->number());

    erl->Print(variables,
        "    '$field_name$' = protobuf:find_field("
                 "Items, $field_number$, ?$messagename_upper$_DEFINITION)"
        );
  }

  erl->Print("\n  }.\n\n");


  // Output the encoding function
  erl->Print(variables,
      "encode_$messagename$(Record) ->\n"
      "  << ");

  for (int i=0 ; i<message->field_count() ; ++i) {
    if (i != 0) {
      erl->Print(",\n     ");
    }

    const FieldDescriptor* field = message->field(i);

    variables["field_name"] = field->name();
    variables["field_number"] = SimpleItoa(field->number());

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

  string module   = ErlangFilename(field->message_type()->file());
  string decode = "decode_" + ErlangThingName(field->message_type());
  string encode = "encode_" + ErlangThingName(field->message_type());
  return "{'" + module + "', '" + decode + "', '" + encode + "'}";
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

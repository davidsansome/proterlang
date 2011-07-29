#include "erlang_generator.h"

#include <google/protobuf/descriptor.h>
#include <google/protobuf/io/printer.h>
#include <google/protobuf/io/zero_copy_stream.h>

#include <sstream>

using std::string;

using namespace google::protobuf;
using namespace google::protobuf::compiler;

namespace {

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

string ErlangThingName(const string& modulename) {
  string ret = modulename + "_pb";
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

  // Print the erl header
  erl_printer.Print("-module($basename$).\n"
                    "-include(\"protobuf.hrl\").\n"
                    "-include(\"$basename$.hrl\").\n"
                    "\n",
                    "basename", basename);
  GenerateErlExports(file, &erl_printer);
  erl_printer.Print("\n");

  for (int i=0; i<file->message_type_count(); ++i) {
    const Descriptor* message = file->message_type(i);
    GenerateMessage(message, context, &erl_printer, &hrl_printer);
  }

  return true;
}

void ErlangGenerator::GenerateErlExports(
    const google::protobuf::FileDescriptor* file,
    google::protobuf::io::Printer* erl) const {
  for (int i=0; i<file->enum_type_count(); ++i) {
    GenerateErlExports(file->enum_type(i), erl);
  }
  for (int i=0; i<file->message_type_count(); ++i) {
    GenerateErlExports(file->message_type(i), erl);
  }
}

void ErlangGenerator::GenerateErlExports(
    const google::protobuf::Descriptor* message,
    google::protobuf::io::Printer* erl) const {
  erl->Print("-export([decode_$name$/1]).\n",
             "name", ErlangThingName(message));

  for (int i=0 ; i<message->enum_type_count() ; ++i) {
    GenerateErlExports(message->enum_type(i), erl);
  }
  for (int i=0 ; i<message->nested_type_count() ; ++i) {
    GenerateErlExports(message->nested_type(i), erl);
  }
}

void ErlangGenerator::GenerateErlExports(
    const google::protobuf::EnumDescriptor* enum_des,
    google::protobuf::io::Printer* erl) const {
  // TODO
}

void ErlangGenerator::GenerateMessage(const Descriptor* message,
                                      GeneratorContext* context,
                                      io::Printer* erl,
                                      io::Printer* hrl) const {
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
    hrl->Print(variables, "  $field_name$");
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
    variables["field_number"] = SimpleItoa(field->number());
    variables["label_atom"] = "undefined";
    variables["nested_type"] = "undefined";

    switch(field->label()) {
      case FieldDescriptor::LABEL_OPTIONAL:
        variables["label_atom"] = "optional";
        break;
      case FieldDescriptor::LABEL_REPEATED:
        variables["label_atom"] = "repeated";
        break;
      case FieldDescriptor::LABEL_REQUIRED:
        variables["label_atom"] = "required";
        break;
    }

    if (field->type() == FieldDescriptor::TYPE_MESSAGE) {
      variables["nested_type"] = "{" +
          ErlangFilename(field->message_type()->file()) + ", " +
          "decode_" + ErlangThingName(field->message_type()) + "}";
    }

    hrl->Print(variables,
        "    #field_definition{\n"
        "      name = \"$field_name$\",\n"
        "      number = $field_number$,\n"
        "      label = $label_atom$,\n"
        "      nested_type = $nested_type$\n"
        "    }");
  }

  // Finish definition
  hrl->Print("\n  ]\n}).\n\n");


  // Output the decoding function
  erl->Print(variables,
      "decode_$messagename$(Binary) ->\n"
      "  Items = protobuf:decode_items(Binary),\n"
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
        "    $field_name$ = protobuf:find_field("
                 "Items, $field_number$, ?$messagename_upper$_DEFINITION)"
        );
  }

  erl->Print("\n  }.\n\n");
}

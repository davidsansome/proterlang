#include "erlang_generator.h"

#include <google/protobuf/descriptor.h>
#include <google/protobuf/io/printer.h>
#include <google/protobuf/io/zero_copy_stream.h>
#include <google/protobuf/compiler/cpp/cpp_helpers.h>
#include <google/protobuf/stubs/strutil.h>

namespace google {
namespace protobuf {
namespace compiler {
namespace erlang {

namespace {

// Returns the Erlang module base name expected for a given .proto filename.
string ModuleBaseName(const string& filename) {
  string basename = cpp::StripProto(filename);
  StripString(&basename, "-", '_');
  StripString(&basename, "/", '_');
  return basename;
}

string MessageName(const string& module, const Descriptor* message) {
  string ret = module + "_" + message->name() + "_pb";
  LowerString(&ret);
  return ret;
}

} // namespace


Generator::Generator() {
}

Generator::~Generator() {
}

bool Generator::Generate(const FileDescriptor* file,
                         const string& parameter,
                         GeneratorContext* generator_context,
                         string* error) const {
  string basename = ModuleBaseName(file->name());

  for (int i=0; i<file->message_type_count(); ++i) {
    const Descriptor* message = file->message_type(i);
    string messagename = MessageName(basename, message);

    GenerateMessageHeader(messagename, message, generator_context);
    GenerateMessageSource(messagename, message, generator_context);
  }

  return true;
}

void Generator::GenerateMessageHeader(const string& messagename,
                                      const Descriptor* message,
                                      GeneratorContext* context) const {
  string filename = messagename + ".hrl";

  scoped_ptr<io::ZeroCopyOutputStream> output(context->Open(filename));
  io::Printer printer(output.get(), '$');

  string upper_name = messagename;
  UpperString(&upper_name);

  printer.Print(
      "-include(\"protobuf.hrl\").\n"
      "\n"
      "-record($messagename$, {\n",
      "messagename", messagename);

  for (int i=0 ; i<message->field_count() ; ++i) {
    if (i != 0) {
      printer.Print(",\n");
    }
    printer.Print("  $field_name$",
                  "field_name", message->field(i)->name());
  }

  printer.Print(
      "\n"
      "}).\n"
      "\n"
      "-define($upper_name$_DEFINITION, #message_definition{\n"
      "  name = \"$name$\",\n"
      "  fields = [\n",
      "upper_name", upper_name,
      "name", message->name());

  for (int i=0 ; i<message->field_count() ; ++i) {
    if (i != 0) {
      printer.Print(",\n");
    }

    const FieldDescriptor* field = message->field(i);

    map<string, string> variables;
    variables["name"] = field->name();
    variables["number"] = SimpleItoa(field->number());
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
      string modulename = ModuleBaseName(field->message_type()->file()->name());
      variables["nested_type"] = MessageName(modulename, field->message_type());
    }

    printer.Print(variables,
        "    #field_definition{\n"
        "      name = \"$name$\",\n"
        "      number = $number$,\n"
        "      label = $label_atom$,\n"
        "      nested_type = $nested_type$\n"
        "    }");
  }

  printer.Print(
      "\n"
      "  ]\n"
      "}).\n"
      );
}

void Generator::GenerateMessageSource(const string& messagename,
                                      const Descriptor* message,
                                      GeneratorContext* context) const {
  string filename = messagename + ".erl";

  scoped_ptr<io::ZeroCopyOutputStream> output(context->Open(filename));
  io::Printer printer(output.get(), '$');

  string messagename_upper = messagename;
  UpperString(&messagename_upper);

  map<string, string> variables;
  variables["messagename"] = messagename;
  variables["messagename_upper"] = messagename_upper;

  printer.Print(variables,
      "-module($messagename$).\n"
      "-export([decode_file/1, decode_binary/1]).\n"
      "-include(\"$messagename$.hrl\").\n"
      "\n"
      "decode_file(Filename) ->\n"
      "  case file:read_file(Filename) of\n"
      "    {ok, Binary} ->\n"
      "      decode_binary(Binary);\n"
      "    {error, _Reason} = Error ->\n"
      "      Error\n"
      "  end.\n"
      "\n"
      "decode_binary(Binary) ->\n"
      "  Items = protobuf:decode_items(Binary),\n"
      "  #$messagename${\n"
      );

  for (int i=0 ; i<message->field_count() ; ++i) {
    if (i != 0) {
      printer.Print(",\n");
    }

    const FieldDescriptor* field = message->field(i);

    variables["fieldname"] = field->name();
    variables["number"] = SimpleItoa(field->number());

    printer.Print(variables,
        "    $fieldname$ = protobuf:find_field("
                 "Items, $number$, ?$messagename_upper$_DEFINITION)"
        );
  }

  printer.Print("\n  }.\n");
}


} // namespace erlang
} // namespace compiler
} // namespace protobuf
} // namespace google
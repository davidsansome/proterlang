#ifndef GOOGLE_PROTOBUF_COMPILER_ERLANG_GENERATOR_H__
#define GOOGLE_PROTOBUF_COMPILER_ERLANG_GENERATOR_H__

#include <string>

#include <google/protobuf/compiler/code_generator.h>
#include <google/protobuf/stubs/common.h>

namespace google {
  namespace protobuf {
    class Descriptor;
    class EnumDescriptor;
    class EnumValueDescriptor;
    class FieldDescriptor;
    class ServiceDescriptor;

    namespace io {
      class Printer;
    }
  }
}

using google::protobuf::Descriptor;
using google::protobuf::EnumDescriptor;
using google::protobuf::FieldDescriptor;
using google::protobuf::FileDescriptor;
using google::protobuf::compiler::CodeGenerator;
using google::protobuf::compiler::GeneratorContext;
using google::protobuf::io::Printer;
using std::string;


class ErlangGenerator : public CodeGenerator {
public:
  ErlangGenerator();
  virtual ~ErlangGenerator();

  virtual bool Generate(
      const FileDescriptor* file,
      const string& parameter,
      GeneratorContext* generator_context,
      string* error) const;

private:
  void GenerateErlExports(const FileDescriptor* file, Printer* erl) const;
  void GenerateErlExports(const Descriptor* message, Printer* erl) const;
  void GenerateErlExports(const EnumDescriptor* enum_des, Printer* erl) const;

  void GenerateEnum(const EnumDescriptor* des, Printer* erl, Printer* hrl) const;
  void GenerateMessage(const Descriptor* msg, Printer* erl, Printer* hrl) const;

  string FieldTypeAtom(const FieldDescriptor* field) const;
  string FieldLabelAtom(const FieldDescriptor* field) const;
  string FieldNestedTypeTuple(const FieldDescriptor* field) const;
};

#endif // GENERATOR_H

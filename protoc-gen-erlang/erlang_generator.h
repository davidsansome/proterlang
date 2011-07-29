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


class ErlangGenerator : public google::protobuf::compiler::CodeGenerator {
public:
  ErlangGenerator();
  virtual ~ErlangGenerator();

  virtual bool Generate(
      const google::protobuf::FileDescriptor* file,
      const std::string& parameter,
      google::protobuf::compiler::GeneratorContext* generator_context,
      std::string* error) const;

private:
  void GenerateMessage(
      const google::protobuf::Descriptor* msg,
      google::protobuf::compiler::GeneratorContext* context,
      google::protobuf::io::Printer* erl,
      google::protobuf::io::Printer* hrl) const;

  void GenerateErlExports(
      const google::protobuf::FileDescriptor* file,
      google::protobuf::io::Printer* erl) const;
  void GenerateErlExports(
      const google::protobuf::Descriptor* message,
      google::protobuf::io::Printer* erl) const;
  void GenerateErlExports(
      const google::protobuf::EnumDescriptor* enum_des,
      google::protobuf::io::Printer* erl) const;
};

#endif // GENERATOR_H

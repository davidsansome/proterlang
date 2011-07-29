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
  }
}

using google::protobuf::Descriptor;
using google::protobuf::FileDescriptor;
using google::protobuf::compiler::GeneratorContext;


class ErlangGenerator : public google::protobuf::compiler::CodeGenerator {
public:
  ErlangGenerator();
  virtual ~ErlangGenerator();

  virtual bool Generate(const FileDescriptor* file,
                        const std::string& parameter,
                        GeneratorContext* generator_context,
                        std::string* error) const;

private:
  void GenerateMessageHeader(const std::string& messagename,
                             const Descriptor* msg,
                             GeneratorContext* context) const;
  void GenerateMessageSource(const std::string& messagename,
                             const Descriptor* msg,
                             GeneratorContext* context) const;
};

#endif // GENERATOR_H

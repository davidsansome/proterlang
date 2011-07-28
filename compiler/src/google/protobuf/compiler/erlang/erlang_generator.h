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

namespace compiler {
namespace erlang {

class LIBPROTOC_EXPORT Generator : public CodeGenerator {
public:
  Generator();
  virtual ~Generator();

  virtual bool Generate(const FileDescriptor* file,
                        const string& parameter,
                        GeneratorContext* generator_context,
                        string* error) const;

private:
  void GenerateMessageHeader(const string& messagename,
                             const Descriptor* msg,
                             GeneratorContext* context) const;
  void GenerateMessageSource(const string& messagename,
                             const Descriptor* msg,
                             GeneratorContext* context) const;
};

}
}
}
}

#endif // GENERATOR_H

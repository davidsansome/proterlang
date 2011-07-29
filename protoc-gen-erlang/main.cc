#include "erlang_generator.h"

#include <google/protobuf/compiler/plugin.h>

int main(int argc, char** argv) {
  ErlangGenerator generator;
  return google::protobuf::compiler::PluginMain(argc, argv, &generator);
}


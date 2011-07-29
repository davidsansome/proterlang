add_executable(protoc-gen-erlang
  erlang_generator.cc
  main.cc
)

target_link_libraries(protoc-gen-erlang
  protobuf
  protoc
)


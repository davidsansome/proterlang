add_erlang(SOURCES
  protobuf.erl
  protobuf_tests.erl
)

compile_erlang(proterlang_source ${SOURCES})

# Compile the test protobufs
add_custom_target(testmessages
  COMMAND env PATH=${CMAKE_BINARY_DIR}/protoc-gen-erlang
          ${PROTOC_PROGRAM}
            --erlang_out=${CMAKE_CURRENT_BINARY_DIR}
            --proto_path=${CMAKE_CURRENT_SOURCE_DIR}
            ${CMAKE_CURRENT_SOURCE_DIR}/testmessages.proto
  COMMAND ${ERLANG_COMPILE}
            -o ${CMAKE_CURRENT_BINARY_DIR}/
            ${CMAKE_CURRENT_BINARY_DIR}/testmessages.erl
  DEPENDS ${CMAKE_BINARY_DIR}/protoc-gen-erlang/protoc-gen-erlang
          ${CMAKE_CURRENT_SOURCE_DIR}/testmessages.proto
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

test_erlang(proterlang_source protobuf_tests)
add_dependencies(test_protobuf_tests testmessages)

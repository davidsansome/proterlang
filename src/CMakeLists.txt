add_erlang(SOURCES
  protobuf.erl
  protobuf_tests.erl
)

compile_erlang(${SOURCES})

test_erlang(protobuf_tests)

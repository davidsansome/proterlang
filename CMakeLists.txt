cmake_minimum_required(VERSION 2.8)

include(cmake/Erlang.cmake)

find_library(PROTOC_LIBRARY protoc)
find_library(PROTOBUF_LIBRARY protobuf)

add_subdirectory(protoc-gen-erlang)
add_subdirectory(src)


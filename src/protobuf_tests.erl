-module(protobuf_tests).
-include_lib("eunit/include/eunit.hrl").

encode_varint_test() ->
  % Single byte values
  ?assert(<<0>> =:= protobuf:encode_varint(0)),
  ?assert(<<1>> =:= protobuf:encode_varint(1)),
  ?assert(<<42>> =:= protobuf:encode_varint(42)),

  % Two byte values
  ?assert(<<1:1, 0:7, 0:1, 1:7>> =:= protobuf:encode_varint(128)),
  ?assert(<<1:1, 1:7, 0:1, 1:7>> =:= protobuf:encode_varint(129)),
  ?assert(<<1:1, 0:7, 0:1, 2:7>> =:= protobuf:encode_varint(256)),

  % Three byte values
  ?assert(<<1:1, 0:7, 1:1, 0:7, 0:1, 1:7>> =:= protobuf:encode_varint(16384)),
  ?assert(<<1:1, 2#0110101:7, 1:1, 2#1001011:7, 0:1, 2#10:7>> =:= protobuf:encode_varint(42421)).


decode_varint_test() ->
  % Single byte values
  ?assert({0, <<>>} =:= protobuf:decode_varint(<<0>>)),
  ?assert({1, <<>>} =:= protobuf:decode_varint(<<1>>)),
  ?assert({42, <<>>} =:= protobuf:decode_varint(<<42>>)).


varint_test() ->
  lists:foreach(
    fun(N) ->
      {Value, <<>>} = protobuf:decode_varint(protobuf:encode_varint(N)),
      ?assert(Value =:= N)
    end,
    [erlang:trunc(math:pow(2, X)) || X <- lists:seq(1, 64)]
  ).

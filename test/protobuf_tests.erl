-module(protobuf_tests).
-include_lib("eunit/include/eunit.hrl").
-include("protobuf.hrl").
-include("testmessages.hrl").

-export([enum_name/1, enum_value/1]).

encode_varint_test() ->
  % Single byte values
  ?assertEqual(<<0>>, protobuf:encode_varint(0)),
  ?assertEqual(<<1>>, protobuf:encode_varint(1)),
  ?assertEqual(<<42>>, protobuf:encode_varint(42)),

  % Two byte values
  ?assertEqual(<<1:1, 0:7, 0:1, 1:7>>, protobuf:encode_varint(128)),
  ?assertEqual(<<1:1, 1:7, 0:1, 1:7>>, protobuf:encode_varint(129)),
  ?assertEqual(<<1:1, 0:7, 0:1, 2:7>>, protobuf:encode_varint(256)),

  % Three byte values
  ?assertEqual(<<1:1, 0:7, 1:1, 0:7, 0:1, 1:7>>, protobuf:encode_varint(16384)),
  ?assertEqual(<<1:1, 2#0110101:7, 1:1, 2#1001011:7, 0:1, 2#10:7>>, protobuf:encode_varint(42421)).


decode_varint_test() ->
  % Single byte values
  ?assertEqual({0, <<>>}, protobuf:decode_varint(<<0>>)),
  ?assertEqual({1, <<>>}, protobuf:decode_varint(<<1>>)),
  ?assertEqual({42, <<>>}, protobuf:decode_varint(<<42>>)).


varint_test() ->
  lists:foreach(
    fun(N) ->
      {Value, <<>>} = protobuf:decode_varint(protobuf:encode_varint(N)),
      ?assertEqual(N, Value)
    end,
    [erlang:trunc(math:pow(2, X)) || X <- lists:seq(1, 64)]
  ).


encode_key_test() ->
  ?assertEqual(0, protobuf:encode_key(0, 0)),
  ?assertEqual(1, protobuf:encode_key(1, 0)),
  ?assertEqual(7, protobuf:encode_key(7, 0)),
  ?assertError(function_clause, protobuf:encode_key(8, 0)),
  ?assertEqual(8, protobuf:encode_key(0, 1)),
  ?assertEqual(9, protobuf:encode_key(1, 1)),
  ?assertEqual(17, protobuf:encode_key(1, 2)).


key_test() ->
  ?assertEqual({1,2}, protobuf:decode_key(protobuf:encode_key(1,2))),
  ?assertEqual({3,4}, protobuf:decode_key(protobuf:encode_key(3,4))),
  ?assertEqual({5,6}, protobuf:decode_key(protobuf:encode_key(5,6))).


encode(Value, Definition) ->
  protobuf:encode_item(protobuf:wire_type(Definition#field_definition.type),
                       protobuf:encode_item_value(Value, Definition)).


encode_int_test() ->
  % Normal ints
  lists:foreach(
    fun(Definition) ->
      ?assertEqual(<<0:8>>, encode(0, Definition)),
      ?assertEqual(<<1:8>>, encode(1, Definition)),
      ?assertEqual(<<1:1, 0:7, 0:1, 1:7>>, encode(128, Definition)),
      ?assertEqual(<<16#FFFFFFFFFFFFFFFFFF01:80>>, encode(-1, Definition)),
      ?assertEqual(<<16#FEFFFFFFFFFFFFFFFF01:80>>, encode(-2, Definition))
    end,
    [#field_definition{type = int32}, #field_definition{type = int64}]
  ),

  % Unsigned ints
  lists:foreach(
    fun(Definition) ->
      ?assertEqual(<<0:8>>, encode(0, Definition)),
      ?assertEqual(<<1:8>>, encode(1, Definition)),
      ?assertEqual(<<1:1, 0:7, 0:1, 1:7>>, encode(128, Definition)),
      ?assertEqual(<<16#FFFFFFFFFF1F:48>>, encode(16#FFFFFFFFFF, Definition)),
      ?assertError(function_clause, encode(-1, Definition))
    end,
    [#field_definition{type = uint32}, #field_definition{type = uint64}]
  ),

  % Signed ints with zig zag encoding
  lists:foreach(
    fun(Definition) ->
      ?assertEqual(<<0:8>>, encode(0, Definition)),
      ?assertEqual(<<1:8>>, encode(-1, Definition)),
      ?assertEqual(<<2:8>>, encode(1, Definition)),
      ?assertEqual(<<3:8>>, encode(-2, Definition)),
      ?assertEqual(<<4:8>>, encode(2, Definition)),
      ?assertEqual(<<5:8>>, encode(-3, Definition)),
      ?assertEqual(<<6:8>>, encode(3, Definition)),
      ?assertEqual(<<16#FFFFFFFF0F:40>>, encode(-2147483648, Definition)),
      ?assertEqual(<<16#FEFFFFFF0F:40>>, encode(2147483647, Definition))
    end,
    [#field_definition{type = sint32}, #field_definition{type = sint64}]
  ).


encode_bool_test() ->
  Definition = #field_definition{type = bool},
  ?assertEqual(<<0>>, encode(0, Definition)),
  ?assertEqual(<<0>>, encode(false, Definition)),
  ?assertEqual(<<1>>, encode(1, Definition)),
  ?assertEqual(<<1>>, encode(true, Definition)).


enum_value(bob) -> 1;
enum_value(fred) -> 2.
enum_name(1) -> bob;
enum_name(2) -> fred.

encode_enum_test() ->
  Definition = #field_definition{
    type = enum,
    enum_functions = {protobuf_tests, enum_name, enum_value}
  },
  ?assertEqual(<<0:8>>, encode(0, Definition)),
  ?assertEqual(<<1:8>>, encode(1, Definition)),
  ?assertEqual(<<1:8>>, encode(bob, Definition)),
  ?assertEqual(<<2:8>>, encode(fred, Definition)),
  ?assertError(function_clause, encode(jim, Definition)).


encode_string_test() ->
  Definition = #field_definition{type = string},
  ?assertEqual(<<4:8, "test">>, encode("test", Definition)),
  ?assertEqual(<<4:8, "test">>, encode(<<"test">>, Definition)),
  ?assertEqual(<<3:8, 16#e6, 16#9c, 16#ac>>, encode("本", Definition)).


fuzzy_compare(A, B) ->
  ?assert(abs(A - B) < 0.00001).

encode_and_decode(Test, Msg) ->
  Rec = testmessages:decode_testmessage_pb(Msg),
  Test(Rec),

  Msg2 = testmessages:encode_testmessage_pb(Rec),
  Rec2 = testmessages:decode_testmessage_pb(Msg2),
  Test(Rec2).


decode_simple_message_test() ->
  Test = fun(Rec) ->
    fuzzy_compare(1.2, Rec#testmessage_pb.a_double),
    fuzzy_compare(2.3, Rec#testmessage_pb.a_float),
    ?assertEqual(3, Rec#testmessage_pb.a_int32),
    ?assertEqual(9123456789, Rec#testmessage_pb.a_int64),
    ?assertEqual(123, Rec#testmessage_pb.a_uint32),
    ?assertEqual(456, Rec#testmessage_pb.a_uint64),
    ?assertEqual(42, Rec#testmessage_pb.a_sint32),
    ?assertEqual(84, Rec#testmessage_pb.a_sint64),
    ?assertEqual(123, Rec#testmessage_pb.a_fixed32),
    ?assertEqual(456, Rec#testmessage_pb.a_fixed64),
    ?assertEqual(789, Rec#testmessage_pb.a_sfixed32),
    ?assertEqual(123456, Rec#testmessage_pb.a_sfixed64),
    ?assertEqual(true, Rec#testmessage_pb.a_bool),
    ?assertEqual(<<"hello world">>, Rec#testmessage_pb.a_string),
    ?assertEqual(<<0>>, Rec#testmessage_pb.a_bytes),
    ?assertEqual(undefined, Rec#testmessage_pb.top_level_enum),
    ?assertEqual(undefined, Rec#testmessage_pb.nested_enum),
    ?assertEqual(undefined, Rec#testmessage_pb.nested_message)
  end,

  Msg = <<(16#09333333333333f33f153333134018032095ceb3fe2128):23/integer-unit:8,
          (16#7b30c803385440a8014d7b00000051c801000000000000):23/integer-unit:8,
          (16#5d150300006140e20100000000006801720b68656c6c6f):23/integer-unit:8,
          (16#20776f726c647a0100):9/integer-unit:8>>,
  encode_and_decode(Test, Msg).


decode_negative_message_test() ->
  Test = fun(Rec) ->
    fuzzy_compare(-42.42, Rec#testmessage_pb.a_double),
    fuzzy_compare(-42.42, Rec#testmessage_pb.a_float),
    ?assertEqual(-1234, Rec#testmessage_pb.a_int32),
    ?assertEqual(-1234, Rec#testmessage_pb.a_int64),
    ?assertEqual(-1234, Rec#testmessage_pb.a_sint32),
    ?assertEqual(-1234, Rec#testmessage_pb.a_sint64),
    ?assertEqual(-1234, Rec#testmessage_pb.a_sfixed32),
    ?assertEqual(-1234, Rec#testmessage_pb.a_sfixed64),
    ?assertEqual(false, Rec#testmessage_pb.a_bool)
  end,

  Msg = <<(16#09f6285c8fc23545c01514ae29c218aef6ffffffffffff):23/integer-unit:8,
          (16#ff0120aef6ffffffffffffff0138a31340a3135d2efbff):23/integer-unit:8,
          (16#ff612efbffffffffffff6800):12/integer-unit:8>>,
  encode_and_decode(Test, Msg).


decode_enum_message_test() ->
  Test = fun(Rec) ->
    ?assertEqual(apple, Rec#testmessage_pb.top_level_enum),
    ?assertEqual(plum, Rec#testmessage_pb.nested_enum)
  end,

  Msg = <<(16#800101880102):6/integer-unit:8>>,
  encode_and_decode(Test, Msg).


decode_nested_message_test() ->
  Test = fun(Rec) ->
    Nested1 = Rec#testmessage_pb.nested_message,
    ?assert(is_record(Nested1, testmessage_nestedmessage_pb)),

    Nested2 = Nested1#testmessage_nestedmessage_pb.deeper,
    ?assert(is_record(Nested2, testmessage_nestedmessage_pb)),

    Nested3 = Nested2#testmessage_nestedmessage_pb.deeper,
    ?assertEqual(undefined, Nested3)
  end,

  Msg = <<(16#9201020a00):5/integer-unit:8>>,
  encode_and_decode(Test, Msg).

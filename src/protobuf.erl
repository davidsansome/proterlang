-module(protobuf).
-export([decode_items/2, find_field/3, decode_file/3, enum_name/2, enum_value/2]).
-export([encode_varint/1, encode_key/2, varint_leading_bit/1, decode_varint/1]).
-include("protobuf.hrl").


% Decodes a varint from the Binary and returns a {Integer, Tail} tuple where
% Tail is the remainder of Binary after the varint.
decode_varint(Binary) ->
  decode_varint(Binary, []).

decode_varint(<<1:1, Value:7/bitstring, Tail/binary>>, Acc) ->
  decode_varint(Tail, [Value | Acc]);
decode_varint(<<0:1, Value:7/bitstring, Tail/binary>>, Acc) ->
  CompleteValue = << << X:7 >> || <<X:7>> <- [Value | Acc] >>,
  PaddingLength = length(Acc) + 1,
  Length = PaddingLength + PaddingLength * 7,
  << PaddedValue:Length >> = << 0:PaddingLength, CompleteValue/bitstring >>,
  {PaddedValue, Tail}.


% When encoding a varint, returns the value of the leading bit as a bitstring.
varint_leading_bit(Value) when Value < 128 ->
  << 0:1 >>;
varint_leading_bit(_Value) ->
  << 1:1 >>.


% Encodes an integer Value as a varint.  Returns a binary.
encode_varint(Value) ->
  case encode_varint_1(Value) of
    << >> -> <<0:8>>;
    Ret   -> Ret
  end.

encode_varint_1(Value) when Value == 0 ->
  << >>;
encode_varint_1(Value) ->
  << (varint_leading_bit(Value))/bitstring,
     (Value band 16#7F):7/integer,
     (encode_varint_1(Value bsr 7))/binary >>.


% Decodes the {WireType, FieldNumber} from the field header.
decode_key(Key) ->
  {Key band 16#7, Key bsr 3}.


encode_key(WireType, FieldNumber) ->
  (FieldNumber bsl 3) bor WireType.


% Decodes an encoded item from the binary.  The first argument is the wire type.
decode_item(0, Binary) ->
  decode_varint(Binary);
decode_item(1, Binary) ->
  << Value:64/float-little, Tail/binary >> = Binary,
  {Value, Tail};
decode_item(2, Binary) ->
  {Length, Tail1} = decode_varint(Binary),
  << String:Length/binary, Tail2/binary >> = Tail1,
  {String, Tail2};
decode_item(5, Binary) ->
  << Value:32/float-little, Tail/binary >> = Binary,
  {Value, Tail}.


% Decodes encoded protobuf fields from a binary.  Returns a list of
% {FieldNumber, Value} pairs.
decode_items(Binary, MessageDefinition) ->
  decode_items(Binary, MessageDefinition, []).

decode_items(<< >>, _MessageDefinition, Acc) ->
  Acc;
decode_items(Binary, MessageDefinition, Acc) ->
  % Decode the wire type and field number from the header.
  {Key, Tail1} = decode_varint(Binary),
  {WireType, FieldNumber} = decode_key(Key),

  % Decode the actual item's value.
  {Value1, Tail2} = decode_item(WireType, Tail1),

  % The raw value might need converting to the actual type we're expecting, for
  % example 1 needs to be converted to true for bools, and signed ints need to
  % be interpreted correctly
  Value2 = convert_item_value(Value1, FieldNumber, MessageDefinition),

  % Decode any remaining items from the tail of the binary.
  decode_items(Tail2, MessageDefinition, [{FieldNumber, Value2} | Acc]).


convert_item_value(Value, FieldNumber, MessageDefinition) ->
  % Get the field definition
  Definition = lists:keyfind(FieldNumber, #field_definition.number,
    MessageDefinition#message_definition.fields),

  case Definition of
    false ->
      % We don't know about this field number, just leave the value alone.
      Value;
    #field_definition{type = Type} ->
      convert_item_value(Value, Type)
  end.

convert_item_value(Value, int32) ->
  convert_item_value(Value, int64);
convert_item_value(Value, int64) ->
  <<Ret:64/signed-integer>> = <<Value:64>>,
  Ret;
convert_item_value(Value, uint32) ->
  convert_item_value(Value, uint64);
convert_item_value(Value, uint64) ->
  Value;
convert_item_value(Value, sint32) ->
  convert_item_value(Value, sint64);
convert_item_value(Value, sint64) when Value band 1 == 0 ->
  Value bsr 1;
convert_item_value(Value, sint64) when Value band 1 == 1 ->
  - ((Value + 1) bsr 1);
convert_item_value(0, bool) ->
  false;
convert_item_value(_, bool) ->
  true;
convert_item_value(Value, _) ->
  Value.


% Convenience function that decodes a message from a file.  Module and Function
% are a decode_... function in a generated protobuf module.
decode_file(Filename, Module, Function) ->
  case file:read_file(Filename) of
    {ok, Binary} ->
      Module:Function(Binary);
    {error, _Reason} = Error ->
      Error
  end.


% If the field definition says this value is a nested type, decode the value
% into a record of the nested type.  Otherwise just return the value as is.
decode_nested_type(Value, #field_definition{nested_type = undefined}) ->
  Value;
decode_nested_type(Value, #field_definition{nested_type = {Module, Function}}) ->
  Module:Function(Value).


% Finds a field with the given field number in the list of decoded items.
% If the field wasn't found in the list if items and the definition says it was
% required, raises an exception, otherwise returns the default field value.
find_field(Items, FieldNumber, MessageDefinition) ->
  % Get the field definition.  This will always exist.
  Definition = lists:keyfind(FieldNumber, #field_definition.number,
    MessageDefinition#message_definition.fields),

  % Find the field's value in the message.
  Item = lists:keyfind(FieldNumber, 1, Items),
  case Item of
    false ->
      % The value wasn't found in the message - raise an exception if it was
      % required, otherwise use the default value.
      case Definition#field_definition.label of
        required -> undefined; % TODO: log something?
        _        -> Definition#field_definition.default_value
      end;

    {FieldNumber, Value} ->
      decode_nested_type(Value, Definition)
  end.


% Gets the atom corresponding to an integer enum value.
enum_name(Value, EnumValues) when is_integer(Value) ->
  {Name, Value} = lists:keyfind(Value, 2, EnumValues),
  Name.


% Gets the integer value corresponding to an enum name atom.
enum_value(Name, EnumValues) when is_atom(Name) ->
  {Name, Value} = lists:keyfind(Name, 1, EnumValues),
  Value.


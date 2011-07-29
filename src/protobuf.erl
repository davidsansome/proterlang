-module(protobuf).
-export([decode_items/1, find_field/3, decode_file/3]).
-include("protobuf.hrl").


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


% Decodes the {WireType, FieldNumber} from the field header.
decode_key(Key) ->
  {Key band 16#7, Key bsr 3}.


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
decode_items(Binary) ->
  decode_items(Binary, []).

decode_items(<< >>, Acc) ->
  Acc;
decode_items(Binary, Acc) ->
  % Decode the wire type and field number from the header.
  {Key, Tail1} = decode_varint(Binary),
  {WireType, FieldNumber} = decode_key(Key),

  % Decode the actual item's value.
  {Item, Tail2} = decode_item(WireType, Tail1),

  % Decode any remaining items from the tail of the binary.
  decode_items(Tail2, [{FieldNumber, Item} | Acc]).


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
        required -> error(missing_required_field, [Definition]);
        _        -> Definition#field_definition.default_value
      end;

    {FieldNumber, Value} ->
      decode_nested_type(Value, Definition)
  end.


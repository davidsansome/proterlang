-module(protobuf).
-include("protobuf.hrl").

% Functions to be called externally.
-export([decode_file/3]).

% Varint functions.
-export([decode_varint/1, encode_varint/1]).

% Interface used by generated code.
-export([decode_items/2, find_field/3, encode_field/3, enum_name/2, enum_value/2]).

% Other functions are exported to be used by unit tests.
-export([encode_key/2, decode_key/1, encode_item_value/2, wire_type/1, encode_item/2]).


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
encode_varint(Value) when Value == 0 ->
  <<0:8>>;
encode_varint(Value) when Value > 0 ->
  encode_varint_1(Value).

encode_varint_1(Value) when Value == 0 ->
  << >>;
encode_varint_1(Value) ->
  << (varint_leading_bit(Value))/bitstring,
     (Value band 16#7F):7/integer,
     (encode_varint_1(Value bsr 7))/binary >>.


% Decodes the {WireType, FieldNumber} from the field header.
decode_key(Key) ->
  {Key band 16#7, Key bsr 3}.


% Encodes a WireType and FieldNumber into a Key to go in the field header.
encode_key(WireType, FieldNumber) when WireType >= 0, WireType =< 7 ->
  (FieldNumber bsl 3) bor WireType.


% Converts a type atom to an integer wire type.
wire_type(int32)    -> 0;
wire_type(int64)    -> 0;
wire_type(uint32)   -> 0;
wire_type(uint64)   -> 0;
wire_type(sint32)   -> 0;
wire_type(sint64)   -> 0;
wire_type(bool)     -> 0;
wire_type(enum)     -> 0;
wire_type(fixed64)  -> 1;
wire_type(sfixed64) -> 1;
wire_type(double)   -> 1;
wire_type(string)   -> 2;
wire_type(bytes)    -> 2;
wire_type(message)  -> 2;
wire_type(fixed32)  -> 5;
wire_type(sfixed32) -> 5;
wire_type('float')  -> 5.


% Decodes an encoded item from the binary.  The first argument is the wire type.
% Returns a {Value, Tail} tuple, where Tail is the remainder of the Binary
% after the Value.
decode_item(0, Binary) ->
  decode_varint(Binary);
decode_item(1, Binary) ->
  << Value:64/unsigned-integer-little, Tail/binary >> = Binary,
  {Value, Tail};
decode_item(2, Binary) ->
  {Length, Tail1} = decode_varint(Binary),
  << String:Length/binary, Tail2/binary >> = Tail1,
  {String, Tail2};
decode_item(5, Binary) ->
  << Value:32/unsigned-integer-little, Tail/binary >> = Binary,
  {Value, Tail}.


% Encodes an item to a binary.  The first argument is the wire type.
encode_item(0, Value) ->
  encode_varint(Value);
encode_item(1, Value) ->
  << Value:64/unsigned-integer-little >>;
encode_item(2, Value) when is_list(Value) ->
  encode_item(2, list_to_binary(Value));
encode_item(2, Value) when is_binary(Value) ->
  << (encode_varint(size(Value)))/binary, Value/binary >>;
encode_item(5, Value) ->
  << Value:32/unsigned-integer-little >>.


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
  Value2 = decode_item_value(Value1, FieldNumber, MessageDefinition),

  % Decode any remaining items from the tail of the binary.
  decode_items(Tail2, MessageDefinition, [{FieldNumber, Value2} | Acc]).


% Converts the value from its raw wire type to a more sensible Erlang type.
decode_item_value(Value, FieldNumber, MessageDefinition) ->
  % Get the field definition
  Definition = lists:keyfind(FieldNumber, #field_definition.number,
    MessageDefinition#message_definition.fields),

  case Definition of
    false ->
      % We don't know about this field number, just leave the value alone.
      Value;
    Definition ->
      decode_item_value(Value, Definition)
  end.

decode_item_value(Value, #field_definition{type = int32} = Def) ->
  decode_item_value(Value, Def#field_definition{type = int64});
decode_item_value(Value, #field_definition{type = int64}) ->
  <<Ret:64/signed-integer>> = <<Value:64>>,
  Ret;
decode_item_value(Value, #field_definition{type = uint32} = Def) ->
  decode_item_value(Value, Def#field_definition{type = uint64});
decode_item_value(Value, #field_definition{type = uint64}) ->
  Value;
decode_item_value(Value, #field_definition{type = sint32} = Def) ->
  decode_item_value(Value, Def#field_definition{type = sint64});
decode_item_value(Value, #field_definition{type = sint64}) ->
  zigzag_decode(Value);
decode_item_value(0, #field_definition{type = bool}) ->
  false;
decode_item_value(_, #field_definition{type = bool}) ->
  true;
decode_item_value(Value, #field_definition{type = message,
                                           nested_type = {Module, Fun, _}}) ->
  Module:Fun(Value);
decode_item_value(Value, #field_definition{type = enum,
                                           enum_functions = {Module, V2N, _}}) ->
  Module:V2N(Value);
decode_item_value(Value, #field_definition{type = double}) ->
  <<Ret:64/float>> = <<Value:64>>,
  Ret;
decode_item_value(Value, #field_definition{type = float}) ->
  <<Ret:32/float>> = <<Value:32>>,
  Ret;
decode_item_value(Value, #field_definition{type = sfixed32}) ->
  <<Ret:32/signed-integer>> = <<Value:32>>,
  Ret;
decode_item_value(Value, #field_definition{type = sfixed64}) ->
  <<Ret:64/signed-integer>> = <<Value:64>>,
  Ret;
decode_item_value(Value, _) ->
  Value.


zigzag_decode(Value) when Value band 1 == 0 ->
  Value bsr 1;
zigzag_decode(Value) when Value band 1 == 1 ->
  - ((Value + 1) bsr 1).

zigzag_encode(Value) when Value >= 0 ->
  Value bsl 1;
zigzag_encode(Value) when Value < 0 ->
  ((- Value) bsl 1) - 1.


encode_field(Value, FieldNumber, MessageDefinition) ->
  % Get the field definition.  This will always exist.
  Definition = lists:keyfind(FieldNumber, #field_definition.number,
    MessageDefinition#message_definition.fields),

  DefaultValue = Definition#field_definition.default_value,

  case Value of
    undefined ->
      % If this field was required then raise an error.
      case Definition#field_definition.label of
        required ->
          erlang:error(missing_required_field, [
            MessageDefinition#message_definition.name,
            FieldNumber,
            Definition#field_definition.name
          ]);
        _ -> << >>
      end;

    DefaultValue ->
      % The value is the same as the default value for this field, so don't
      % bother encoding anything.
      << >>;

    _ ->
      Type = Definition#field_definition.type,
      WireType = wire_type(Type),
      Key = encode_key(WireType, FieldNumber),
      EncodedValue = encode_item(WireType, encode_item_value(Value, Definition)),

      << (encode_varint(Key))/binary, EncodedValue/binary >>
  end.


encode_item_value(Value, #field_definition{type = int32} = Def) ->
  encode_item_value(Value, Def#field_definition{type = int64});
encode_item_value(Value, #field_definition{type = int64}) ->
  <<Value2:64/integer-unsigned>> = <<Value:64/integer-unsigned>>,
  Value2;
encode_item_value(Value, #field_definition{type = uint32} = Def) ->
  encode_item_value(Value, Def#field_definition{type = uint64});
encode_item_value(Value, #field_definition{type = uint64}) when Value >= 0 ->
  Value;
encode_item_value(Value, #field_definition{type = sint32} = Def) ->
  encode_item_value(Value, Def#field_definition{type = sint64});
encode_item_value(Value, #field_definition{type = sint64}) ->
  zigzag_encode(Value);
encode_item_value(0, #field_definition{type = bool}) ->
  0;
encode_item_value(false, #field_definition{type = bool}) ->
  0;
encode_item_value(_, #field_definition{type = bool}) ->
  1;
encode_item_value(Value, #field_definition{type = enum}) when is_integer(Value) ->
  Value;
encode_item_value(Name, #field_definition{
    type = enum, enum_functions = {Module, _, N2V}}) when is_atom(Name) ->
  Module:N2V(Name);
encode_item_value(Record, #field_definition{
    type = message, nested_type = {Module, _, Fun}}) when is_tuple(Record) ->
  Module:Fun(Record);
encode_item_value(Value, #field_definition{type = float}) ->
  <<Ret:32/unsigned-integer-big>> = <<Value:32/float>>,
  Ret;
encode_item_value(Value, #field_definition{type = double}) ->
  <<Ret:64/unsigned-integer-big>> = <<Value:64/float>>,
  Ret;
encode_item_value(Value, _) ->
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


% Finds a field with the given field number in the list of decoded items.
find_field(Items, FieldNumber, MessageDefinition) ->
  % Get the field definition.  This will always exist.
  Definition = lists:keyfind(FieldNumber, #field_definition.number,
    MessageDefinition#message_definition.fields),

  % Find the field's value in the message.
  Item = lists:keyfind(FieldNumber, 1, Items),
  case Item of
    false ->
      Definition#field_definition.default_value;

    {FieldNumber, Value} ->
      Value
  end.


% Gets the atom corresponding to an integer enum value, or 'unknown' if the
% value wasn't valid.
enum_name(Value, EnumValues) when is_integer(Value) ->
  case lists:keyfind(Value, 2, EnumValues) of
    {Name, Value} -> Name;
    false         -> unknown
  end.


% Gets the integer value corresponding to an enum name atom, or 0 if the name
% wasn't valid.
enum_value(Name, EnumValues) when is_atom(Name) ->
  case lists:keyfind(Name, 1, EnumValues) of
    {Name, Value} -> Value;
    false         -> 0
  end.


% Metadata about a message.  One of these is generated for each Message type.
-record(message_definition, {
  name,
  fields = []
}).

% Metadata about a single field in a message.
-record(field_definition, {
  name,
  type,
  number,
  label = required,
  default_value = undefined,
  nested_type = undefined,
  enum_functions = undefined
}).

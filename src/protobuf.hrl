-record(message_definition, {
  name,
  fields = []
}).

-record(field_definition, {
  name,
  number,
  label = required,
  default_value = undefined,
  nested_type = undefined
}).

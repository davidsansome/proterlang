import testmessages_pb2 as pb

def toerlang(pb):
  data = pb.SerializeToString()

  bits = []
  for i in xrange(0, len(data)-1, 23):
    chunk = data[i:i+23]
    bits.append("(16#%s):%d/integer-unit:8" % (
      "".join(["%02x" % ord(x) for x in chunk]),
      len(chunk)))

  print "<<" + ",\n  ".join(bits) + ">>"

m = pb.TestMessage()
m.a_double = 1.2
m.a_float = 2.3
m.a_int32 = 3
m.a_int64 = 9123456789
m.a_uint32 = 123
m.a_uint64 = 456
m.a_sint32 = 42
m.a_sint64 = 84
m.a_fixed32 = 123
m.a_fixed64 = 456
m.a_sfixed32 = 789
m.a_sfixed64 = 123456
m.a_bool = True
m.a_string = "hello world"
m.a_bytes = "\00"
toerlang(m)

m = pb.TestMessage()
m.a_double = -42.42
m.a_float = -42.42
m.a_int32 = -1234
m.a_int64 = -1234
m.a_sint32 = -1234
m.a_sint64 = -1234
m.a_sfixed32 = -1234
m.a_sfixed64 = -1234
m.a_bool = False
toerlang(m)


m = pb.TestMessage()
m.top_level_enum = pb.apple
m.nested_enum = pb.TestMessage.plum
toerlang(m)


m = pb.TestMessage()
m.nested_message.deeper.ClearField("deeper")
toerlang(m)


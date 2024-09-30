use aiken/primitive/bytearray

pub type ParamConstr {
  intA: Int,
  intB: Int,
  msg: ByteArray,
}

validator baz(
  isTrue: Bool,
  ParamConstr { intA, intB, msg }: ParamConstr,
  primitiveInt: Int,
  primitiveBA: ByteArray,
) {
  else(_ctx: Data) {
    and {
      isTrue,
      intA + intB + bytearray.length(msg) + primitiveInt + bytearray.length(
        primitiveBA,
      ) == 60,
    }
  }
}

use aiken/primitive/bytearray
use cardano/transaction.{OutputReference, Transaction}

pub type ParamConstr {
  intP: Int,
  msgP: ByteArray,
}

pub type MyDatum {
  DatumA { intA: Int, msgA: ByteArray }
  DatumB { intB: Int, msgB: ByteArray }
}

pub type MyRedeemer {
  intR: Int,
  msgR: ByteArray,
}

validator baz(
  isTrue: Bool,
  ParamConstr { intP, msgP }: ParamConstr,
  primitiveInt: Int,
  primitiveBA: ByteArray,
) {
  spend(
    datumOpt: Option<MyDatum>,
    redeemer: MyRedeemer,
    _utxo: OutputReference,
    _self: Transaction,
  ) {
    expect Some(datum) = datumOpt
    let datResult =
      when datum is {
        DatumA { intA, msgA } -> intA + bytearray.length(msgA)
        DatumB { intB, msgB } -> intB + bytearray.length(msgB)
      }
    and {
      isTrue,
      datResult + redeemer.intR + bytearray.length(redeemer.msgR) + intP + bytearray.length(
        msgP,
      ) + primitiveInt + bytearray.length(primitiveBA) == 60,
    }
  }

  else(_) {
    fail
  }
}

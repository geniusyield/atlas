## 0.8.0

* Note that there has been large internal refactoring inside this update, but these are mostly non breaking as we provide pattern & type synonyms to keep earlier behavior. In particular:
  * We define a new type `GYKeyHash kr` and all other key hashes such as `GYPaymentKeyHash` are type synonyms around it.
  * We define a new type `GYCredential kr` and all other credentials such as `GYPaymentCredential` are type synonyms around it. 
  * Likewise types `GYSigningKey kr`, `GYVerificationKey kr`, `GYExtendedSigningKey kr` are newly introduced and previous relevant key types like `GYPaymentSigningKey` are simply type synonyms.
* Constructor of `GYPubKeyHash` is no longer exported.
* `readSomeSigningKey` is removed.
* Adds additional certificates such as those related to governance, drep participation, etc.
* Tracks node version 10.1.3 and corresponding updated CLB version.
* Update default value of `GYAwaitTxParameters` to now have 100 max attempts.

## 0.7.0

* Era histories are now cached through entire run of the program whereas protocol parameters are fetched once per epoch. In case you were utilising era summary given by Atlas, note that era end of last era is now set to being unbounded.
* Bug fix for our caching mechanism, see PR [#370](https://github.com/geniusyield/atlas/pull/370) for more details.
* We no longer fetch registered stake pools as it is not required.
* Added utility functions to do slot to epoch related conversations.
* `addRefScript` now accepts for scripts that has version greater than or equal to `PlutusV2`.

## 0.6.3

* Avoid dependency upon `cardano-balance-tx:internal`. See [#368](https://github.com/geniusyield/atlas/issues/368) for more details.

## 0.6.2

* Give `Eq`, `Semigroup`, `Monoid` instance for `GYTxWitness`.
* Adds `valueAlter` utility function inside `GeniusYield.Types.Value` module.
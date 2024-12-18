## 0.7.0

* Era histories are now cached through entire run of the program (and era end of last era is set to unbounded) whereas protocol parameters are fetched once per epoch.
* Bug fix for our caching mechanism, see PR [#370](https://github.com/geniusyield/atlas/pull/370) for more details.
* We no longer fetch registered stake pools as it is not required.
* Added utility functions to do slot to epoch related conversations.
* `addRefScript` now accepts for scripts that has version greater than or equal to `PlutusV2`.

## 0.6.3

* Avoid dependency upon `cardano-balance-tx:internal`. See [#368](https://github.com/geniusyield/atlas/issues/368) for more details.

## 0.6.2

* Give `Eq`, `Semigroup`, `Monoid` instance for `GYTxWitness`.
* Adds `valueAlter` utility function inside `GeniusYield.Types.Value` module.
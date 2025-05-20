## Next

* Added `utxosWithAsset` to efficiently querying for UTxOs containing specified asset.

## 0.14.0

* Function to obtain `TxBodyContent BuildTx ApiEra` from `GYTxBody`, called `obtainTxBodyContentBuildTx` (also see `obtainTxBodyContentBuildTx'`). This is useful to build upon previously built transactions.
* Add support for treasury donation. See "Donation" test in `GeniusYield.Test.Privnet.Examples.Misc` module.
* Added extra build configuration (within `GYTxExtraConfiguration`) to provide for fee UTxO. If such a UTxO is provided then it's used to cover for transaction fees and we won't be exercising our usual logarithmic fee over approximation algorithm (see [specification](https://github.com/geniusyield/atlas/blob/main/src/GeniusYield/Transaction/CoinSelection/Specification.md) for details about this algorithm -- mainly the first few paras) since purpose of that algorithm is to select sufficient ADA from user's wallet to satisfy for fees but here we have separate fee UTxO which is expected to provide for it. `GYBuildTxFeeUtxoAdaInsufficient` error is returned if it proves to be insufficient (either fee is higher than ADA available in this UTxO or that subsequent change output lacks sufficient ADA to cover minimum ADA requirements of an output).

## 0.13.0

* Support of GHC 9.10 & 9.12.
* Updated to latest IOG libraries, in particular `cardano-api` version >= 10.13 & `cardano-node` version >= 10.3.
* Usage of reference scripts no longer prohibit PlutusV1 scripts which can now also be given via reference.
* On-chain code is now split into a separate package so that Atlas no longer depends upon `plutus-tx-plugin` which is constrained to a very specific GHC version. User's of Atlas, should have following `subdir` stanza when specifying Atlas dependency in their `cabal.project` file:
  ```
  subdir:
    plutus/atlas-onchain-common
    .
  ```
* Support of advanced transaction building configuration. Currently Atlas assumes that extra wallet inputs selected by coin selection algorithm are coming from key based wallet but these inputs could also come from script based wallet instead. Type `GYTxExtraConfiguration` has field `gytxecUtxoInputMapper` to allow for this configuration. Other fields included in `GYTxExtraConfiguration` provides advanced configuration, please see corresponding haddock for more details. Transaction building functions such as `buildTxBodyWithExtraConfiguration` allow passing this extra configuration.
* Support of extended verification keys. See `GYExtendedVerificationKey` type and related utilities such as `getExtendedVerificationKey`.
* Added `applyParam` to apply data encoded script parameter to a plutus script.
* Added `readScript'`, which is a variant of `readScript` where file contents are already loaded.

## 0.12.0

* `valueAdjust` now omits for entries if result of adjustment is zero.
* Port coin selection algorithm from [`cardano-wallet`](https://github.com/cardano-foundation/cardano-wallet) to Atlas. This is done in sync with our 3rd Milestone and allows us to support latest versions of node & other IOG tooling.
* Update to latest IOG dependencies.
* Support of monitoring mempool transactions (`mempoolTxs` query function).
* Support of government related queries such as `constitution` (to query the current constitution definition) and `proposals` (to fetch for proposals that are considered for ratification)
* Support of mempool based caching, enabled by setting `mempoolCache` in provider's configuration.
* Support of local transaction submission based caching (i.e., submitted transactions are considered to know for to be made available outputs & spent outputs), enabled by setting `localTxSubmissionCache` in provider's configuration.

## 0.11.1

* Adds support of Ogmios-Kupo provider, see section on providers at https://atlas-app.io/getting-started/endpoints.
* `ToJSON` instance for `GYTxOutRefCbor`.
* New `GeniusYield.Debug` module to perform Atlas's operation from repl.

## 0.11.0

* Allows reference scripts to be of version greater than the minimum supported constrained version of `GYTxSkeleton`. Thanks [@SeungheonOh](https://github.com/SeungheonOh) for finding [this bug](https://github.com/geniusyield/atlas/issues/404)! Please visit the linked issue for more details.

## 0.10.0

* Support of extended keys in `runGYTxMonadIO`.
* Don't throw error for querying DRep state in case of Maestro/BF provider.

## 0.9.0

* When spending an input, datum is now optional since it is not required to be specified in case it's inlined or not needed by associated Plutus script.

## 0.8.1

* Add combined stake address registration and delegation certificate.

## 0.8.0

* Note that there has been large internal refactoring inside this update, but these are mostly non breaking as we provide pattern & type synonyms to keep earlier behavior. In particular:
  * We define a new type `GYKeyHash kr` and all other key hashes such as `GYPaymentKeyHash`, `GYStakePoolId` are type synonyms around it.
  * We define a new type `GYCredential kr` and all other credentials such as `GYPaymentCredential` are type synonyms around it. 
  * Likewise types `GYSigningKey kr`, `GYVerificationKey kr`, `GYExtendedSigningKey kr` are newly introduced and previous relevant key types like `GYPaymentSigningKey` are simply type synonyms.
* Constructor of `GYPubKeyHash` is no longer exported.
* `readSomeSigningKey` is removed.
* Adds additional certificates such as those related to governance, drep participation, stake pool registration, etc.
* Tracks node version 10.1.3 and corresponding updated CLB version.
* Update default value of `GYAwaitTxParameters` to now have 100 max attempts.
* `GYInScript` and `GYStakeValScript` are now defined as a type synonyms around `GYBuildPlutusScript` whereas `GYMintScript` is a type synonym around `GYBuildScript` which now also includes simple scripts (besides plutus scripts). Pattern synonyms are provided for backwards compatibility. These and related functions such as `stakeValidatorVersionFromWitness`, `gyStakeValScriptToSerialisedScript` are now exported from `GeniusYield.Types.BuildScript` instead of `GeniusYield.Types.Script`.
* `GYTxWdrlWitness`, `GYTxCertWitness` are now defined as a type synonyms around generic `GYTxBuildWitness` and now also includes simple scripts. Pattern synonyms are provided to maintain backwards compatibility.
* Adds support for governance actions, namely proposal procedures & voting procedures.

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
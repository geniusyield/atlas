{-|
Module      : GeniusYield.Types.Tx
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Types.Tx
    ( -- * Docspec setup
      -- $setup

      -- * Transactions
      GYTx
    , txFromApi
    , txToApi
    , txFromHex
    , txFromHexBS
    , txFromCBOR
    , txToHex
    , txToHexBS
    , txToCBOR
    , writeTx
      -- * Transaction Id's
    , GYTxId
    , txIdFromHex
    , txIdFromHexE
    , txIdToApi
    , txIdFromApi
    , txIdFromPlutus
      -- * Transaction Witness Set
    , GYTxWitness
    , txWitFromHexBS
    , txWitFromHex
    , txWitFromLedger
    , txWitToLedger
    , txWitToKeyWitnessApi
) where

import qualified Cardano.Api                        as Api
import qualified Cardano.Api.Shelley                as Api.S
import           Cardano.Ledger.Alonzo.TxWits       (AlonzoTxWits,
                                                     addrAlonzoTxWitsL)
import           Cardano.Ledger.Babbage             (Babbage)
import qualified Cardano.Ledger.Babbage             as Babbage (BabbageEra)
import qualified Cardano.Ledger.Binary              as CBOR
import qualified Cardano.Ledger.Crypto              as Crypto
import           Control.Lens                       (view, (?~))
import qualified Data.Aeson.Types                   as Aeson
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Base16             as BS16
import qualified Data.ByteString.Char8              as BS8
import qualified Data.ByteString.Lazy               as LBS
import qualified Data.Set                           as Set
import qualified Data.Swagger                       as Swagger
import qualified Data.Swagger.Internal.Schema       as Swagger
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as TE
import qualified Database.PostgreSQL.Simple         as PQ
import qualified Database.PostgreSQL.Simple.ToField as PQ
import qualified PlutusLedgerApi.V1                 as Plutus (TxId (..))
import qualified PlutusTx.Builtins.Internal         as Plutus
import qualified Text.Printf                        as Printf
import qualified Web.HttpApiData                    as Web

import           Cardano.Ledger.Core                (eraProtVerHigh)
import           GeniusYield.Imports

-- $setup
--
-- >>> :set -XOverloadedStrings -XTypeApplications
-- >>> import qualified Cardano.Api                as Api
-- >>> import qualified Data.Aeson                 as Aeson
-- >>> import qualified Data.Text                  as Text
-- >>> import qualified Data.Text.Encoding         as TE
-- >>> import qualified Data.ByteString            as BS
-- >>> import           Data.Maybe                 (fromMaybe)
-- >>> import qualified Text.Printf                as Printf
-- >>>
-- >>>
-- >>> let gyTxId = "6c751d3e198c5608dfafdfdffe16aeac8a28f88f3a769cf22dd45e8bc84f47e8" :: GYTxId
-- >>> let txHexBS = "84a70082825820975e4c7f8d7937f8102e500714feb3f014c8766fcf287a11c10c686154fcb27501825820c887cba672004607a0f60ab28091d5c24860dbefb92b1a8776272d752846574f000d818258207a67cd033169e330c9ae9b8d0ef8b71de9eb74bbc8f3f6be90446dab7d1e8bfd00018282583900fd040c7a10744b79e5c80ec912a05dbdb3009e372b7f4b0f026d16b0c663651ffc046068455d2994564ba9d4b3e9b458ad8ab5232aebbf401a1abac7d882583900fd040c7a10744b79e5c80ec912a05dbdb3009e372b7f4b0f026d16b0c663651ffc046068455d2994564ba9d4b3e9b458ad8ab5232aebbf40821a0017ad4aa2581ca6bb5fd825455e7c69bdaa9d3a6dda9bcbe9b570bc79bd55fa50889ba1466e69636b656c1911d7581cb17cb47f51d6744ad05fb937a762848ad61674f8aebbaec67be0bb6fa14853696c6c69636f6e190258021a00072f3c0e8009a1581cb17cb47f51d6744ad05fb937a762848ad61674f8aebbaec67be0bb6fa14853696c6c69636f6e1902580b5820291b4e4c5f189cb896674e02e354028915b11889687c53d9cf4c1c710ff5e4aea203815908d45908d101000033332332232332232323232323232323232323232323232323232222223232323235500222222222225335333553024120013232123300122333500522002002001002350012200112330012253350021001102d02c25335325335333573466e3cd400488008d404c880080b40b04ccd5cd19b873500122001350132200102d02c102c3500122002102b102c00a132635335738921115554784f206e6f7420636f6e73756d65640002302115335333573466e3c048d5402c880080ac0a854cd4ccd5cd19b8701335500b2200102b02a10231326353357389210c77726f6e6720616d6f756e740002302113263533573892010b77726f6e6720746f6b656e00023021135500122222222225335330245027007162213500222253350041335502d00200122161353333573466e1cd55cea8012400046644246600200600464646464646464646464646666ae68cdc39aab9d500a480008cccccccccc888888888848cccccccccc00402c02802402001c01801401000c008cd40548c8c8cccd5cd19b8735573aa0049000119910919800801801180f1aba15002301a357426ae8940088c98d4cd5ce01381401301289aab9e5001137540026ae854028cd4054058d5d0a804999aa80c3ae501735742a010666aa030eb9405cd5d0a80399a80a80f1aba15006335015335502101f75a6ae854014c8c8c8cccd5cd19b8735573aa00490001199109198008018011919191999ab9a3370e6aae754009200023322123300100300233502475a6ae854008c094d5d09aba2500223263533573805605805405226aae7940044dd50009aba150023232323333573466e1cd55cea8012400046644246600200600466a048eb4d5d0a80118129aba135744a004464c6a66ae700ac0b00a80a44d55cf280089baa001357426ae8940088c98d4cd5ce01381401301289aab9e5001137540026ae854010cd4055d71aba15003335015335502175c40026ae854008c06cd5d09aba2500223263533573804604804404226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226aae7940044dd50009aba150023232323333573466e1d400520062321222230040053016357426aae79400c8cccd5cd19b875002480108c848888c008014c060d5d09aab9e500423333573466e1d400d20022321222230010053014357426aae7940148cccd5cd19b875004480008c848888c00c014dd71aba135573ca00c464c6a66ae7007807c07407006c0680644d55cea80089baa001357426ae8940088c98d4cd5ce00b80c00b00a9100109aab9e5001137540022464460046eb0004c8004d5406488cccd55cf8009280c119a80b98021aba100230033574400402446464646666ae68cdc39aab9d5003480008ccc88848ccc00401000c008c8c8c8cccd5cd19b8735573aa004900011991091980080180118099aba1500233500c012357426ae8940088c98d4cd5ce00b00b80a80a09aab9e5001137540026ae85400cccd5401dd728031aba1500233500875c6ae84d5d1280111931a99ab9c012013011010135744a00226aae7940044dd5000899aa800bae75a224464460046eac004c8004d5405c88c8cccd55cf8011280b919a80b19aa80c18031aab9d5002300535573ca00460086ae8800c0444d5d080089119191999ab9a3370ea0029000119091180100198029aba135573ca00646666ae68cdc3a801240044244002464c6a66ae7004004403c0380344d55cea80089baa001232323333573466e1cd55cea80124000466442466002006004600a6ae854008dd69aba135744a004464c6a66ae7003403803002c4d55cf280089baa0012323333573466e1cd55cea800a400046eb8d5d09aab9e500223263533573801601801401226ea8004488c8c8cccd5cd19b87500148010848880048cccd5cd19b875002480088c84888c00c010c018d5d09aab9e500423333573466e1d400d20002122200223263533573801c01e01a01801601426aae7540044dd50009191999ab9a3370ea0029001100911999ab9a3370ea0049000100911931a99ab9c00a00b009008007135573a6ea80048c8c8c8c8c8cccd5cd19b8750014803084888888800c8cccd5cd19b875002480288488888880108cccd5cd19b875003480208cc8848888888cc004024020dd71aba15005375a6ae84d5d1280291999ab9a3370ea00890031199109111111198010048041bae35742a00e6eb8d5d09aba2500723333573466e1d40152004233221222222233006009008300c35742a0126eb8d5d09aba2500923333573466e1d40192002232122222223007008300d357426aae79402c8cccd5cd19b875007480008c848888888c014020c038d5d09aab9e500c23263533573802402602202001e01c01a01801601426aae7540104d55cf280189aab9e5002135573ca00226ea80048c8c8c8c8cccd5cd19b875001480088ccc888488ccc00401401000cdd69aba15004375a6ae85400cdd69aba135744a00646666ae68cdc3a80124000464244600400660106ae84d55cf280311931a99ab9c00b00c00a009008135573aa00626ae8940044d55cf280089baa001232323333573466e1d400520022321223001003375c6ae84d55cf280191999ab9a3370ea004900011909118010019bae357426aae7940108c98d4cd5ce00400480380300289aab9d5001137540022244464646666ae68cdc39aab9d5002480008cd5403cc018d5d0a80118029aba135744a004464c6a66ae7002002401c0184d55cf280089baa00149924103505431001200132001355008221122253350011350032200122133350052200230040023335530071200100500400132001355007222533500110022213500222330073330080020060010033200135500622225335001100222135002225335333573466e1c005200000d00c13330080070060031333008007335009123330010080030020060031122002122122330010040031122123300100300212200212200111232300100122330033002002001482c0252210853696c6c69636f6e003351223300248920975e4c7f8d7937f8102e500714feb3f014c8766fcf287a11c10c686154fcb27500480088848cc00400c00880050581840100d87980821a001f372a1a358a2b14f5f6" :: BS.ByteString
-- >>> let tx = fromMaybe (error "Not able to convert hex string to GYTx") (txFromHex $ Text.unpack $ TE.decodeUtf8 txHexBS)
--

newtype GYTx = GYTx (Api.Tx Api.BabbageEra)

-- |
--
-- >>> txToApi <$> (Aeson.fromJSON @GYTx $ Aeson.toJSON tx)
-- Success (ShelleyTx ShelleyBasedEraBabbage (AlonzoTx {body = TxBodyConstr BabbageTxBodyRaw {btbrSpendInputs = fromList [TxIn (TxId {unTxId = SafeHash "975e4c7f8d7937f8102e500714feb3f014c8766fcf287a11c10c686154fcb275"}) (TxIx 1),TxIn (TxId {unTxId = SafeHash "c887cba672004607a0f60ab28091d5c24860dbefb92b1a8776272d752846574f"}) (TxIx 0)], btbrCollateralInputs = fromList [TxIn (TxId {unTxId = SafeHash "7a67cd033169e330c9ae9b8d0ef8b71de9eb74bbc8f3f6be90446dab7d1e8bfd"}) (TxIx 0)], btbrReferenceInputs = fromList [], btbrOutputs = StrictSeq {fromStrict = fromList [Sized {sizedValue = (Addr Testnet (KeyHashObj (KeyHash "fd040c7a10744b79e5c80ec912a05dbdb3009e372b7f4b0f026d16b0")) (StakeRefBase (KeyHashObj (KeyHash "c663651ffc046068455d2994564ba9d4b3e9b458ad8ab5232aebbf40"))),MaryValue 448448472 (MultiAsset (fromList [])),NoDatum,SNothing), sizedSize = 65},Sized {sizedValue = (Addr Testnet (KeyHashObj (KeyHash "fd040c7a10744b79e5c80ec912a05dbdb3009e372b7f4b0f026d16b0")) (StakeRefBase (KeyHashObj (KeyHash "c663651ffc046068455d2994564ba9d4b3e9b458ad8ab5232aebbf40"))),MaryValue 1551690 (MultiAsset (fromList [(PolicyID {policyID = ScriptHash "a6bb5fd825455e7c69bdaa9d3a6dda9bcbe9b570bc79bd55fa50889b"},fromList [("6e69636b656c",4567)]),(PolicyID {policyID = ScriptHash "b17cb47f51d6744ad05fb937a762848ad61674f8aebbaec67be0bb6f"},fromList [("53696c6c69636f6e",600)])])),NoDatum,SNothing), sizedSize = 151}]}, btbrCollateralReturn = SNothing, btbrTotalCollateral = SNothing, btbrCerts = StrictSeq {fromStrict = fromList []}, btbrWithdrawals = Withdrawals {unWithdrawals = fromList []}, btbrTxFee = Coin 470844, btbrValidityInterval = ValidityInterval {invalidBefore = SNothing, invalidHereafter = SNothing}, btbrUpdate = SNothing, btbrReqSignerHashes = fromList [], btbrMint = MultiAsset (fromList [(PolicyID {policyID = ScriptHash "b17cb47f51d6744ad05fb937a762848ad61674f8aebbaec67be0bb6f"},fromList [("53696c6c69636f6e",600)])]), btbrScriptIntegrityHash = SJust (SafeHash "291b4e4c5f189cb896674e02e354028915b11889687c53d9cf4c1c710ff5e4ae"), btbrAuxDataHash = SNothing, btbrTxNetworkId = SNothing} (blake2b_256: SafeHash "a5e1d764a1bb1e8fab4bb5b8529410bf12517937dac87cbbfec7d59044d16e39"), wits = AlonzoTxWitsRaw {atwrAddrTxWits = fromList [], atwrBootAddrTxWits = fromList [], atwrScriptTxWits = fromList [(ScriptHash "b17cb47f51d6744ad05fb937a762848ad61674f8aebbaec67be0bb6f",PlutusScript PlutusV1 ScriptHash "b17cb47f51d6744ad05fb937a762848ad61674f8aebbaec67be0bb6f")], atwrDatsTxWits = TxDatsConstr TxDatsRaw (fromList []) (blake2b_256: SafeHash "45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0"), atwrRdmrsTxWits = RedeemersConstr RedeemersRaw (fromList [(RdmrPtr Mint 0,(DataConstr Constr 0 [] (blake2b_256: SafeHash "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"),WrapExUnits {unWrapExUnits = ExUnits' {exUnitsMem' = 2045738, exUnitsSteps' = 898247444}}))]) (blake2b_256: SafeHash "3a384e30b63601e50ccbdfc7fe5d1364e52ecf8f0c03a0f6eff44fe42fe65557")} (blake2b_256: SafeHash "9085fea61a5bc7baa0abb2e841264b04987017bc2f61183ad4de77ff6f96fb7c"), isValid = IsValid True, auxiliaryData = SNothing}))
--
instance Aeson.FromJSON GYTx where
    parseJSON = Aeson.withText "GYTx" $ \t -> do
        case txFromHexBS $ TE.encodeUtf8 t of
            Left err -> fail $ "Not a GYTx: " ++ err
            Right tx -> return tx

-- |
--
-- >>> Aeson.toJSON tx
-- String "84a70082825820975e4c7f8d7937f8102e500714feb3f014c8766fcf287a11c10c686154fcb27501825820c887cba672004607a0f60ab28091d5c24860dbefb92b1a8776272d752846574f000d818258207a67cd033169e330c9ae9b8d0ef8b71de9eb74bbc8f3f6be90446dab7d1e8bfd00018282583900fd040c7a10744b79e5c80ec912a05dbdb3009e372b7f4b0f026d16b0c663651ffc046068455d2994564ba9d4b3e9b458ad8ab5232aebbf401a1abac7d882583900fd040c7a10744b79e5c80ec912a05dbdb3009e372b7f4b0f026d16b0c663651ffc046068455d2994564ba9d4b3e9b458ad8ab5232aebbf40821a0017ad4aa2581ca6bb5fd825455e7c69bdaa9d3a6dda9bcbe9b570bc79bd55fa50889ba1466e69636b656c1911d7581cb17cb47f51d6744ad05fb937a762848ad61674f8aebbaec67be0bb6fa14853696c6c69636f6e190258021a00072f3c0e8009a1581cb17cb47f51d6744ad05fb937a762848ad61674f8aebbaec67be0bb6fa14853696c6c69636f6e1902580b5820291b4e4c5f189cb896674e02e354028915b11889687c53d9cf4c1c710ff5e4aea203815908d45908d101000033332332232332232323232323232323232323232323232323232222223232323235500222222222225335333553024120013232123300122333500522002002001002350012200112330012253350021001102d02c25335325335333573466e3cd400488008d404c880080b40b04ccd5cd19b873500122001350132200102d02c102c3500122002102b102c00a132635335738921115554784f206e6f7420636f6e73756d65640002302115335333573466e3c048d5402c880080ac0a854cd4ccd5cd19b8701335500b2200102b02a10231326353357389210c77726f6e6720616d6f756e740002302113263533573892010b77726f6e6720746f6b656e00023021135500122222222225335330245027007162213500222253350041335502d00200122161353333573466e1cd55cea8012400046644246600200600464646464646464646464646666ae68cdc39aab9d500a480008cccccccccc888888888848cccccccccc00402c02802402001c01801401000c008cd40548c8c8cccd5cd19b8735573aa0049000119910919800801801180f1aba15002301a357426ae8940088c98d4cd5ce01381401301289aab9e5001137540026ae854028cd4054058d5d0a804999aa80c3ae501735742a010666aa030eb9405cd5d0a80399a80a80f1aba15006335015335502101f75a6ae854014c8c8c8cccd5cd19b8735573aa00490001199109198008018011919191999ab9a3370e6aae754009200023322123300100300233502475a6ae854008c094d5d09aba2500223263533573805605805405226aae7940044dd50009aba150023232323333573466e1cd55cea8012400046644246600200600466a048eb4d5d0a80118129aba135744a004464c6a66ae700ac0b00a80a44d55cf280089baa001357426ae8940088c98d4cd5ce01381401301289aab9e5001137540026ae854010cd4055d71aba15003335015335502175c40026ae854008c06cd5d09aba2500223263533573804604804404226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226aae7940044dd50009aba150023232323333573466e1d400520062321222230040053016357426aae79400c8cccd5cd19b875002480108c848888c008014c060d5d09aab9e500423333573466e1d400d20022321222230010053014357426aae7940148cccd5cd19b875004480008c848888c00c014dd71aba135573ca00c464c6a66ae7007807c07407006c0680644d55cea80089baa001357426ae8940088c98d4cd5ce00b80c00b00a9100109aab9e5001137540022464460046eb0004c8004d5406488cccd55cf8009280c119a80b98021aba100230033574400402446464646666ae68cdc39aab9d5003480008ccc88848ccc00401000c008c8c8c8cccd5cd19b8735573aa004900011991091980080180118099aba1500233500c012357426ae8940088c98d4cd5ce00b00b80a80a09aab9e5001137540026ae85400cccd5401dd728031aba1500233500875c6ae84d5d1280111931a99ab9c012013011010135744a00226aae7940044dd5000899aa800bae75a224464460046eac004c8004d5405c88c8cccd55cf8011280b919a80b19aa80c18031aab9d5002300535573ca00460086ae8800c0444d5d080089119191999ab9a3370ea0029000119091180100198029aba135573ca00646666ae68cdc3a801240044244002464c6a66ae7004004403c0380344d55cea80089baa001232323333573466e1cd55cea80124000466442466002006004600a6ae854008dd69aba135744a004464c6a66ae7003403803002c4d55cf280089baa0012323333573466e1cd55cea800a400046eb8d5d09aab9e500223263533573801601801401226ea8004488c8c8cccd5cd19b87500148010848880048cccd5cd19b875002480088c84888c00c010c018d5d09aab9e500423333573466e1d400d20002122200223263533573801c01e01a01801601426aae7540044dd50009191999ab9a3370ea0029001100911999ab9a3370ea0049000100911931a99ab9c00a00b009008007135573a6ea80048c8c8c8c8c8cccd5cd19b8750014803084888888800c8cccd5cd19b875002480288488888880108cccd5cd19b875003480208cc8848888888cc004024020dd71aba15005375a6ae84d5d1280291999ab9a3370ea00890031199109111111198010048041bae35742a00e6eb8d5d09aba2500723333573466e1d40152004233221222222233006009008300c35742a0126eb8d5d09aba2500923333573466e1d40192002232122222223007008300d357426aae79402c8cccd5cd19b875007480008c848888888c014020c038d5d09aab9e500c23263533573802402602202001e01c01a01801601426aae7540104d55cf280189aab9e5002135573ca00226ea80048c8c8c8c8cccd5cd19b875001480088ccc888488ccc00401401000cdd69aba15004375a6ae85400cdd69aba135744a00646666ae68cdc3a80124000464244600400660106ae84d55cf280311931a99ab9c00b00c00a009008135573aa00626ae8940044d55cf280089baa001232323333573466e1d400520022321223001003375c6ae84d55cf280191999ab9a3370ea004900011909118010019bae357426aae7940108c98d4cd5ce00400480380300289aab9d5001137540022244464646666ae68cdc39aab9d5002480008cd5403cc018d5d0a80118029aba135744a004464c6a66ae7002002401c0184d55cf280089baa00149924103505431001200132001355008221122253350011350032200122133350052200230040023335530071200100500400132001355007222533500110022213500222330073330080020060010033200135500622225335001100222135002225335333573466e1c005200000d00c13330080070060031333008007335009123330010080030020060031122002122122330010040031122123300100300212200212200111232300100122330033002002001482c0252210853696c6c69636f6e003351223300248920975e4c7f8d7937f8102e500714feb3f014c8766fcf287a11c10c686154fcb27500480088848cc00400c00880050581840100d87980821a001f372a1a358a2b14f5f6"
--
instance Aeson.ToJSON GYTx where
    toJSON = Aeson.toJSON . txToHex

instance Swagger.ToSchema GYTx where
    declareNamedSchema _ = pure $ Swagger.named "GYTx" $ mempty
                         & Swagger.type_ ?~ Swagger.SwaggerString

txFromApi :: Api.Tx Api.BabbageEra -> GYTx
txFromApi = coerce

txToApi :: GYTx -> Api.Tx Api.BabbageEra
txToApi = coerce

instance Web.FromHttpApiData GYTx where
  parseUrlPiece t = first (T.pack . ("Not a tx, error: " ++)) $ txFromHexBS $ TE.encodeUtf8 t

instance Printf.PrintfArg GYTx where
    formatArg (GYTx tx) = Printf.formatArg (show tx)

-- |
--
-- >>> txToApi <$> txFromHex (Text.unpack $ TE.decodeUtf8 txHexBS)
-- Just (ShelleyTx ShelleyBasedEraBabbage (AlonzoTx {body = TxBodyConstr BabbageTxBodyRaw {btbrSpendInputs = fromList [TxIn (TxId {unTxId = SafeHash "975e4c7f8d7937f8102e500714feb3f014c8766fcf287a11c10c686154fcb275"}) (TxIx 1),TxIn (TxId {unTxId = SafeHash "c887cba672004607a0f60ab28091d5c24860dbefb92b1a8776272d752846574f"}) (TxIx 0)], btbrCollateralInputs = fromList [TxIn (TxId {unTxId = SafeHash "7a67cd033169e330c9ae9b8d0ef8b71de9eb74bbc8f3f6be90446dab7d1e8bfd"}) (TxIx 0)], btbrReferenceInputs = fromList [], btbrOutputs = StrictSeq {fromStrict = fromList [Sized {sizedValue = (Addr Testnet (KeyHashObj (KeyHash "fd040c7a10744b79e5c80ec912a05dbdb3009e372b7f4b0f026d16b0")) (StakeRefBase (KeyHashObj (KeyHash "c663651ffc046068455d2994564ba9d4b3e9b458ad8ab5232aebbf40"))),MaryValue 448448472 (MultiAsset (fromList [])),NoDatum,SNothing), sizedSize = 65},Sized {sizedValue = (Addr Testnet (KeyHashObj (KeyHash "fd040c7a10744b79e5c80ec912a05dbdb3009e372b7f4b0f026d16b0")) (StakeRefBase (KeyHashObj (KeyHash "c663651ffc046068455d2994564ba9d4b3e9b458ad8ab5232aebbf40"))),MaryValue 1551690 (MultiAsset (fromList [(PolicyID {policyID = ScriptHash "a6bb5fd825455e7c69bdaa9d3a6dda9bcbe9b570bc79bd55fa50889b"},fromList [("6e69636b656c",4567)]),(PolicyID {policyID = ScriptHash "b17cb47f51d6744ad05fb937a762848ad61674f8aebbaec67be0bb6f"},fromList [("53696c6c69636f6e",600)])])),NoDatum,SNothing), sizedSize = 151}]}, btbrCollateralReturn = SNothing, btbrTotalCollateral = SNothing, btbrCerts = StrictSeq {fromStrict = fromList []}, btbrWithdrawals = Withdrawals {unWithdrawals = fromList []}, btbrTxFee = Coin 470844, btbrValidityInterval = ValidityInterval {invalidBefore = SNothing, invalidHereafter = SNothing}, btbrUpdate = SNothing, btbrReqSignerHashes = fromList [], btbrMint = MultiAsset (fromList [(PolicyID {policyID = ScriptHash "b17cb47f51d6744ad05fb937a762848ad61674f8aebbaec67be0bb6f"},fromList [("53696c6c69636f6e",600)])]), btbrScriptIntegrityHash = SJust (SafeHash "291b4e4c5f189cb896674e02e354028915b11889687c53d9cf4c1c710ff5e4ae"), btbrAuxDataHash = SNothing, btbrTxNetworkId = SNothing} (blake2b_256: SafeHash "a5e1d764a1bb1e8fab4bb5b8529410bf12517937dac87cbbfec7d59044d16e39"), wits = AlonzoTxWitsRaw {atwrAddrTxWits = fromList [], atwrBootAddrTxWits = fromList [], atwrScriptTxWits = fromList [(ScriptHash "b17cb47f51d6744ad05fb937a762848ad61674f8aebbaec67be0bb6f",PlutusScript PlutusV1 ScriptHash "b17cb47f51d6744ad05fb937a762848ad61674f8aebbaec67be0bb6f")], atwrDatsTxWits = TxDatsConstr TxDatsRaw (fromList []) (blake2b_256: SafeHash "45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0"), atwrRdmrsTxWits = RedeemersConstr RedeemersRaw (fromList [(RdmrPtr Mint 0,(DataConstr Constr 0 [] (blake2b_256: SafeHash "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"),WrapExUnits {unWrapExUnits = ExUnits' {exUnitsMem' = 2045738, exUnitsSteps' = 898247444}}))]) (blake2b_256: SafeHash "3a384e30b63601e50ccbdfc7fe5d1364e52ecf8f0c03a0f6eff44fe42fe65557")} (blake2b_256: SafeHash "9085fea61a5bc7baa0abb2e841264b04987017bc2f61183ad4de77ff6f96fb7c"), isValid = IsValid True, auxiliaryData = SNothing}))
--
txFromHex :: String -> Maybe GYTx
txFromHex s = rightToMaybe $ txFromHexBS $ BS8.pack s

-- |
--
-- >>> txToApi <$> txFromHexBS txHexBS
-- Right (ShelleyTx ShelleyBasedEraBabbage (AlonzoTx {body = TxBodyConstr BabbageTxBodyRaw {btbrSpendInputs = fromList [TxIn (TxId {unTxId = SafeHash "975e4c7f8d7937f8102e500714feb3f014c8766fcf287a11c10c686154fcb275"}) (TxIx 1),TxIn (TxId {unTxId = SafeHash "c887cba672004607a0f60ab28091d5c24860dbefb92b1a8776272d752846574f"}) (TxIx 0)], btbrCollateralInputs = fromList [TxIn (TxId {unTxId = SafeHash "7a67cd033169e330c9ae9b8d0ef8b71de9eb74bbc8f3f6be90446dab7d1e8bfd"}) (TxIx 0)], btbrReferenceInputs = fromList [], btbrOutputs = StrictSeq {fromStrict = fromList [Sized {sizedValue = (Addr Testnet (KeyHashObj (KeyHash "fd040c7a10744b79e5c80ec912a05dbdb3009e372b7f4b0f026d16b0")) (StakeRefBase (KeyHashObj (KeyHash "c663651ffc046068455d2994564ba9d4b3e9b458ad8ab5232aebbf40"))),MaryValue 448448472 (MultiAsset (fromList [])),NoDatum,SNothing), sizedSize = 65},Sized {sizedValue = (Addr Testnet (KeyHashObj (KeyHash "fd040c7a10744b79e5c80ec912a05dbdb3009e372b7f4b0f026d16b0")) (StakeRefBase (KeyHashObj (KeyHash "c663651ffc046068455d2994564ba9d4b3e9b458ad8ab5232aebbf40"))),MaryValue 1551690 (MultiAsset (fromList [(PolicyID {policyID = ScriptHash "a6bb5fd825455e7c69bdaa9d3a6dda9bcbe9b570bc79bd55fa50889b"},fromList [("6e69636b656c",4567)]),(PolicyID {policyID = ScriptHash "b17cb47f51d6744ad05fb937a762848ad61674f8aebbaec67be0bb6f"},fromList [("53696c6c69636f6e",600)])])),NoDatum,SNothing), sizedSize = 151}]}, btbrCollateralReturn = SNothing, btbrTotalCollateral = SNothing, btbrCerts = StrictSeq {fromStrict = fromList []}, btbrWithdrawals = Withdrawals {unWithdrawals = fromList []}, btbrTxFee = Coin 470844, btbrValidityInterval = ValidityInterval {invalidBefore = SNothing, invalidHereafter = SNothing}, btbrUpdate = SNothing, btbrReqSignerHashes = fromList [], btbrMint = MultiAsset (fromList [(PolicyID {policyID = ScriptHash "b17cb47f51d6744ad05fb937a762848ad61674f8aebbaec67be0bb6f"},fromList [("53696c6c69636f6e",600)])]), btbrScriptIntegrityHash = SJust (SafeHash "291b4e4c5f189cb896674e02e354028915b11889687c53d9cf4c1c710ff5e4ae"), btbrAuxDataHash = SNothing, btbrTxNetworkId = SNothing} (blake2b_256: SafeHash "a5e1d764a1bb1e8fab4bb5b8529410bf12517937dac87cbbfec7d59044d16e39"), wits = AlonzoTxWitsRaw {atwrAddrTxWits = fromList [], atwrBootAddrTxWits = fromList [], atwrScriptTxWits = fromList [(ScriptHash "b17cb47f51d6744ad05fb937a762848ad61674f8aebbaec67be0bb6f",PlutusScript PlutusV1 ScriptHash "b17cb47f51d6744ad05fb937a762848ad61674f8aebbaec67be0bb6f")], atwrDatsTxWits = TxDatsConstr TxDatsRaw (fromList []) (blake2b_256: SafeHash "45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0"), atwrRdmrsTxWits = RedeemersConstr RedeemersRaw (fromList [(RdmrPtr Mint 0,(DataConstr Constr 0 [] (blake2b_256: SafeHash "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"),WrapExUnits {unWrapExUnits = ExUnits' {exUnitsMem' = 2045738, exUnitsSteps' = 898247444}}))]) (blake2b_256: SafeHash "3a384e30b63601e50ccbdfc7fe5d1364e52ecf8f0c03a0f6eff44fe42fe65557")} (blake2b_256: SafeHash "9085fea61a5bc7baa0abb2e841264b04987017bc2f61183ad4de77ff6f96fb7c"), isValid = IsValid True, auxiliaryData = SNothing}))
--
txFromHexBS :: BS.ByteString -> Either String GYTx
txFromHexBS bs = BS16.decode bs >>= txFromCBOR

txFromCBOR :: BS.ByteString -> Either String GYTx
txFromCBOR = fmap txFromApi . first show . Api.deserialiseFromCBOR (Api.AsTx Api.AsBabbageEra)

-- |
--
-- >>> txToHexBS tx == txHexBS
-- True
--
txToHexBS :: GYTx -> BS.ByteString
txToHexBS = BS16.encode . txToCBOR

-- | Get CBOR serialisation of the transaction.
txToCBOR :: GYTx -> BS.ByteString
txToCBOR = Api.serialiseToCBOR . txToApi

-- |
--
-- >>> txToHex tx
-- "84a70082825820975e4c7f8d7937f8102e500714feb3f014c8766fcf287a11c10c686154fcb27501825820c887cba672004607a0f60ab28091d5c24860dbefb92b1a8776272d752846574f000d818258207a67cd033169e330c9ae9b8d0ef8b71de9eb74bbc8f3f6be90446dab7d1e8bfd00018282583900fd040c7a10744b79e5c80ec912a05dbdb3009e372b7f4b0f026d16b0c663651ffc046068455d2994564ba9d4b3e9b458ad8ab5232aebbf401a1abac7d882583900fd040c7a10744b79e5c80ec912a05dbdb3009e372b7f4b0f026d16b0c663651ffc046068455d2994564ba9d4b3e9b458ad8ab5232aebbf40821a0017ad4aa2581ca6bb5fd825455e7c69bdaa9d3a6dda9bcbe9b570bc79bd55fa50889ba1466e69636b656c1911d7581cb17cb47f51d6744ad05fb937a762848ad61674f8aebbaec67be0bb6fa14853696c6c69636f6e190258021a00072f3c0e8009a1581cb17cb47f51d6744ad05fb937a762848ad61674f8aebbaec67be0bb6fa14853696c6c69636f6e1902580b5820291b4e4c5f189cb896674e02e354028915b11889687c53d9cf4c1c710ff5e4aea203815908d45908d101000033332332232332232323232323232323232323232323232323232222223232323235500222222222225335333553024120013232123300122333500522002002001002350012200112330012253350021001102d02c25335325335333573466e3cd400488008d404c880080b40b04ccd5cd19b873500122001350132200102d02c102c3500122002102b102c00a132635335738921115554784f206e6f7420636f6e73756d65640002302115335333573466e3c048d5402c880080ac0a854cd4ccd5cd19b8701335500b2200102b02a10231326353357389210c77726f6e6720616d6f756e740002302113263533573892010b77726f6e6720746f6b656e00023021135500122222222225335330245027007162213500222253350041335502d00200122161353333573466e1cd55cea8012400046644246600200600464646464646464646464646666ae68cdc39aab9d500a480008cccccccccc888888888848cccccccccc00402c02802402001c01801401000c008cd40548c8c8cccd5cd19b8735573aa0049000119910919800801801180f1aba15002301a357426ae8940088c98d4cd5ce01381401301289aab9e5001137540026ae854028cd4054058d5d0a804999aa80c3ae501735742a010666aa030eb9405cd5d0a80399a80a80f1aba15006335015335502101f75a6ae854014c8c8c8cccd5cd19b8735573aa00490001199109198008018011919191999ab9a3370e6aae754009200023322123300100300233502475a6ae854008c094d5d09aba2500223263533573805605805405226aae7940044dd50009aba150023232323333573466e1cd55cea8012400046644246600200600466a048eb4d5d0a80118129aba135744a004464c6a66ae700ac0b00a80a44d55cf280089baa001357426ae8940088c98d4cd5ce01381401301289aab9e5001137540026ae854010cd4055d71aba15003335015335502175c40026ae854008c06cd5d09aba2500223263533573804604804404226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226aae7940044dd50009aba150023232323333573466e1d400520062321222230040053016357426aae79400c8cccd5cd19b875002480108c848888c008014c060d5d09aab9e500423333573466e1d400d20022321222230010053014357426aae7940148cccd5cd19b875004480008c848888c00c014dd71aba135573ca00c464c6a66ae7007807c07407006c0680644d55cea80089baa001357426ae8940088c98d4cd5ce00b80c00b00a9100109aab9e5001137540022464460046eb0004c8004d5406488cccd55cf8009280c119a80b98021aba100230033574400402446464646666ae68cdc39aab9d5003480008ccc88848ccc00401000c008c8c8c8cccd5cd19b8735573aa004900011991091980080180118099aba1500233500c012357426ae8940088c98d4cd5ce00b00b80a80a09aab9e5001137540026ae85400cccd5401dd728031aba1500233500875c6ae84d5d1280111931a99ab9c012013011010135744a00226aae7940044dd5000899aa800bae75a224464460046eac004c8004d5405c88c8cccd55cf8011280b919a80b19aa80c18031aab9d5002300535573ca00460086ae8800c0444d5d080089119191999ab9a3370ea0029000119091180100198029aba135573ca00646666ae68cdc3a801240044244002464c6a66ae7004004403c0380344d55cea80089baa001232323333573466e1cd55cea80124000466442466002006004600a6ae854008dd69aba135744a004464c6a66ae7003403803002c4d55cf280089baa0012323333573466e1cd55cea800a400046eb8d5d09aab9e500223263533573801601801401226ea8004488c8c8cccd5cd19b87500148010848880048cccd5cd19b875002480088c84888c00c010c018d5d09aab9e500423333573466e1d400d20002122200223263533573801c01e01a01801601426aae7540044dd50009191999ab9a3370ea0029001100911999ab9a3370ea0049000100911931a99ab9c00a00b009008007135573a6ea80048c8c8c8c8c8cccd5cd19b8750014803084888888800c8cccd5cd19b875002480288488888880108cccd5cd19b875003480208cc8848888888cc004024020dd71aba15005375a6ae84d5d1280291999ab9a3370ea00890031199109111111198010048041bae35742a00e6eb8d5d09aba2500723333573466e1d40152004233221222222233006009008300c35742a0126eb8d5d09aba2500923333573466e1d40192002232122222223007008300d357426aae79402c8cccd5cd19b875007480008c848888888c014020c038d5d09aab9e500c23263533573802402602202001e01c01a01801601426aae7540104d55cf280189aab9e5002135573ca00226ea80048c8c8c8c8cccd5cd19b875001480088ccc888488ccc00401401000cdd69aba15004375a6ae85400cdd69aba135744a00646666ae68cdc3a80124000464244600400660106ae84d55cf280311931a99ab9c00b00c00a009008135573aa00626ae8940044d55cf280089baa001232323333573466e1d400520022321223001003375c6ae84d55cf280191999ab9a3370ea004900011909118010019bae357426aae7940108c98d4cd5ce00400480380300289aab9d5001137540022244464646666ae68cdc39aab9d5002480008cd5403cc018d5d0a80118029aba135744a004464c6a66ae7002002401c0184d55cf280089baa00149924103505431001200132001355008221122253350011350032200122133350052200230040023335530071200100500400132001355007222533500110022213500222330073330080020060010033200135500622225335001100222135002225335333573466e1c005200000d00c13330080070060031333008007335009123330010080030020060031122002122122330010040031122123300100300212200212200111232300100122330033002002001482c0252210853696c6c69636f6e003351223300248920975e4c7f8d7937f8102e500714feb3f014c8766fcf287a11c10c686154fcb27500480088848cc00400c00880050581840100d87980821a001f372a1a358a2b14f5f6"
-- >>> txToHex tx == (Text.unpack $ TE.decodeUtf8 txHexBS)
-- True
--
txToHex :: GYTx -> String
txToHex = BS8.unpack . txToHexBS

writeTx :: FilePath -> GYTx -> IO ()
writeTx file tx = do
    e <- Api.writeFileTextEnvelope (Api.File file) Nothing (txToApi tx)
    case e of
        Left err -> ioError $ userError $ show err
        Right () -> pure ()

-- | Transaction hash/id of a particular transaction.
newtype GYTxId = GYTxId Api.TxId
    deriving (Eq, Ord)
    deriving newtype (FromJSON)  -- TODO: Also derive ToJSON?

instance PQ.ToField GYTxId where
    toField (GYTxId txId) = PQ.toField (PQ.Binary (Api.serialiseToRawBytes txId))

-- |
--
-- >>> show gyTxId
-- "6c751d3e198c5608dfafdfdffe16aeac8a28f88f3a769cf22dd45e8bc84f47e8"
--
instance Show GYTxId where
  show (GYTxId txid) = T.unpack
                     $ TE.decodeUtf8
                     $ Api.serialiseToRawBytesHex txid

-- |
--
-- >>> "6c751d3e198c5608dfafdfdffe16aeac8a28f88f3a769cf22dd45e8bc84f47e8" :: GYTxId
-- 6c751d3e198c5608dfafdfdffe16aeac8a28f88f3a769cf22dd45e8bc84f47e8
--
instance IsString GYTxId where
  fromString = either error id . txIdFromHexE

-- |
--
-- >>> Aeson.toJSON gyTxId
-- String "6c751d3e198c5608dfafdfdffe16aeac8a28f88f3a769cf22dd45e8bc84f47e8"
--
instance ToJSON GYTxId where
  toJSON (GYTxId txid) = Aeson.String
                       $ TE.decodeUtf8
                       $ Api.serialiseToRawBytesHex txid


instance Swagger.ToSchema GYTxId where
  declareNamedSchema _ = pure $ Swagger.named "GYTxId" $ mempty
                       & Swagger.example ?~ toJSON ("a8d75b90a052302c1232bedd626720966b1697fe38de556c617c340233688935" :: Text)
                       & Swagger.type_ ?~ Swagger.SwaggerString
--
-- |
--
-- >>> Printf.printf "tid = %s" gyTxId
-- tid = 6c751d3e198c5608dfafdfdffe16aeac8a28f88f3a769cf22dd45e8bc84f47e8
--
instance Printf.PrintfArg GYTxId where
    formatArg tid = Printf.formatArg (show tid)

txIdFromHex :: String -> Maybe GYTxId
txIdFromHex = rightToMaybe . txIdFromHexE

txIdFromHexE :: String -> Either String GYTxId
txIdFromHexE = coerce . first show . Api.deserialiseFromRawBytesHex (Api.proxyToAsType @Api.TxId Proxy) . BS8.pack

txIdToApi :: GYTxId -> Api.TxId
txIdToApi = coerce

txIdFromApi :: Api.TxId -> GYTxId
txIdFromApi = coerce

txIdFromPlutus :: Plutus.TxId -> Either Api.S.SerialiseAsRawBytesError GYTxId
txIdFromPlutus (Plutus.TxId (Plutus.BuiltinByteString bs)) = txIdFromApi <$> Api.deserialiseFromRawBytes Api.AsTxId bs

-- | Wrapper around transaction witness set. Note that Babbage ledger also uses the same @TxWitness@ type defined in Alonzo ledger, which was updated for Plutus-V2 scripts and same is expected for Plutus-V3.
newtype GYTxWitness = GYTxWitness (AlonzoTxWits (Babbage.BabbageEra Crypto.StandardCrypto))
  deriving newtype Show

instance Swagger.ToSchema GYTxWitness where
    declareNamedSchema _ = pure $ Swagger.named "GYTxWitness" $ mempty
                         & Swagger.type_ ?~ Swagger.SwaggerString

instance Printf.PrintfArg GYTxWitness where
    formatArg (GYTxWitness txWit) = Printf.formatArg (show txWit)

instance Aeson.FromJSON GYTxWitness where
  parseJSON = Aeson.withText "GYTxWitness" $ \t -> do
    case txWitFromHexBS $ TE.encodeUtf8 t of
      Left err    -> fail $ "Not a GYTxWitness: " ++ err
      Right txWit -> return txWit

instance Web.FromHttpApiData GYTxWitness where
  parseUrlPiece t = first (T.pack . ("Not a tx witness, error: " ++)) $ txWitFromHexBS $ TE.encodeUtf8 t

txWitFromHexBS :: BS.ByteString -> Either String GYTxWitness
txWitFromHexBS bs = do
  bs' <- BS16.decode bs
  txWit <- first show $ CBOR.decodeFullAnnotator (eraProtVerHigh @Babbage) "Reading transaction witness set" CBOR.decCBOR (LBS.fromStrict bs')
  return (GYTxWitness txWit)

txWitFromHex :: String -> Maybe GYTxWitness
txWitFromHex = rightToMaybe . txWitFromHexBS . TE.encodeUtf8 . fromString

txWitFromLedger :: AlonzoTxWits (Babbage.BabbageEra Crypto.StandardCrypto) -> GYTxWitness
txWitFromLedger = coerce

txWitToLedger :: GYTxWitness -> AlonzoTxWits (Babbage.BabbageEra Crypto.StandardCrypto)
txWitToLedger = coerce

-- `txWitCbor` is the cbor obtained using CIP-30 compatible wallet's `api.signTx`.
-- >>> txWitCbor = "a100818258206400a17ee58ce12a54c6edb7b964f0eb217e00dac75f2a47eccb6eedd02809a4584048bfa2dbf21514cafd1425b0072c67dd09cce82f5688169cff4761ca47e6557371ecc1ccbadde231dee179cf17d9dac29a61ac64ff9ef2dbd94968ec26301801"
-- >>> txWit = txWitFromHex txWitCbor
-- >>> show txWit
-- "Just AlonzoTxWitsRaw {atwrAddrTxWits = fromList [WitVKeyInternal {wvkKey = VKey (VerKeyEd25519DSIGN \"6400a17ee58ce12a54c6edb7b964f0eb217e00dac75f2a47eccb6eedd02809a4\"), wvkSig = SignedDSIGN (SigEd25519DSIGN \"48bfa2dbf21514cafd1425b0072c67dd09cce82f5688169cff4761ca47e6557371ecc1ccbadde231dee179cf17d9dac29a61ac64ff9ef2dbd94968ec26301801\"), wvkKeyHash = KeyHash \"f24712bd05f058c6dca5df794f6afbffa8392076e7cb9fda9f508d7a\", wvkBytes = \"\\130X d\\NUL\\161~\\229\\140\\225*T\\198\\237\\183\\185d\\240\\235!~\\NUL\\218\\199_*G\\236\\203n\\237\\208(\\t\\164X@H\\191\\162\\219\\242\\NAK\\DC4\\202\\253\\DC4%\\176\\a,g\\221\\t\\204\\232/V\\136\\SYN\\156\\255Ga\\202G\\230Usq\\236\\193\\204\\186\\221\\226\\&1\\222\\225y\\207\\ETB\\217\\218\\194\\154a\\172d\\255\\158\\242\\219\\217Ih\\236&0\\CAN\\SOH\"}], atwrBootAddrTxWits = fromList [], atwrScriptTxWits = fromList [], atwrDatsTxWits = TxDatsConstr TxDatsRaw (fromList []) (blake2b_256: SafeHash \"45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0\"), atwrRdmrsTxWits = RedeemersConstr RedeemersRaw (fromList []) (blake2b_256: SafeHash \"45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0\")} (blake2b_256: SafeHash \"e95be59b0840700f3441bd540ef3ebe87ca5f40d331ce02e3c47cc0da797bd25\")"
-- >>> txWitToKeyWitnessApi <$> txWit
-- Just [ShelleyKeyWitness ShelleyBasedEraBabbage (WitVKeyInternal {wvkKey = VKey (VerKeyEd25519DSIGN "6400a17ee58ce12a54c6edb7b964f0eb217e00dac75f2a47eccb6eedd02809a4"), wvkSig = SignedDSIGN (SigEd25519DSIGN "48bfa2dbf21514cafd1425b0072c67dd09cce82f5688169cff4761ca47e6557371ecc1ccbadde231dee179cf17d9dac29a61ac64ff9ef2dbd94968ec26301801"), wvkKeyHash = KeyHash "f24712bd05f058c6dca5df794f6afbffa8392076e7cb9fda9f508d7a", wvkBytes = "\130X d\NUL\161~\229\140\225*T\198\237\183\185d\240\235!~\NUL\218\199_*G\236\203n\237\208(\t\164X@H\191\162\219\242\NAK\DC4\202\253\DC4%\176\a,g\221\t\204\232/V\136\SYN\156\255Ga\202G\230Usq\236\193\204\186\221\226\&1\222\225y\207\ETB\217\218\194\154a\172d\255\158\242\219\217Ih\236&0\CAN\SOH"})]

-- >>> txWitCbor' = "A200818258206400A17EE58CE12A54C6EDB7B964F0EB217E00DAC75F2A47ECCB6EEDD02809A4584048BFA2DBF21514CAFD1425B0072C67DD09CCE82F5688169CFF4761CA47E6557371ECC1CCBADDE231DEE179CF17D9DAC29A61AC64FF9EF2DBD94968EC263018010481D87983581C25195AF85C41B9D97DA7F4F215D3E74C9CEF7F04739D6BA473BA72A2D87982D87981581C25195AF85C41B9D97DA7F4F215D3E74C9CEF7F04739D6BA473BA72A2D87981D87981D87981581C358A4E4105C08F59E4779070699C7A72566893332F9857DB4E742BEBD879811B00000189B0D436E7"
-- >>> txWit' = txWitFromHex txWitCbor'
-- >>> show txWit'
-- "Just AlonzoTxWitsRaw {atwrAddrTxWits = fromList [WitVKeyInternal {wvkKey = VKey (VerKeyEd25519DSIGN \"6400a17ee58ce12a54c6edb7b964f0eb217e00dac75f2a47eccb6eedd02809a4\"), wvkSig = SignedDSIGN (SigEd25519DSIGN \"48bfa2dbf21514cafd1425b0072c67dd09cce82f5688169cff4761ca47e6557371ecc1ccbadde231dee179cf17d9dac29a61ac64ff9ef2dbd94968ec26301801\"), wvkKeyHash = KeyHash \"f24712bd05f058c6dca5df794f6afbffa8392076e7cb9fda9f508d7a\", wvkBytes = \"\\130X d\\NUL\\161~\\229\\140\\225*T\\198\\237\\183\\185d\\240\\235!~\\NUL\\218\\199_*G\\236\\203n\\237\\208(\\t\\164X@H\\191\\162\\219\\242\\NAK\\DC4\\202\\253\\DC4%\\176\\a,g\\221\\t\\204\\232/V\\136\\SYN\\156\\255Ga\\202G\\230Usq\\236\\193\\204\\186\\221\\226\\&1\\222\\225y\\207\\ETB\\217\\218\\194\\154a\\172d\\255\\158\\242\\219\\217Ih\\236&0\\CAN\\SOH\"}], atwrBootAddrTxWits = fromList [], atwrScriptTxWits = fromList [], atwrDatsTxWits = TxDatsConstr TxDatsRaw (fromList [(SafeHash \"363eaf5abda5b96e8887f50f2ecb619e65f9b1bc7ada10e46ddf67ccd8d60b54\",DataConstr Constr 0 [B \"%\\EMZ\\248\\\\A\\185\\217}\\167\\244\\242\\NAK\\211\\231L\\156\\239\\DEL\\EOTs\\157k\\164s\\186r\\162\",Constr 0 [Constr 0 [B \"%\\EMZ\\248\\\\A\\185\\217}\\167\\244\\242\\NAK\\211\\231L\\156\\239\\DEL\\EOTs\\157k\\164s\\186r\\162\"],Constr 0 [Constr 0 [Constr 0 [B \"5\\138NA\\ENQ\\192\\143Y\\228w\\144pi\\156zrVh\\147\\&3/\\152W\\219Nt+\\235\"]]]],Constr 0 [I 1690888845031]] (blake2b_256: SafeHash \"363eaf5abda5b96e8887f50f2ecb619e65f9b1bc7ada10e46ddf67ccd8d60b54\"))]) (blake2b_256: SafeHash \"680688c4f76e7a17c6959cba342848deb2a7a835614dac36322f3bd82266178d\"), atwrRdmrsTxWits = RedeemersConstr RedeemersRaw (fromList []) (blake2b_256: SafeHash \"45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0\")} (blake2b_256: SafeHash \"de9582be5cd5ffaaeddf425df752428496c12ec3b0b200e2dd7614d2c66c9475\")"
-- >>> txWitToKeyWitnessApi <$> txWit'
-- Just [ShelleyKeyWitness ShelleyBasedEraBabbage (WitVKeyInternal {wvkKey = VKey (VerKeyEd25519DSIGN "6400a17ee58ce12a54c6edb7b964f0eb217e00dac75f2a47eccb6eedd02809a4"), wvkSig = SignedDSIGN (SigEd25519DSIGN "48bfa2dbf21514cafd1425b0072c67dd09cce82f5688169cff4761ca47e6557371ecc1ccbadde231dee179cf17d9dac29a61ac64ff9ef2dbd94968ec26301801"), wvkKeyHash = KeyHash "f24712bd05f058c6dca5df794f6afbffa8392076e7cb9fda9f508d7a", wvkBytes = "\130X d\NUL\161~\229\140\225*T\198\237\183\185d\240\235!~\NUL\218\199_*G\236\203n\237\208(\t\164X@H\191\162\219\242\NAK\DC4\202\253\DC4%\176\a,g\221\t\204\232/V\136\SYN\156\255Ga\202G\230Usq\236\193\204\186\221\226\&1\222\225y\207\ETB\217\218\194\154a\172d\255\158\242\219\217Ih\236&0\CAN\SOH"})]

-- | Obtain `vkeywitness` as cddl calls it to make our unsigned transaction, signed (see `makeSignedTransaction` method).
txWitToKeyWitnessApi :: GYTxWitness -> [Api.S.KeyWitness Api.S.BabbageEra]
txWitToKeyWitnessApi = fmap (Api.S.ShelleyKeyWitness Api.ShelleyBasedEraBabbage) . Set.toList . view addrAlonzoTxWitsL . txWitToLedger

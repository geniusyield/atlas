module GeniusYield.Test.Providers
    ( providersTests
    ) where

import           Data.Aeson as Aeson (decode, decodeStrict, Value)
import           Data.ByteString.Lazy as BS (readFile, toStrict)
import           Data.String         (fromString)
import qualified Data.Text as Text   (Text, unpack, pack, replace)
import           Data.Maybe          (fromJust, fromMaybe)
import           Data.Map.Strict as Map    (difference)
import           Data.Some (mkSome)
import qualified Data.Set as Set (fromList, difference)
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Text.Printf                       as Printf
import           Test.Tasty.Golden.Advanced        (goldenTest)

import           GHC.Natural                       (Natural)
import           GeniusYield.Types.Address         (GYAddress,
                                                    unsafeAddressFromText)
import           GeniusYield.Types.Datum           (datumFromApi',
                                                    datumHashFromHex)
import           GeniusYield.Types.Script          (scriptFromCBOR)
import           GeniusYield.Types.TxOutRef        (GYTxOutRef)
import           GeniusYield.Types.UTxO            (GYOutDatum (..),
                                                    GYUTxO (..), utxosToApi,
                                                    utxosFromList, utxosRefs,
                                                    utxosFromApi)
import           GeniusYield.Types.Value           (GYAssetClass (GYLovelace, GYToken),
                                                    valueFromList,
                                                    valueFromLovelace)

import qualified Cardano.Api                       as Api
import           GeniusYield.GYConfig
import           GeniusYield.Providers.Common      (SomeDeserializeError (..))
import           GeniusYield.Providers.Maestro     (utxoFromMaestro, maestroQueryUtxo,
                                                    networkIdToMaestroEnv)
import           GeniusYield.Test.Providers.Mashup (providersMashupTests)
import           GeniusYield.Types                 (PlutusVersion (PlutusV2), GYQueryUTxO, GYNetworkId,
                                                    gyQueryUtxosAtAddress', gyQueryUtxosAtAddresses',
                                                    gyQueryUtxoAtTxOutRef', gyQueryUtxoRefsAtAddress')
import qualified Maestro.Types                     as Maestro

providersTests :: [GYCoreConfig] -> Text.Text -> GYNetworkId -> TestTree
providersTests configs pToken netId = testGroup "Providers" [ testGroup "Maestro" (maestroTests pToken netId), providersMashupTests configs ]

maestroTests :: Text.Text -> GYNetworkId -> [TestTree]
maestroTests token netId =
    [ testGroup "Maestro Provider GYQueryUTxO"
        [ goldenTestUtxos "UtxosAtAddress Simple" (getUTxOsAtAddress simpleQueryAddress token) (getFileUTxOs simpleAddressPath)
        , goldenTestUtxos "UtxosAtAddress Complex" (getUTxOsAtAddress queryAddress1 token) (getFileUTxOs complexAddressPath)
        , goldenTestUtxos "UtxosAtAddresses" (getUTxOsAtAddresses [queryAddress1, queryAddress2, queryAddress3] token) (getFileUTxOs utxosAtAddressesPath)
        , goldenTestUtxos "UtxoAtRef" (getUTxOAtRef mockQueryTxOutRef token) (getFileUTxOs utxoAtRefPath)
        , goldenTestRefs  "UtxosRefsAtAddress" (getUTxOsRefsAtAddress simpleQueryAddress token) (getFileRefs simpleAddressPath)
        ]
    , testGroup "MaestroUtxo to GYUTxO translation"
        [ testCase "Invalid Address" $ do
            let expected = Left DeserializeErrorAddress
                res = utxoFromMaestro $ Maestro.Utxo
                                        { _utxoTxHash          = mockTxId
                                        , _utxoIndex           = mockTxIx
                                        , _utxoAssets          = []
                                        , _utxoAddress         = "invalidaddress"
                                        , _utxoDatum           = Nothing
                                        , _utxoReferenceScript = Nothing
                                        }
            res @?= expected
        , testCase "Invalid UTxORef" $ do
            let expected = Left (DeserializeErrorHex "GYTxOutRef: Failed reading: takeWhile1")
                res = utxoFromMaestro $ Maestro.Utxo
                                        { _utxoTxHash          = "invalidhash"
                                        , _utxoIndex           = mockTxIx
                                        , _utxoAssets          = []
                                        , _utxoAddress         = mockAddressB32
                                        , _utxoDatum           = Nothing
                                        , _utxoReferenceScript = Nothing
                                        }
            res @?= expected
        , testCase "Simplest Case" $ do
            let expected = Right GYUTxO { utxoRef       = mockTxOutRef
                                        , utxoAddress   = mockAddress
                                        , utxoValue     = valueFromList []
                                        , utxoOutDatum  = GYOutDatumNone
                                        , utxoRefScript = Nothing
                                        }
                res = utxoFromMaestro $ mockMaestroUtxo [] Nothing Nothing
            res @?= expected
        , testCase "Some Adas" $ do
            let expected = Right GYUTxO { utxoRef       = mockTxOutRef
                                        , utxoAddress   = mockAddress
                                        , utxoValue     = valueFromLovelace 100_000_000
                                        , utxoOutDatum  = GYOutDatumNone
                                        , utxoRefScript = Nothing
                                        }
                res = utxoFromMaestro $ mockMaestroUtxo [maestroAssetFromLovelace 100_000_000] Nothing Nothing
            res @?= expected
        , testCase "Some Adas and tokens" $ do
            let expected = Right GYUTxO { utxoRef       = mockTxOutRef
                                        , utxoAddress   = mockAddress
                                        , utxoValue     = valueFromList [ (GYLovelace, 100_000_000)
                                                                        , (mockAssetA, 100)
                                                                        , (mockAssetEmptyName, 1000)
                                                                        ]
                                        , utxoOutDatum  = GYOutDatumNone
                                        , utxoRefScript = Nothing
                                        }
                res = utxoFromMaestro $ mockMaestroUtxo [ maestroAssetFromLovelace 100_000_000
                                                      , maestroAssetSingleton mockAssetA 100
                                                      , maestroAssetSingleton mockAssetEmptyName 1000
                                                      ]
                                                      Nothing
                                                      Nothing
            res @?= expected
        , testCase "Datum Hash" $ do
            let expected = Right GYUTxO { utxoRef       = mockTxOutRef
                                        , utxoAddress   = mockAddress
                                        , utxoValue     = valueFromList []
                                        , utxoOutDatum  = GYOutDatumHash (fromJust $ datumHashFromHex $ Text.unpack mockDatumHashHex)
                                        , utxoRefScript = Nothing
                                        }
                res = utxoFromMaestro $ mockMaestroUtxo [] (Just maestroDatumHash) Nothing
            res @?= expected
        , testCase "Datum Inline" $ do
            let expected = Right GYUTxO { utxoRef       = mockTxOutRef
                                        , utxoAddress   = mockAddress
                                        , utxoValue     = valueFromList []
                                        , utxoOutDatum  = GYOutDatumInline $ datumFromApi' $ either (error "Unable to read mock datum") id $ Api.scriptDataFromJson Api.ScriptDataJsonDetailedSchema mockScriptDataDetailed
                                        , utxoRefScript = Nothing
                                        }
                res = utxoFromMaestro $ mockMaestroUtxo [] (Just maestroInlineDatum) Nothing
            res @?= expected
        , testCase "Ref Script" $ do
            let expected = Right GYUTxO { utxoRef       = mockTxOutRef
                                        , utxoAddress   = mockAddress
                                        , utxoValue     = valueFromList []
                                        , utxoOutDatum  = GYOutDatumNone
                                        , utxoRefScript = mkSome <$> scriptFromCBOR  @'PlutusV2 mockScriptCBOR
                                        }
                res = utxoFromMaestro $ mockMaestroUtxo [] Nothing (Just mockMaestroScript)
            res @?= expected
        ]
    ]
  where
    getQueryUtxo :: Text.Text -> IO GYQueryUTxO
    getQueryUtxo pToken = maestroQueryUtxo <$> networkIdToMaestroEnv pToken netId

    getUTxOsAtAddress :: GYAddress -> Text.Text -> IO (Api.UTxO Api.BabbageEra)
    getUTxOsAtAddress addr pToken = do
        queryUtxo <- getQueryUtxo pToken
        utxos <- gyQueryUtxosAtAddress' queryUtxo addr
        return $ utxosToApi utxos

    getUTxOsAtAddresses :: [GYAddress] -> Text.Text -> IO (Api.UTxO Api.BabbageEra)
    getUTxOsAtAddresses addrs pToken = do
        queryUtxo <- getQueryUtxo pToken
        utxos <- gyQueryUtxosAtAddresses' queryUtxo addrs
        return $ utxosToApi utxos

    getUTxOAtRef :: GYTxOutRef -> Text.Text -> IO (Api.UTxO Api.BabbageEra)
    getUTxOAtRef ref pToken = do
        queryUtxo <- getQueryUtxo pToken
        utxo <- gyQueryUtxoAtTxOutRef' queryUtxo ref
        return $ utxosToApi $ utxosFromList [fromJust utxo]

    getUTxOsRefsAtAddress :: GYAddress -> Text.Text -> IO [GYTxOutRef]
    getUTxOsRefsAtAddress addr pToken = do
        queryUtxo <- getQueryUtxo pToken
        gyQueryUtxoRefsAtAddress' queryUtxo addr

    getFileRefs :: String -> IO [GYTxOutRef]
    getFileRefs fileName = do
        json <- BS.readFile fileName
        let utxos = fromMaybe (utxosToApi $ utxosFromList []) (Aeson.decodeStrict (toStrict json))
            refs = utxosRefs $ utxosFromApi utxos
        return refs

    getFileUTxOs :: String -> IO (Api.UTxO Api.BabbageEra)
    getFileUTxOs fileName = do
        json <- BS.readFile fileName
        let utxos = fromMaybe (utxosToApi $ utxosFromList []) (Aeson.decodeStrict (toStrict json))
        return utxos

    compareUTxOs :: Api.UTxO Api.BabbageEra -> Api.UTxO Api.BabbageEra -> IO (Maybe String)
    compareUTxOs utxosQuery utxosFile =
        return $ if utxosQuery == utxosFile
            then Nothing
            else Just $ show (Map.difference (Api.unUTxO utxosFile) (Api.unUTxO utxosQuery))

    compareRefs :: [GYTxOutRef] -> [GYTxOutRef] -> IO (Maybe String)
    compareRefs refsQuery refsFile = do
        let refSetQuery = Set.fromList refsQuery
            refSetFile = Set.fromList refsFile
        return $ if refSetQuery == refSetFile
            then Nothing
            else Just $ show (Set.difference refSetFile refSetQuery)

    updateGolden :: Show a => a -> IO ()
    updateGolden = error . show

    goldenTestUtxos :: TestName -> IO (Api.UTxO Api.BabbageEra) -> IO (Api.UTxO Api.BabbageEra) -> TestTree
    goldenTestUtxos name queryData getFileData =
        goldenTest name queryData getFileData compareUTxOs updateGolden

    goldenTestRefs :: TestName -> IO [GYTxOutRef] -> IO [GYTxOutRef] -> TestTree
    goldenTestRefs name queryData getFileData =
        goldenTest name queryData getFileData compareRefs updateGolden

-------------------------------------------------------------------------------
-- Mock Values
-------------------------------------------------------------------------------
mockQueryTxOutRef :: GYTxOutRef
mockQueryTxOutRef = fromString $ concat [Text.unpack mockQueryTxId, "#", show mockQueryTxIx]

mockQueryTxId :: Text.Text
mockQueryTxId = "45e7172a4ff66f6e6236ca910182ac4e391f8b8a7cb8c22a26c489981dd7a0b9"

mockQueryTxIx :: Word
mockQueryTxIx = 0

mockMaestroUtxo :: [Maestro.Asset] -> Maybe Maestro.DatumOption -> Maybe Maestro.Script ->Maestro.Utxo
mockMaestroUtxo assets mDat mRefScript = Maestro.Utxo
  { _utxoTxHash          = mockTxId
  , _utxoIndex           = mockTxIx
  , _utxoAssets          = assets
  , _utxoAddress         = mockAddressB32
  , _utxoDatum           = mDat
  , _utxoReferenceScript = mRefScript
  }

mockTxId :: Text.Text
mockTxId = "4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189"

mockTxIx :: Natural
mockTxIx = 0

mockTxOutRef :: GYTxOutRef
mockTxOutRef = fromString $ concat [Text.unpack mockTxId, "#", show mockTxIx]

mockAddressB32 :: Text.Text
mockAddressB32 = "addr_test1qr30nkfx28r452r3006kytnpvn39zv7c2m5uqt4zrg35mly35pesdyk43wnxk3edkkw74ak56n4zh67reqjhcfp3mm7qtyekt4"

mockAddress :: GYAddress
mockAddress = unsafeAddressFromText mockAddressB32

maestroDatumHash :: Maestro.DatumOption
maestroDatumHash = Maestro.DatumOption
  { _datumOptionType  = Maestro.Hash
  , _datumOptionHash  = mockDatumHashHex
  , _datumOptionBytes = Nothing
  , _datumOptionJson  = Nothing
  }

maestroInlineDatum :: Maestro.DatumOption
maestroInlineDatum = Maestro.DatumOption
  { _datumOptionType  = Maestro.Inline
  , _datumOptionHash  = mockDatumHashHex
  , _datumOptionBytes = Just mockDatumBtyes
  , _datumOptionJson  = Just mockScriptDataDetailed
  }

mockDatumBtyes :: Text.Text
mockDatumBtyes = "d8799fd8799f1830ffff"

mockDatumHashHex :: Text.Text
mockDatumHashHex = "b034c17cf9eef7e2d38fff1ec8956c3a3c9fece616e1ce03df5860fee81adb1e"

mockScriptDataDetailed :: Aeson.Value
mockScriptDataDetailed = fromJust $ Aeson.decode "{\"fields\": [{\"fields\": [{\"int\": 48}],\"constructor\": 0}],\"constructor\": 0}"

maestroAssetFromLovelace :: Integer -> Maestro.Asset
maestroAssetFromLovelace n = Maestro.Asset { _assetQuantity = fromIntegral n
                                           , _assetUnit = Text.pack $ Printf.printf "%s" GYLovelace
                                           }

maestroAssetSingleton :: GYAssetClass -> Integer -> Maestro.Asset
maestroAssetSingleton ac n = Maestro.Asset { _assetQuantity = fromIntegral n
                                           , _assetUnit = Text.replace "." "#" $ Text.pack $ Printf.printf "%s" ac
                                           }

mockAssetA :: GYAssetClass
mockAssetA = GYToken "005eaf690cba88f441494e42f5edce9bd7f595c56f99687e2fa0aad4" "A"

mockAssetEmptyName :: GYAssetClass
mockAssetEmptyName = GYToken "005eaf690cba88f441494e42f5edce9bd7f595c56f99687e2fa0bbd4" ""

mockMaestroScript :: Maestro.Script
mockMaestroScript = Maestro.Script
  { _scriptType  = Maestro.PlutusV2
  , _scriptHash  = "90dbacba2758d72a3e0d75c56fbe393da91cc474a4bffbb59c3baeb6"
  , _scriptBytes = Just mockScriptCBOR
  , _scriptJson  = Nothing
  }

mockScriptCBOR :: Text.Text
mockScriptCBOR = "5910fe010000332323232323232323232323232323322323232322232232322323253353330093333573466e1cd55cea803a4000464646666ae68cdc39aab9d5001480008dd69aba135573ca004464c6a66ae7007c0780740704dd50009aba135573ca010464c6a66ae7007006c068064cccd5cd19b875004480188488880108cccd5cd19b875005480108c848888c004014ccd54069d73ae357426aae79401c8cccd5cd19b8750064800884888800c8cccd5cd19b875007480008488880088c98d4cd5ce00f80f00e80e00d80d00c9999ab9a3370e6aae754009200023322123300100300232323232323232323232323333573466e1cd55cea8052400046666666666444444444424666666666600201601401201000e00c00a00800600466a02e464646666ae68cdc39aab9d5002480008cc8848cc00400c008c088d5d0a801180e1aba135744a004464c6a66ae700b00ac0a80a44d55cf280089baa00135742a01466a02e0306ae854024ccd54069d7280c9aba150083335501a75ca0326ae85401ccd405c088d5d0a80319a80b99aa812811bad35742a00a6464646666ae68cdc39aab9d5002480008cc8848cc00400c008c8c8c8cccd5cd19b8735573aa004900011991091980080180119a8143ad35742a00460526ae84d5d1280111931a99ab9c03002f02e02d135573ca00226ea8004d5d0a8011919191999ab9a3370e6aae754009200023322123300100300233502875a6ae854008c0a4d5d09aba2500223263533573806005e05c05a26aae7940044dd50009aba135744a004464c6a66ae700b00ac0a80a44d55cf280089baa00135742a00866a02eeb8d5d0a80199a80b99aa812bae200135742a004603e6ae84d5d1280111931a99ab9c028027026025135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135573ca00226ea8004d5d0a8011919191999ab9a3370ea0029003119091111802002980d1aba135573ca00646666ae68cdc3a8012400846424444600400a60386ae84d55cf280211999ab9a3370ea0069001119091111800802980c1aba135573ca00a46666ae68cdc3a8022400046424444600600a6eb8d5d09aab9e500623263533573804604404204003e03c03a26aae7540044dd50009aba135744a004464c6a66ae7007006c06806440684c98d4cd5ce2481035054350001a019135573ca00226ea80044d55cea80089baa001137540022464460046eb0004c8004d5405088cccd55cf80092804919a80418021aba100230033574400402646464646666ae68cdc39aab9d5003480008ccc88848ccc00401000c008c8c8c8cccd5cd19b8735573aa0049000119910919800801801180a9aba1500233500e014357426ae8940088c98d4cd5ce00c80c00b80b09aab9e5001137540026ae85400cccd5401dd728031aba1500233500a75c6ae84d5d1280111931a99ab9c015014013012135744a00226aae7940044dd5000899aa800bae75a224464460046eac004c8004d5404888c8cccd55cf80112804119a80399aa80a98031aab9d5002300535573ca00460086ae8800c0484d5d080088910010910911980080200189119191999ab9a3370ea0029000119091180100198029aba135573ca00646666ae68cdc3a801240044244002464c6a66ae7004404003c0380344d55cea80089baa001232323333573466e1cd55cea80124000466442466002006004600a6ae854008dd69aba135744a004464c6a66ae7003803403002c4d55cf280089baa0012323333573466e1cd55cea800a400046eb8d5d09aab9e500223263533573801801601401226ea8004488c8c8cccd5cd19b87500148010848880048cccd5cd19b875002480088c84888c00c010c018d5d09aab9e500423333573466e1d400d20002122200223263533573801e01c01a01801601426aae7540044dd50009191999ab9a3370ea0029001109100111999ab9a3370ea0049000109100091931a99ab9c00b00a009008007135573a6ea80048c8c8c8c8c8cccd5cd19b8750014803084888888800c8cccd5cd19b875002480288488888880108cccd5cd19b875003480208cc8848888888cc004024020dd71aba15005375a6ae84d5d1280291999ab9a3370ea00890031199109111111198010048041bae35742a00e6eb8d5d09aba2500723333573466e1d40152004233221222222233006009008300c35742a0126eb8d5d09aba2500923333573466e1d40192002232122222223007008300d357426aae79402c8cccd5cd19b875007480008c848888888c014020c038d5d09aab9e500c23263533573802602402202001e01c01a01801601426aae7540104d55cf280189aab9e5002135573ca00226ea80048c8c8c8c8cccd5cd19b875001480088ccc888488ccc00401401000cdd69aba15004375a6ae85400cdd69aba135744a00646666ae68cdc3a80124000464244600400660106ae84d55cf280311931a99ab9c00c00b00a009008135573aa00626ae8940044d55cf280089baa001232323333573466e1d400520022321223001003375c6ae84d55cf280191999ab9a3370ea004900011909118010019bae357426aae7940108c98d4cd5ce00480400380300289aab9d5001137540022244464646666ae68cdc39aab9d5002480008cd54028c018d5d0a80118029aba135744a004464c6a66ae7002402001c0184d55cf280089baa0014984800524103505431001122123300100300211232300100122330033002002001332323322332232323232332232323232323232323232323232323232332232323232323232323232222232323232323253333500815335333573466e1ccdc3004a400890010170168817099ab9c49011c5374617465206e6f742076616c696420666f7220636c6f73696e672e0002d15335300d0072135001223500122253353330170120023550082220021500715335335738920115496e76616c6964206f75747075742076616c75652e0003315007103313562615335300d00721350012235001222533533301701200235500a222002150091533533573892115496e76616c6964206f75747075742076616c75652e00033150091033135626232323232153353232325335333573466e20044cdc0000a400806a06c2a0042a66a666ae68cdc480899b81001480100d80d45400840d4d4044880044ccd5cd19b883322333355002323350272233350250030010023502200133502622230033002001200122337000029001000a4000603c2400266062a06c002900301a019a8058a8008a99a99ab9c491225374617465206e6f742076616c696420666f72206d696e74696e67207072697a652e000321500110321533533301501032323355301d1200123500122335503a002335530201200123500122335503d00233350012330374800000488cc0e00080048cc0dc0052000001330180020015004500a355009222002150011533533573892115496e76616c6964206f75747075742076616c75652e0003115001103115335333573466e1c030d540208894cd400484d4038894cd4cc06000c008854cd4c0a400484004540cc540c8540bc0c40c05400454cd4cd5ce248115496e76616c6964206f75747075742073746174652e000301500110301533533301300e500135009223500222222222220071030133573892011e546865207072697a65206973206e6f74206265696e67206d696e7465642e0002f13500122335032335503400233503233550340014800940cd40cc54cd4ccd5cd19b8753333500710081337020109001099b800084800884024d540048894cd400484d4028894cd4cc05000c008854cd4c09400484004540a4540a0540940b40b040b44cd5ce248115496e76616c6964206f75747075742073746174652e0002c153353009005130204988854cd40044008884c0912615335333573466e1d4cccd401440184cdc080324004266e00019200221007355001222533500121350082253353301200300221533530230012100115029150281502502b02a102b133573892115496e76616c6964206f75747075742073746174652e0002a153353007003130204988854cd40044008884c09126153353006002130214988854cd40044008884c09526153353007001213500122350012220021356262350012235002222222222253353301000a00b2135001223500122233355301c12001223500222235008223500522325335335005233500425335333573466e3c0080041081045400c410481048cd4010810494cd4ccd5cd19b8f002001042041150031041133504100a0091009153350032153350022133500223350022335002233500223303200200120442335002204423303200200122204422233500420442225335333573466e1c01800c11c11854cd4ccd5cd19b8700500204704613302500400110461046103f153350012103f103f503800f132635335738921024c660002302222333573466e1c00800409008c8d400488d40088888888888cc03802802c88cccd40049407094070940708ccd54c0344800540148d4004894cd54cd4ccd5cd19b8f350022200235004220020260251333573466e1cd400888004d40108800409809440944d408000c5407c00c88d400488888888894cd4ccd54c0544800540348d4004894cd4ccd5cd19b8f00200f02e02d135028003150270022135026350012200115024133500e225335002210031001501722233355300a120013500f500e2350012233355300d1200135012501123500122333500123300a4800000488cc02c0080048cc028005200000133004002001223355300712001235001223355024002333500123355300b1200123500122335502800235500d0010012233355500801000200123355300b1200123500122335502800235500c00100133355500300b00200111122233355300412001501f335530071200123500122335502400235500900133355300412001223500222533533355300c1200132335013223335003220020020013500122001123300122533500210251001022235001223300a002005006100313350230040035020001335530071200123500122323355025003300100532001355025225335001135500a003221350022253353300c002008112223300200a004130060030023200135501e221122253350011002221330050023335530071200100500400111212223003004112122230010043200135501b22112253350011501d22133501e300400233553006120010040013200135501a22112225335001135006003221333500900530040023335530071200100500400112350012200112350012200222333573466e3c008004054050448cc004894cd40084004405004c48cd400888ccd400c88008008004d40048800448848cc00400c0088c8c8ccccccd5d200191999ab9a3370e6aae75400d2000233335573ea0064a01c46666aae7cd5d128021299a9919191999999aba400323333573466e1cd55cea801a400046666aae7d400c940548cccd55cf9aba250042533532333333357480024a0304a0304a03046a0326eb400894060044d5d0a802909a80c0008a80b1280b0078071280a0061280992809928099280980609aab9e5001137540026ae85401484d40440045403c9403c02001c94034014940309403094030940300144d55cf280089baa001498480048d58984d58988d58984d58988d589848488c00800c44880044d589888cdc0001000990009aa803911299a80088011109a8011119803999804001003000801990009aa8031111299a80088011109a80111299a999ab9a3370e00290000050048999804003803001899980400399a80589199800804001801003001891001091000889100109109119800802001889109198008018010891918008009119801980100100099a89119a8911980119aa80224411c725ba16e744abf2074c951c320fcc92ea0158ed7bb325b092a58245d00488100481508848cc00400c0088004448848cc00400c0084480041"

-------------------------------------------------------------------------------
-- Test Addresses
-------------------------------------------------------------------------------
simpleQueryAddress :: GYAddress
simpleQueryAddress = unsafeAddressFromText "addr_test1qp3fx29hm39xvyeer3v5xalcpmye7078vz09uylzuvxgqy0ma9xfzua4lag9wwgwk059k07f3kf46cjvx5ldmknqh7xqmg5pu4"

queryAddress1 :: GYAddress
queryAddress1 = unsafeAddressFromText "addr_test1qz4rmz7gvmtmgrygr72ahuh52zr42ejzlefnptgeex3g0pr9973t5mlxtywqarjaghv2x8vvpnkpyln0d263jqd4vhfs868tfp"

queryAddress2 :: GYAddress
queryAddress2 = unsafeAddressFromText "addr_test1qrp2pg3g6tk7umgqepeasxnzvcsuen879mrsmtm2czhaxgn9973t5mlxtywqarjaghv2x8vvpnkpyln0d263jqd4vhfskgun9e"

queryAddress3 :: GYAddress
queryAddress3 = unsafeAddressFromText "addr_test1qp04dl7vvvqlvveqd4gr23z726n5ly8srr0ecg375pfhjht9973t5mlxtywqarjaghv2x8vvpnkpyln0d263jqd4vhfs60knu3"

------------------------------------------------------------------------------
-- GoldenTests FilePaths
------------------------------------------------------------------------------

simpleAddressPath :: String
simpleAddressPath = "tests/mock-utxos/utxosAddrSimple.json"

complexAddressPath :: String
complexAddressPath = "tests/mock-utxos/utxosAddrComplex.json"

utxosAtAddressesPath :: String
utxosAtAddressesPath = "tests/mock-utxos/utxosAddresses.json"

utxoAtRefPath :: String
utxoAtRefPath = "tests/mock-utxos/utxoAtRef.json"

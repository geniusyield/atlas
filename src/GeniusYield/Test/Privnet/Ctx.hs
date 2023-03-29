{-|
Module      : GeniusYield.Test.Privnet.Ctx
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Test.Privnet.Ctx (
    -- * Context
    Ctx (..),
    -- * User
    User (..),
    UserIdx (..),
    userPkh,
    userVKey,
    -- * Operations
    ctxRunI,
    ctxRunC,
    ctxRunF,
    __ctxRunF,
    ctxCurrentSlot,
    ctxWaitNextSlot,
    ctxWaitUntilSlot,
    ctxProviders,
    ctxSlotConfig,
    submitTx,
    __submitTx,
    -- * Helpers
    ctxUser,
    ctxUserAddr,
    ctxUserPkh,
    newTempUserCtx,
    ctxQueryBalance,
    findOutput,
    addRefScriptCtx,
    addRefInputCtx,
) where

import           Test.Tasty.HUnit                     (assertFailure)

import qualified Cardano.Api                          as Api
import           Control.Concurrent                   (threadDelay)
import qualified Data.Map.Strict                      as Map

import           GeniusYield.Imports
import           GeniusYield.Providers.CardanoDbSync
import           GeniusYield.Providers.LiteChainIndex
import           GeniusYield.Providers.Node
import           GeniusYield.Transaction
import           GeniusYield.TxBuilder
import           GeniusYield.Types

import qualified GeniusYield.Examples.Limbo           as Limbo

data User = User
    { userSKey :: !GYPaymentSigningKey
    , userAddr :: !GYAddress
    , userColl :: !GYTxOutRef
    }

userVKey :: User -> GYPaymentVerificationKey
userVKey = paymentVerificationKey . userSKey

data UserIdx
    = User1  -- ^ user 1 is used as /geniusyield/ user
    | User2
    | User3

userPkh :: User -> GYPubKeyHash
userPkh = pubKeyHash . paymentVerificationKey . userSKey

data Ctx = Ctx
    { ctxEra         :: !GYEra
    , ctxInfo        :: !(Api.LocalNodeConnectInfo Api.CardanoMode)
    , ctxLCI         :: !LCIClient
    , ctxDbSync      :: !(Maybe CardanoDbSyncConn)
    , ctxUser1       :: !User
    , ctxUser2       :: !User
    , ctxUser3       :: !User
    , ctxGold        :: !GYAssetClass  -- ^ asset used in tests
    , ctxIron        :: !GYAssetClass  -- ^ asset used in tests
    , ctxLog         :: !GYLog
    , ctxLookupDatum :: !GYLookupDatum
    , ctxQueryUtxos  :: !GYQueryUTxO
    , ctxGetParams   :: !GYGetParameters
    }

ctxUser :: Ctx -> UserIdx -> User
ctxUser ctx User1 = ctxUser1 ctx
ctxUser ctx User2 = ctxUser2 ctx
ctxUser ctx User3 = ctxUser3 ctx

ctxUserAddr :: Ctx -> UserIdx -> GYAddress
ctxUserAddr ctx ui = userAddr (ctxUser ctx ui)

ctxUserPkh :: Ctx -> UserIdx -> GYPubKeyHash
ctxUserPkh ctx ui = userPkh (ctxUser ctx ui)

-- | Creates a new user with the given balance. Note that we'll deduct 5 ada from the given fund as a collateral output.
newTempUserCtx:: Ctx
          -> UserIdx         -- ^ User which will fund this new user.
          -> GYValue         -- ^ Describes balance of new user.
          -> IO User
newTempUserCtx ctx fundUser fundValue = do
  newSKey <- generatePaymentSigningKey
  let newVKey = paymentVerificationKey newSKey
      newKeyHash = pubKeyHash newVKey
      newAddr = addressFromPubKeyHash GYPrivnet newKeyHash
      (adaInValue, otherValue) = valueSplitAda fundValue
      collateralLovelace = 5_000_000
      collateralValue = valueFromLovelace collateralLovelace
  -- Our balancer would add minimum ada required for other utxo in case of equality
  if adaInValue < collateralLovelace then fail "Given value for new user has less than 5 ada"
  else do
    txBody <- ctxRunI ctx fundUser $ return (
      mustHaveOutput (mkGYTxOutNoDatum newAddr (otherValue <> (valueFromLovelace adaInValue `valueMinus` collateralValue))) <>
      mustHaveOutput (mkGYTxOutNoDatum newAddr collateralValue)
      )
    void $ submitTx ctx fundUser txBody
    -- wait a tiny bit.
    threadDelay 1_000_000
    utxos <- ctxRunC ctx fundUser $ utxosAtAddress newAddr
    -- I have kept the following logic a bit general for now so that changes above don't affect the following logic.
    let adaOnlyUtxos = utxosToList $ filterUTxOs (\GYUTxO {utxoValue} -> valueTotalAssets utxoValue == 1 && fst (valueSplitAda utxoValue) >= 5_000_000) utxos
    if null adaOnlyUtxos then fail "New user doesn't have an ada only UTxO with value geq 5 ada"
    else
      let _collateralUtxo@GYUTxO {utxoRef = collRef} = minimumBy (compare `on` (\GYUTxO {utxoValue} -> fst (valueSplitAda utxoValue))) adaOnlyUtxos
      in return $ User {userSKey = newSKey, userAddr = newAddr, userColl = collRef}

ctxRunF :: forall t v. Traversable t => Ctx -> UserIdx -> GYTxMonadNode (t (GYTxSkeleton v)) -> IO (t GYTxBody)
ctxRunF ctx u = __ctxRunF ctx (ctxUser ctx u)

-- To run against any arbitrary 'User', not necessarily belonging to 'Ctx'.
__ctxRunF :: forall t v. Traversable t => Ctx -> User -> GYTxMonadNode (t (GYTxSkeleton v)) -> IO (t GYTxBody)
__ctxRunF ctx User {..} =  runGYTxMonadNodeF GYRandomImproveMultiAsset GYPrivnet (ctxProviders ctx) [userAddr] userAddr userColl

ctxRunC :: forall a. Ctx -> UserIdx -> GYTxMonadNode a -> IO a
ctxRunC = coerce (ctxRunF @(Const a))

ctxRunI :: Ctx -> UserIdx -> GYTxMonadNode (GYTxSkeleton v) -> IO GYTxBody
ctxRunI = coerce (ctxRunF @Identity)

ctxCurrentSlot :: Ctx -> IO GYSlot
ctxCurrentSlot (ctxProviders -> providers) =
    gyGetCurrentSlot providers

ctxWaitNextSlot :: Ctx -> IO ()
ctxWaitNextSlot ctx@(ctxProviders -> providers) = do
    slot <- gyWaitForNextBlock providers
    _ <- lciWaitUntilSlot (ctxLCI ctx) slot
    forM_ (ctxDbSync ctx) $ \dbSync -> dbSyncWaitUntilSlot dbSync slot

ctxWaitUntilSlot :: Ctx -> GYSlot -> IO ()
ctxWaitUntilSlot ctx@(ctxProviders -> providers) slot = do
    slot' <- gyWaitUntilSlot providers slot
    _ <- lciWaitUntilSlot (ctxLCI ctx) slot'
    forM_ (ctxDbSync ctx) $ \dbSync -> dbSyncWaitUntilSlot dbSync slot

ctxSlotConfig :: Ctx -> IO GYSlotConfig
ctxSlotConfig (ctxProviders -> providers) = gyGetSlotConfig providers

ctxQueryBalance :: Ctx -> UserIdx -> IO GYValue
ctxQueryBalance ctx u = ctxRunC ctx u $ do
    queryBalance $ ctxUserAddr ctx u

ctxProviders :: Ctx -> GYProviders
ctxProviders ctx = GYProviders
    { gyLookupDatum    = ctxLookupDatum ctx
    , gySubmitTx       = nodeSubmitTx (ctxInfo ctx)
    , gySlotActions    = nodeSlotActions (ctxInfo ctx)
    , gyGetParameters  = ctxGetParams ctx
    , gyQueryUTxO      = ctxQueryUtxos ctx
    , gyLog'           = ctxLog ctx
    }

submitTx :: Ctx -> UserIdx -> GYTxBody -> IO GYTxId
submitTx ctx u = __submitTx ctx (ctxUser ctx u)

-- Variant of 'submitTx' to run against any arbitrary 'User', not necessarily belonging to Ctx.
__submitTx :: Ctx -> User -> GYTxBody -> IO GYTxId
__submitTx ctx@Ctx { ctxInfo } User {..} txBody = do
    let tx = signTx txBody [userSKey]
    -- when optsPrintTxBodies $ printf "Transaction %s\n" (ppShow txBody')
    txId <- nodeSubmitTx ctxInfo tx
    -- printf "Submitted transaction %s\n" (show txId)

    ctxWaitNextSlot ctx
    return txId

-- | Function to find for the first locked output in the given `GYTxBody` at the given `GYAddress`.
findOutput :: GYAddress -> GYTxBody -> IO GYTxOutRef
findOutput addr txBody = do
    let utxos = txBodyUTxOs txBody
    maybe (assertFailure "expecting an order in utxos") return $
        findFirst (\utxo -> if utxoAddress utxo == addr then Just (utxoRef utxo) else Nothing) $ utxosToList utxos

-- | Function to add for a reference script. It adds the script in so called "Always failing" validator so that it won't be later possible to spend this output. There is a slight optimisation here in that if the desired reference script already exists then we don't add another one and return the reference for the found one else, we create a new one.
addRefScriptCtx :: Ctx                 -- ^ Given context.
                -> UserIdx             -- ^ User which will execute the transaction (if required).
                -> GYScript 'PlutusV2  -- ^ Given script.
                -> IO GYTxOutRef       -- ^ Returns the reference for the desired output.
addRefScriptCtx ctx user script = do
  txBodyRefScript <- ctxRunF ctx user $ Limbo.addRefScript script
  case txBodyRefScript of
    Left ref -> return ref
    Right body -> do
      let refs = Limbo.findRefScriptsInBody body
      ref <- case Map.lookup (Some script) refs of
        Just ref -> return ref
        Nothing  -> fail "Shouldn't happen: no ref in body"
      void $ submitTx ctx user body
      return ref

-- | Function to add for a reference input.
addRefInputCtx :: Ctx            -- ^ Given context.
               -> UserIdx        -- ^ User which will execute this transaction.
               -> Bool           -- ^ Whether to inline the datum.
               -> GYAddress      -- ^ Address to put this output at.
               -> GYDatum        -- ^ The datum to put.
               -> IO GYTxOutRef  -- ^ Returns the reference for the required output.
addRefInputCtx ctx user toInline addr ourDatum = do
  txBody <- ctxRunI ctx user $ return $ mustHaveOutput (GYTxOut addr mempty (Just (ourDatum, if toInline then GYTxOutUseInlineDatum else GYTxOutDontUseInlineDatum)) Nothing)
  let utxos = utxosToList $ txBodyUTxOs txBody
      ourDatumHash = hashDatum ourDatum
      mRefInputUtxo = find (\utxo ->
        case utxoOutDatum utxo of
          GYOutDatumHash dh  -> ourDatumHash == dh
          GYOutDatumInline d -> ourDatum == d
          GYOutDatumNone     -> False
        ) utxos
  case mRefInputUtxo of
    Nothing               -> fail "Shouldn't happen: Couldn't find desired UTxO in tx outputs"
    Just GYUTxO {utxoRef} -> do
      void $ submitTx ctx user txBody
      return utxoRef

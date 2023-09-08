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
    ctxUsers,
    userPkh,
    userVKey,
    -- * Operations
    ctxRunI,
    ctxRunC,
    ctxRunF,
    ctxRunFWithCollateral,
    ctxSlotOfCurrentBlock,
    ctxWaitNextSlot,
    ctxWaitUntilSlot,
    ctxProviders,
    ctxSlotConfig,
    submitTx,
    -- * Helpers
    newTempUserCtx,
    ctxQueryBalance,
    findOutput,
    addRefScriptCtx,
    addRefInputCtx,
) where

import           Test.Tasty.HUnit                     (assertFailure)

import qualified Cardano.Api                          as Api
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
    }

userVKey :: User -> GYPaymentVerificationKey
userVKey = paymentVerificationKey . userSKey

userPkh :: User -> GYPubKeyHash
userPkh = pubKeyHash . paymentVerificationKey . userSKey

data Ctx = Ctx
    { ctxEra              :: !GYEra
    , ctxInfo             :: !(Api.LocalNodeConnectInfo Api.CardanoMode)
    , ctxLCI              :: !LCIClient
    , ctxDbSync           :: !(Maybe CardanoDbSyncConn)
    , ctxUserF            :: !User  -- ^ Funder. All other users begin with same status of funds.
    , ctxUser2            :: !User
    , ctxUser3            :: !User
    , ctxUser4            :: !User
    , ctxUser5            :: !User
    , ctxUser6            :: !User
    , ctxUser7            :: !User
    , ctxUser8            :: !User
    , ctxUser9            :: !User
    , ctxGold             :: !GYAssetClass  -- ^ asset used in tests
    , ctxIron             :: !GYAssetClass  -- ^ asset used in tests
    , ctxLog              :: !GYLog
    , ctxLookupDatum      :: !GYLookupDatum
    , ctxAwaitTxConfirmed :: !GYAwaitTx
    , ctxQueryUtxos       :: !GYQueryUTxO
    , ctxGetParams        :: !GYGetParameters
    }

-- | List of context sibling users - all of which begin with same balance.
ctxUsers :: Ctx -> [User]
ctxUsers ctx = ($ ctx) <$> [ctxUser2, ctxUser3, ctxUser4, ctxUser5, ctxUser6, ctxUser7, ctxUser8, ctxUser9]

-- | Creates a new user with the given balance. Note that the actual balance which this user get's could be more than what is provided to satisfy minimum ada requirement of a UTxO.
newTempUserCtx:: Ctx
              -> User            -- ^ User which will fund this new user.
              -> GYValue         -- ^ Describes balance of new user.
              -> Bool            -- ^ Create collateral output of 5 ada?
              -> IO User
newTempUserCtx ctx fundUser fundValue createCollateral = do
  newSKey <- generatePaymentSigningKey
  let newVKey = paymentVerificationKey newSKey
      newKeyHash = pubKeyHash newVKey
      newAddr = addressFromPubKeyHash GYPrivnet newKeyHash
      (adaInValue, otherValue) = valueSplitAda fundValue

  -- We want this new user to have at least 5 ada if we want to create collateral.
  -- Our balancer would add minimum ada required for other utxo in case of equality
  when (createCollateral && adaInValue < collateralLovelace) $ fail "Given value for new user has less than 5 ada"

  txBody <- ctxRunI ctx fundUser $ return $
    if createCollateral then
      mustHaveOutput (mkGYTxOutNoDatum newAddr (otherValue <> (valueFromLovelace adaInValue `valueMinus` collateralValue))) <>
      mustHaveOutput (mkGYTxOutNoDatum newAddr collateralValue)
    else
      mustHaveOutput (mkGYTxOutNoDatum newAddr fundValue)

  void $ submitTx ctx fundUser txBody
  return $ User {userSKey = newSKey, userAddr = newAddr}


ctxRunF :: forall t v. Traversable t => Ctx -> User -> GYTxMonadNode (t (GYTxSkeleton v)) -> IO (t GYTxBody)
ctxRunF ctx User {..} =  runGYTxMonadNodeF GYRandomImproveMultiAsset GYPrivnet (ctxProviders ctx) [userAddr] userAddr Nothing

-- | Variant of `ctxRunF` where caller can also give the UTxO to be used as collateral.
ctxRunFWithCollateral :: forall t v. Traversable t
                      => Ctx
                      -> User
                      -> GYTxOutRef  -- ^ Reference to UTxO to be used as collateral.
                      -> Bool        -- ^ To check whether this given collateral UTxO has value of exact 5 ada? If it doesn't have exact 5 ada, it would be ignored.
                      -> GYTxMonadNode (t (GYTxSkeleton v))
                      -> IO (t GYTxBody)
ctxRunFWithCollateral ctx User {..} coll toCheck5Ada =  runGYTxMonadNodeF GYRandomImproveMultiAsset GYPrivnet (ctxProviders ctx) [userAddr] userAddr $ Just (coll, toCheck5Ada)

ctxRunC :: forall a. Ctx -> User -> GYTxMonadNode a -> IO a
ctxRunC = coerce (ctxRunF @(Const a))

ctxRunI :: Ctx -> User -> GYTxMonadNode (GYTxSkeleton v) -> IO GYTxBody
ctxRunI = coerce (ctxRunF @Identity)

ctxSlotOfCurrentBlock :: Ctx -> IO GYSlot
ctxSlotOfCurrentBlock (ctxProviders -> providers) =
    gyGetSlotOfCurrentBlock providers

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

ctxQueryBalance :: Ctx -> User -> IO GYValue
ctxQueryBalance ctx u = ctxRunC ctx u $ do
    queryBalance $ userAddr u

ctxProviders :: Ctx -> GYProviders
ctxProviders ctx = GYProviders
    { gyLookupDatum      = ctxLookupDatum ctx
    , gySubmitTx         = nodeSubmitTx (ctxInfo ctx)
    , gyAwaitTxConfirmed = ctxAwaitTxConfirmed ctx
    , gySlotActions      = nodeSlotActions (ctxInfo ctx)
    , gyGetParameters    = ctxGetParams ctx
    , gyQueryUTxO        = ctxQueryUtxos ctx
    , gyLog'             = ctxLog ctx
    }

submitTx :: Ctx -> User -> GYTxBody -> IO GYTxId
submitTx ctx@Ctx { ctxInfo } User {..} txBody = do
    let tx = signGYTxBody txBody [userSKey]
    -- when optsPrintTxBodies $ printf "Transaction %s\n" (ppShow txBody')
    txId <- nodeSubmitTx ctxInfo tx
    -- printf "Submitted transaction %s\n" (show txId)

    gyAwaitTxConfirmed (ctxProviders ctx) (GYAwaitTxParameters { maxAttempts = 30, checkInterval = 1_000_000, confirmations = 1 }) txId
    return txId

-- | Function to find for the first locked output in the given `GYTxBody` at the given `GYAddress`.
findOutput :: GYAddress -> GYTxBody -> IO GYTxOutRef
findOutput addr txBody = do
    let utxos = txBodyUTxOs txBody
    maybe (assertFailure "expecting an order in utxos") return $
        findFirst (\utxo -> if utxoAddress utxo == addr then Just (utxoRef utxo) else Nothing) $ utxosToList utxos

-- | Function to add for a reference script. It adds the script in so called "Always failing" validator so that it won't be later possible to spend this output. There is a slight optimisation here in that if the desired reference script already exists then we don't add another one and return the reference for the found one else, we create a new one.
addRefScriptCtx :: Ctx                 -- ^ Given context.
                -> User                -- ^ User which will execute the transaction (if required).
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
               -> User           -- ^ User which will execute this transaction.
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

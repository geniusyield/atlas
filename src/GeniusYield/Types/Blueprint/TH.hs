{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : GeniusYield.Types.Blueprint.TH
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Blueprint.TH (makeBlueprintTypes) where

import Data.List (foldl')
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import GeniusYield.Imports (Generic)
import GeniusYield.ReadJSON (readJSON)
import GeniusYield.Types.Blueprint.Contract
import GeniusYield.Types.Blueprint.DefinitionId (DefinitionId, mkDefinitionId, unDefinitionId)
import GeniusYield.Types.Blueprint.Schema
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import PlutusLedgerApi.Common qualified as Plutus
import PlutusTx qualified

-- | Get the name of the current module.
moduleName :: String
moduleName = $(location >>= stringE . loc_module)

-- | In case there is a valid use for an unsupported case.
createIssue :: String
createIssue = "Please raise an issue if you think it's a valid use case o/w."

-- | Sanitize type names to be valid.
sanitize :: Text.Text -> Text.Text
sanitize = Text.map (\c -> if c `elem` supportedChars then c else '_')
 where
  -- TODO: Are these exhaustive?
  supportedChars = '_' : ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9']

-- | Generate `Name` from `DefinitionId`.
genTyconName :: DefinitionId -> Name
genTyconName defId = mkName $ Text.unpack $ "Blueprint" <> sanitize (unDefinitionId defId)

-- | Some stock derivations for our types.
stockDerivations :: [DerivClause]
stockDerivations = [DerivClause (Just StockStrategy) [ConT ''Eq, ConT ''Show, ConT ''Ord, ConT ''Generic]]

-- | Declare associated types for a given `DefinitionId` and `Schema`.
decTypes :: DefinitionId -> Schema -> Q [Dec]
decTypes defId = \case
  SchemaInteger _schemaInfo _integerSchema ->
    pure [NewtypeD [] tyconName [] Nothing (NormalC tyconName [(Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Integer)]) stockDerivations]
  SchemaBytes _schemaInfo _bytesSchema ->
    pure [NewtypeD [] tyconName [] Nothing (NormalC tyconName [(Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Plutus.BuiltinByteString)]) stockDerivations]
  SchemaList _schemaInfo MkListSchema {..} ->
    case lsItems of
      ListItemSchemaSchema s -> case s of
        SchemaDefinitionRef itemDefId -> pure [NewtypeD [] tyconName [] Nothing (NormalC tyconName [(Bang NoSourceUnpackedness NoSourceStrictness, AppT ListT (ConT (genTyconName itemDefId)))]) stockDerivations]
        _anyOther ->
          -- TODO: Should I do something to avoid their being name conflicts in case there is already a type with name ..._Item?
          let itemDefId = mkDefinitionId $ unDefinitionId defId <> "_Item"
           in pure [NewtypeD [] tyconName [] Nothing (NormalC tyconName [(Bang NoSourceUnpackedness NoSourceStrictness, AppT ListT (ConT (genTyconName itemDefId)))]) stockDerivations] <> decTypes itemDefId s
      ListItemSchemaSchemas _ -> error $ moduleName <> ": items as a list of schemas is unsupported. " <> createIssue
  SchemaMap _schemaInfo MkMapSchema {..} ->
    let keyDefId = mkDefinitionId $ unDefinitionId defId <> "_Key"
        valDefId = mkDefinitionId $ unDefinitionId defId <> "_Value"
     in pure [NewtypeD [] tyconName [] Nothing (NormalC tyconName [(Bang NoSourceUnpackedness NoSourceStrictness, AppT (AppT (ConT ''Map.Map) (ConT (genTyconName keyDefId))) (ConT (genTyconName valDefId)))]) stockDerivations] <> decTypes keyDefId msKeys <> decTypes valDefId msValues -- TODO: Possibly make optimisation here, to not use newtypes? Not really clear, perhaps should get rid of newtypes entirely.
  SchemaConstructor _schemaInfo MkConstructorSchema {..} ->
    if csFields == mempty
      then error (moduleName <> ": top level \"constructor\" fields must not be empty. " <> createIssue)
      else do
        pure [DataD [] tyconName [] Nothing [NormalC tyconName (reverse $ foldl' getFieldRefs [] csFields)] stockDerivations]
  -- TODO: Define as a type synonym for defId?
  SchemaBuiltInData _ -> pure [NewtypeD [] tyconName [] Nothing (NormalC tyconName [(Bang NoSourceUnpackedness NoSourceStrictness, ConT ''PlutusTx.BuiltinData)]) stockDerivations]
  SchemaBuiltInUnit _ -> error $ moduleName <> ": \"#unit\" " <> avoidBuiltin
  SchemaBuiltInBoolean _ -> error $ moduleName <> ": \"#boolean\" " <> avoidBuiltin
  SchemaBuiltInInteger _ -> error $ moduleName <> ": \"#integer\" " <> avoidBuiltin
  SchemaBuiltInBytes _ -> error $ moduleName <> ": \"#bytes\" " <> avoidBuiltin
  SchemaBuiltInString _ -> error $ moduleName <> ": \"#string\" " <> avoidBuiltin
  SchemaBuiltInPair _ _ -> error $ moduleName <> ": \"#pair\" " <> avoidBuiltin
  SchemaBuiltInList _ _ -> error $ moduleName <> ": \"#list\" " <> avoidBuiltin
  SchemaOneOf ss ->
    -- TODO: To enforce strictness for fields?
    -- TODO: To enforce that indices are all unique?
    let compareConstructors (SchemaConstructor _ a) (SchemaConstructor _ b) = compare (csIndex a) (csIndex b)
        compareConstructors _ _ = error $ moduleName <> ": schemas inside \"oneOf\" must be all of dataType \"constructor\". " <> createIssue
        ssSorted = NE.sortBy compareConstructors ss
        f acc s = case s of
          SchemaConstructor _schemaInfo MkConstructorSchema {..} ->
            let constructorName = genTyconName $ mkDefinitionId $ unDefinitionId defId <> Text.pack (show csIndex)
             in NormalC constructorName (reverse $ foldl' getFieldRefs [] csFields) : acc
          _anyOther -> error $ moduleName <> ": absurd case encountered when handling \"oneOf\"."
     in pure [DataD [] tyconName [] Nothing (reverse $ foldl' f [] ssSorted) stockDerivations]
  SchemaAnyOf _ -> error $ moduleName <> ": \"anyOf\" is not supported as type is ambiguous."
  SchemaAllOf _ -> error $ moduleName <> ": \"allOf\" is not supported as type is ambiguous."
  SchemaNot _ -> error $ moduleName <> ": \"not\" is not supported as type is ambiguous."
  -- TODO: To use newtype here instead?
  SchemaDefinitionRef r -> pure [TySynD tyconName [] (ConT (genTyconName r))]
 where
  avoidBuiltin = "is a built-in type which are not supported."
  tyconName = genTyconName defId
  getFieldRefs racc (SchemaDefinitionRef d) = (Bang NoSourceUnpackedness NoSourceStrictness, ConT (genTyconName d)) : racc
  getFieldRefs _racc _ = error $ moduleName <> ": \"constructor\" fields must be all of type \"$ref\". " <> createIssue

makeBlueprintTypes :: FilePath -> Q [Dec]
makeBlueprintTypes fp = do
  bp :: ContractBlueprint <- runIO (readJSON fp)
  addDependentFile fp
  -- TODO: As a first step, I should get all the definitions (including those in validator if not via reference), and generate types for them all.
  -- TODO: Ok to prefix these with Blueprint?
  Map.foldlWithKey'
    (\acc defId schema -> acc <> decTypes defId schema)
    (pure [])
    (contractDefinitions bp)

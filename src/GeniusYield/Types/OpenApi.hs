{-# OPTIONS_GHC -Wno-orphans #-}

module GeniusYield.Types.OpenApi where

import           Control.Lens          ((&), (.~), (^.))
import           Data.Data             (Typeable)
import           Data.OpenApi          (OpenApiType (..))
import qualified Data.OpenApi          as OpenApi
import qualified Data.OpenApi.Declare  as OpenApi
import qualified Data.Swagger          as Swagger
import qualified Data.Swagger.Declare  as Swagger
import qualified Data.Swagger.Internal as Swagger

-- | Lift a @Swagger.Schema@ to an @OpenApi.Schema@.
liftSwaggerSchema :: Swagger.Schema -> OpenApi.Schema
liftSwaggerSchema swaggerSchema =
  mempty
  & OpenApi.title .~ swaggerSchema ^. Swagger.title
  & OpenApi.description .~ swaggerSchema ^. Swagger.description
  & OpenApi.required .~ swaggerSchema ^. Swagger.required
  & OpenApi.allOf .~ (fmap convertSwaggerReferencedSchema <$> swaggerSchema ^. Swagger.allOf)
  & OpenApi.properties .~ (convertSwaggerReferencedSchema <$> swaggerSchema ^. Swagger.properties)
  & OpenApi.additionalProperties .~ (convertSwaggerAdditionalProperties <$> swaggerSchema ^. Swagger.additionalProperties)
  & OpenApi.readOnly .~ swaggerSchema ^. Swagger.readOnly
  & OpenApi.example .~ swaggerSchema ^. Swagger.example
  & OpenApi.maxProperties .~ swaggerSchema ^. Swagger.maxProperties
  & OpenApi.minProperties .~ swaggerSchema ^. Swagger.minProperties
  & OpenApi.default_ .~ swaggerSchema ^. Swagger.default_
  & OpenApi.type_ .~ (convertSwaggerType <$> swaggerSchema ^. Swagger.type_)
  & OpenApi.format .~ swaggerSchema ^. Swagger.format
  & OpenApi.pattern .~ swaggerSchema ^. Swagger.pattern
  & OpenApi.maximum_ .~ swaggerSchema ^. Swagger.maximum_
  & OpenApi.exclusiveMaximum .~ swaggerSchema ^. Swagger.exclusiveMaximum
  & OpenApi.minimum_ .~ swaggerSchema ^. Swagger.minimum_
  & OpenApi.exclusiveMinimum .~ swaggerSchema ^. Swagger.exclusiveMinimum
  & OpenApi.maxLength .~ swaggerSchema ^. Swagger.maxLength
  & OpenApi.minLength .~ swaggerSchema ^. Swagger.minLength
  & OpenApi.pattern .~ swaggerSchema ^. Swagger.pattern
  & OpenApi.maxItems .~ swaggerSchema ^. Swagger.maxItems
  & OpenApi.minItems .~ swaggerSchema ^. Swagger.minItems
  & OpenApi.uniqueItems .~ swaggerSchema ^. Swagger.uniqueItems
  & OpenApi.enum_ .~ swaggerSchema ^. Swagger.enum_
  & OpenApi.multipleOf .~ swaggerSchema ^. Swagger.multipleOf
  where
    convertSwaggerReferencedSchema :: Swagger.Referenced Swagger.Schema -> OpenApi.Referenced OpenApi.Schema
    convertSwaggerReferencedSchema (Swagger.Inline s) = OpenApi.Inline (liftSwaggerSchema s)
    convertSwaggerReferencedSchema (Swagger.Ref r) = OpenApi.Ref (convertSwaggerRef r)

    convertSwaggerRef :: Swagger.Reference -> OpenApi.Reference
    convertSwaggerRef (Swagger.Reference ref) = OpenApi.Reference ref

    convertSwaggerType :: Swagger.SwaggerType 'Swagger.SwaggerKindSchema -> OpenApiType
    convertSwaggerType Swagger.SwaggerString  = OpenApiString
    convertSwaggerType Swagger.SwaggerNumber  = OpenApiNumber
    convertSwaggerType Swagger.SwaggerInteger = OpenApiInteger
    convertSwaggerType Swagger.SwaggerBoolean = OpenApiBoolean
    convertSwaggerType Swagger.SwaggerArray   = OpenApiArray
    convertSwaggerType Swagger.SwaggerNull    = OpenApiNull
    convertSwaggerType Swagger.SwaggerObject  = OpenApiObject

    convertSwaggerAdditionalProperties :: Swagger.AdditionalProperties -> OpenApi.AdditionalProperties
    convertSwaggerAdditionalProperties (Swagger.AdditionalPropertiesAllowed b) = OpenApi.AdditionalPropertiesAllowed b
    convertSwaggerAdditionalProperties (Swagger.AdditionalPropertiesSchema s) = OpenApi.AdditionalPropertiesSchema (convertSwaggerReferencedSchema s)

-- | Convert a @Swagger.NamedSchema@ to an @OpenApi.NamedSchema@.
convertNamedSchema :: Swagger.NamedSchema -> OpenApi.NamedSchema
convertNamedSchema (Swagger.NamedSchema name swaggerSchema) =
  OpenApi.NamedSchema name (liftSwaggerSchema swaggerSchema)

liftSwaggerDec :: Swagger.Declare (Swagger.Definitions Swagger.Schema) Swagger.NamedSchema -> OpenApi.Declare (OpenApi.Definitions OpenApi.Schema) OpenApi.NamedSchema
liftSwaggerDec swaggerDeclare =
  let (swaggerSchemas, swaggerNamedSchema) = Swagger.runDeclare swaggerDeclare mempty
      openApiNamedSchema = convertNamedSchema swaggerNamedSchema
      openApiSchemas = liftSwaggerSchema <$> swaggerSchemas
  in OpenApi.DeclareT $ \_ -> pure (openApiSchemas, openApiNamedSchema)

instance {-# OVERLAPPABLE #-} (Swagger.ToSchema a, Typeable a) => OpenApi.ToSchema a where
  declareNamedSchema p = liftSwaggerDec (Swagger.declareNamedSchema p)

instance {-# OVERLAPPABLE #-} (Swagger.ToParamSchema a, Swagger.ToSchema a) => OpenApi.ToParamSchema a where
  toParamSchema p = liftSwaggerSchema $ Swagger.toSchema p

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module ConfigSchema (
    ConfigField (..),
    FieldKind (..),
    codecFields,
    providerNameCodec,
    multilineTextCodec,
    decodeToml,
    renderToml,
) where

import Autodocodec (Codec (..), JSONCodec, ObjectCodec, ValueCodec, named, textCodec, toJSONVia)
import Data.Aeson (ToJSON (..), Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List (elemIndex, find)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Scientific as Scientific
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as Vector
import qualified Toml
import Toml.Pretty (prettyTomlOrdered)
import qualified Toml.Semantics as TomlValue

data ConfigField = ConfigField
    { fieldKey :: Text
    , fieldDoc :: Maybe Text
    , fieldRequired :: Bool
    , fieldDefault :: Maybe Value
    , fieldKind :: FieldKind
    }

data FieldKind
    = TextKind
    | MultilineKind
    | ProviderKind
    | IntegerKind
    | NumberKind
    | BoolKind
    | EnumKind [Text]
    | ListKind
    | MapKind
    | TableKind [ConfigField]
    | TablesKind [ConfigField]

instance ToJSON ConfigField where
    toJSON field =
        object $
            [ "key" .= fieldKey field
            , "doc" .= fieldDoc field
            , "required" .= fieldRequired field
            , "default" .= fieldDefault field
            ]
                ++ kindPairs (fieldKind field)
      where
        kindPairs kind = case kind of
            TextKind -> named_ "text"
            MultilineKind -> named_ "multiline"
            ProviderKind -> named_ "provider"
            IntegerKind -> named_ "integer"
            NumberKind -> named_ "number"
            BoolKind -> named_ "bool"
            EnumKind values -> named_ "enum" ++ ["values" .= values]
            ListKind -> named_ "list"
            MapKind -> named_ "map"
            TableKind fields -> named_ "table" ++ ["fields" .= fields]
            TablesKind fields -> named_ "tables" ++ ["fields" .= fields]
        named_ name = ["kind" .= (name :: Text)]

providerNameCodec :: JSONCodec Text
providerNameCodec = named "provider" textCodec

multilineTextCodec :: JSONCodec Text
multilineTextCodec = named "multiline" textCodec

codecFields :: ValueCodec input output -> [ConfigField]
codecFields codec = case kindOf codec of
    TableKind fields -> fields
    _ -> []

kindOf :: ValueCodec input output -> FieldKind
kindOf codec = case codec of
    BoolCodec _ -> BoolKind
    StringCodec _ -> TextKind
    IntegerCodec _ _ -> IntegerKind
    NumberCodec _ _ -> NumberKind
    ArrayOfCodec _ item -> case kindOf item of
        TableKind fields -> TablesKind fields
        _ -> ListKind
    MapCodec _ -> MapKind
    HashMapCodec _ -> MapKind
    ObjectOfCodec _ fields -> TableKind (objectFields fields)
    BimapCodec _ _ inner -> kindOf inner
    CommentCodec _ inner -> kindOf inner
    ReferenceCodec "provider" _ -> ProviderKind
    ReferenceCodec "multiline" _ -> MultilineKind
    ReferenceCodec _ inner -> kindOf inner
    EqCodec{} -> maybe TextKind EnumKind (enumValues codec)
    EitherCodec{} -> maybe TextKind EnumKind (enumValues codec)
    _ -> TextKind

enumValues :: Codec context input output -> Maybe [Text]
enumValues codec = case codec of
    EitherCodec _ left right -> (<>) <$> enumValues left <*> enumValues right
    BimapCodec _ _ inner -> enumValues inner
    EqCodec value valueCodec -> case toJSONVia valueCodec value of
        String text -> Just [text]
        _ -> Nothing
    _ -> Nothing

objectFields :: ObjectCodec input output -> [ConfigField]
objectFields codec = case codec of
    RequiredKeyCodec key value doc -> [ConfigField key doc True Nothing (kindOf value)]
    OptionalKeyCodec key value doc -> [ConfigField key doc False Nothing (kindOf value)]
    OptionalKeyWithDefaultCodec key value defaultValue doc -> [ConfigField key doc False (Just (toJSONVia value defaultValue)) (kindOf value)]
    OptionalKeyWithOmittedDefaultCodec key value defaultValue doc -> [ConfigField key doc False (Just (toJSONVia value defaultValue)) (kindOf value)]
    BimapCodec _ _ inner -> objectFields inner
    ApCodec function argument -> objectFields function ++ objectFields argument
    _ -> []

decodeToml :: Text -> Either String Value
decodeToml = fmap (tableJson . TomlValue.forgetTableAnns) . Toml.parse

tableJson :: TomlValue.Table -> Value
tableJson (TomlValue.MkTable entries) = Object (KeyMap.fromList [(Key.fromText key, valueJson value) | (key, (_, value)) <- Map.toList entries])

valueJson :: TomlValue.Value -> Value
valueJson value = case value of
    TomlValue.Integer integer -> Number (fromInteger integer)
    TomlValue.Double double
        | isNaN double || isInfinite double -> Null
        | otherwise -> Number (Scientific.fromFloatDigits double)
    TomlValue.Text text -> String text
    TomlValue.Bool bool -> Bool bool
    TomlValue.List values -> Array (Vector.fromList (map valueJson values))
    TomlValue.Table table -> tableJson table
    _ -> Null

renderToml :: [ConfigField] -> Value -> Text
renderToml fields json = T.pack (show (prettyTomlOrdered (keyOrder fields) (objectTable json))) <> "\n"

objectTable :: Value -> TomlValue.Table
objectTable json = TomlValue.MkTable $ case json of
    Object entries -> Map.fromList [(Key.toText key, ((), value)) | (key, Just value) <- map (fmap jsonValue) (KeyMap.toList entries)]
    _ -> Map.empty

jsonValue :: Value -> Maybe TomlValue.Value
jsonValue json = case json of
    Number number -> Just (either TomlValue.Double TomlValue.Integer (Scientific.floatingOrInteger number))
    String text -> Just (TomlValue.Text text)
    Bool bool -> Just (TomlValue.Bool bool)
    Array values -> Just (TomlValue.List (mapMaybe jsonValue (Vector.toList values)))
    Object _ -> Just (TomlValue.Table (objectTable json))
    Null -> Nothing

keyOrder :: [ConfigField] -> [Text] -> Text -> Int
keyOrder fields path key = case path of
    [] -> fromMaybe maxBound (elemIndex key (map fieldKey fields))
    section : rest -> case fieldKind <$> find ((== section) . fieldKey) fields of
        Just (TableKind children) -> keyOrder children rest key
        Just (TablesKind children) -> keyOrder children rest key
        _ -> maxBound

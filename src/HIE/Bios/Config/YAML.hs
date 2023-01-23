{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE LambdaCase #-}

-- | Datatypes for parsing @hie.yaml@ files
module HIE.Bios.Config.YAML
  ( CradleConfigYAML(..)
  , CradleComponent(..)
  , MultiSubComponent(..)
  , CabalConfig(..)
  , CabalComponent(..)
  , StackConfig(..)
  , StackComponent(..)
  , DirectConfig(..)
  , BiosConfig(..)
  , NoneConfig(..)
  , OtherConfig(..)
  , OneOrManyComponents(..)
  , Callable(..)
  ) where

import           Control.Applicative ((<|>))
import           Data.Aeson
#if MIN_VERSION_aeson(2,0,0)
import           Data.Aeson.KeyMap   (keys)
#else
import qualified Data.HashMap.Strict as Map
import qualified Data.Text           as T
#endif
import           Data.Aeson.Types    (Object, Parser, Value (Null),
                                      typeMismatch, object)
import qualified Data.Char           as C (toLower)
import           Data.List           ((\\))
import           GHC.Generics        (Generic)

#if !MIN_VERSION_aeson(2,0,0)
-- | Backwards compatible type-def for Key
-- This used to be just a Text, but since aeson >= 2
-- this is an opaque datatype.
type Key = T.Text
-- | Backwards compatible type-def for KeyMap
-- This used to be just a HashMap, but since aeson >= 2
-- this is an opaque datatype.
type KeyMap v = Map.HashMap T.Text v

keys :: KeyMap v -> [Key]
keys = Map.keys
#endif

data CradleConfigYAML a
  = CradleConfigYAML { cradle       :: CradleComponent a
                     , dependencies :: Maybe [FilePath]
                     } deriving (Generic, FromJSON)

data CradleComponent a
  = Multi [MultiSubComponent a]
  | Cabal CabalConfig
  | Stack StackConfig
  | Direct DirectConfig
  | Bios BiosConfig
  | None NoneConfig
  | Other (OtherConfig a)
  deriving (Generic)

instance FromJSON a => FromJSON (CradleComponent a) where
  parseJSON = let opts = defaultOptions { constructorTagModifier = fmap C.toLower
                                        , sumEncoding = ObjectWithSingleField
                                        }
               in genericParseJSON opts

data NoneConfig = NoneConfig

data OtherConfig a
  = OtherConfig { otherConfig       :: a
                , originalYamlValue :: Value
                }

instance FromJSON a => FromJSON (OtherConfig a) where
  parseJSON v = OtherConfig
                  <$> parseJSON v
                  <*> pure v

instance FromJSON NoneConfig where
  parseJSON Null = pure NoneConfig
  parseJSON v    = typeMismatch "NoneConfig" v

data MultiSubComponent a
  = MultiSubComponent { path   :: FilePath
                      , config :: CradleConfigYAML a
                      } deriving (Generic, FromJSON)

data CabalConfig
  = CabalConfig { cabalComponents :: OneOrManyComponents CabalComponent }

instance FromJSON CabalConfig where
  parseJSON v@(Array _)     = CabalConfig . ManyComponents <$> parseJSON v
  parseJSON v@(Object _)  = CabalConfig <$> parseJSON v
  parseJSON Null            = pure $ CabalConfig NoComponent
  parseJSON v               = typeMismatch "CabalConfig" v

data CabalComponent
  = CabalComponent { cabalPath      :: FilePath
                   , cabalComponent :: String
                   }

instance FromJSON CabalComponent where
  parseJSON = withObject "CabalComponent" $ \obj -> CabalComponent
    <$> obj .: "path"
    <*> obj .: "component"

data StackConfig
  = StackConfig { stackYaml       :: Maybe FilePath
                , stackComponents :: OneOrManyComponents StackComponent
                }

data StackComponent
  = StackComponent { stackPath          :: FilePath
                   , stackComponent     :: String
                   , stackComponentYAML :: Maybe String
                   }

instance FromJSON StackConfig where
  parseJSON v@(Array _)     = StackConfig Nothing . ManyComponents <$> parseJSON v
  parseJSON v@(Object obj)  = StackConfig <$> obj .:? "stackYaml" <*> parseJSON v
  parseJSON Null            = pure $ StackConfig Nothing NoComponent
  parseJSON v               = typeMismatch "StackConfig" v

instance FromJSON StackComponent where
  parseJSON = withObject "StackComponent" $ \obj -> StackComponent
    <$> obj .: "path"
    <*> obj .: "component"
    <*> obj .:? "stackYaml"

data OneOrManyComponents component
  = SingleComponent String
  | ManyComponents [component]
  | NoComponent

instance FromJSON component => FromJSON (OneOrManyComponents component) where
  parseJSON =
    let parseComponents o = (parseSingleComponent o <|> parseSubComponents o <|> pure NoComponent)
        parseSingleComponent o = SingleComponent <$> o .: "component"
        parseSubComponents   o = ManyComponents <$> o .: "components"
     in withObject "Components" parseComponents

instance ToJSON component => ToJSON (OneOrManyComponents component) where
  toJSON = \case
    NoComponent -> object []
    SingleComponent s -> object ["component" .= s]
    ManyComponents cs -> object ["components" .= cs]

data DirectConfig
  = DirectConfig { arguments :: [String] }
  deriving (Generic, FromJSON, ToJSON)

data BiosConfig =
  BiosConfig { callable     :: Callable
             , depsCallable :: Maybe Callable
             , ghcPath      :: Maybe FilePath
             }

instance FromJSON BiosConfig where
  parseJSON = withObject "BiosConfig" parseBiosConfig

instance ToJSON BiosConfig where
  toJSON bc =
    let
      mode r = case callable bc of
        Program a -> object $ ["program" .= a] <> r
        Shell a -> object $ ["shell" .= a] <> r
    in
      mode []

data Callable
  = Program FilePath
  | Shell String

parseBiosConfig :: Object -> Parser BiosConfig
parseBiosConfig o =
  let parseCallable = (Program <$> o .: "program") <|> (Shell <$> o .: "shell")
      parseDepsCallable = (Just . Program <$> o .: "dependency-program")
                            <|> (Just . Shell <$> o .: "dependency-shell")
                            <|> (pure Nothing)
   in BiosConfig
      <$> parseCallable
      <*> parseDepsCallable
      <*> (o .:? "with-ghc")

data Differences
  = UnknownKey Key [Key]
  | StructureError


-- diffValueKeys :: Value -> Value -> [Differences]
-- diffValueKeys

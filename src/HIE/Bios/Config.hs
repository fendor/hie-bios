{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module HIE.Bios.Config(
    readConfig,
    Config(..),
    CradleConfig(..),
    CradleType(..)
    ) where

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as Map
import           Data.Yaml

data CradleConfig =
    CradleConfig
        { cradleDependencies :: [FilePath]
        -- ^ Dependencies of a cradle.
        -- Dependencies are expected to be relative to the root directory.
        -- The given files are not required to exist.
        , cradleType :: CradleType
        -- ^ Type of the cradle to use. Actions to obtain
        -- compiler flags from are dependant on this field.
        }
        deriving (Show)

data CradleType
    = Cabal { component :: Maybe String }
    | Stack
    | Bazel
    | Obelisk
    | Bios
        { prog :: FilePath
        -- ^ Path to program that retrieves options to compile a file
        , depsProg :: Maybe FilePath
        -- ^ Optional Path to program to obtain cradle dependencies.
        -- Each cradle dependency is to be expected to be on a separate line
        -- and relative to the root dir of the cradle, not relative
        -- to the location of this program.
        }
    | Direct { arguments :: [String] }
    | Default
    deriving (Show)

instance FromJSON CradleConfig where
    parseJSON (Object o) = do
        deps <- o .:? "dependencies" .!= []
        crdType <- parseCradleType o
        return $ CradleConfig deps crdType

    parseJSON _ = error "me"

instance FromJSON CradleType where
    parseJSON (Object o) = parseCradleType o
    parseJSON _ = fail "Not a known configuration"

parseCradleType :: Object -> Parser CradleType
parseCradleType o
    | Just val <- Map.lookup "cabal" o = parseCabal val
    | Just _val <- Map.lookup "stack" o = return Stack
    | Just _val <- Map.lookup "bazel" o = return Bazel
    | Just _val <- Map.lookup "obelisk" o = return Obelisk
    | Just val <- Map.lookup "bios" o = parseBios val
    | Just val <- Map.lookup "direct" o = parseDirect val
    | Just _val <- Map.lookup "default" o = return Default
parseCradleType o = fail $ "Unknown cradle type: " ++ show o

parseCabal :: Value -> Parser CradleType
parseCabal cabalValue
    | Object x <- cabalValue
    , Just (String cabalComponent) <- Map.lookup "component" x
    = return $ Cabal $ Just $ T.unpack cabalComponent

    | Object _ <- cabalValue
    = return $ Cabal Nothing

    | otherwise
    = fail $ "Could not parse Cabal component for value: " ++ show cabalValue

parseBios :: Value -> Parser CradleType
parseBios biosValue
    | Object x <- biosValue
    , Just (String biosProgram) <- Map.lookup "program" x
    = case  Map.lookup "dependency-program" x of
        Just (String deps) -> return $ Bios (T.unpack biosProgram) (Just (T.unpack deps))
        _ -> return $ Bios (T.unpack biosProgram) Nothing

    | otherwise
    = fail "Not a valid Bios Configuration type"

parseDirect :: Value -> Parser CradleType
parseDirect directValue
    | Object x <- directValue
    , Just (Array v) <- Map.lookup "arguments" x
    = return $ Direct [T.unpack s | String s <- V.toList v]

    | otherwise
    = fail "Not a correct Direct Configuration type"

data Config = Config { cradle :: CradleConfig }
    deriving (Show)

instance FromJSON Config where
    parseJSON (Object (Map.toList -> [("cradle", x)])) = Config <$> parseJSON x
    parseJSON _ = fail "Expected a cradle: key containing the preferences"

readConfig :: FilePath -> IO Config
readConfig fp = decodeFileThrow fp

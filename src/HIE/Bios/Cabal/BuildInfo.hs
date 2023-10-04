{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HIE.Bios.Cabal.BuildInfo where

import Cabal.BuildInfo
import HIE.Bios.Types
import Data.Conduit.Process.Typed (ExitCode(ExitFailure))
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (maybeToList)

readBuildInfoIntoComponentOptions :: FilePath -> CradleLoadResultT IO (ComponentOptions)
readBuildInfoIntoComponentOptions fp = do
  binfo <- liftIO (decodeBuildInfoFile fp) >>= \case
    Left err -> throwCE $ CradleError [] (ExitFailure 101) ["Failed to decode build-info", err]
    Right b -> pure b

  case components binfo of
    [] -> throwCE $ CradleError [] (ExitFailure 102) ["Weird"]
    [c] -> pure ComponentOptions
      { componentOptions =
          componentCompilerArgs c ++
          componentModules c
      , componentRoot = componentSrcDir c
      , componentDependencies =
          [componentCabalFile c]
      }
    xs -> throwCE $ CradleError [] (ExitFailure 103) ["Custom build"]

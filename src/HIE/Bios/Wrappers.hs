{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module HIE.Bios.Wrappers (cabalWrapper, cabalWrapperHs) where

import Data.FileEmbed
import qualified Data.Text as T

cabalWrapper :: FilePath -> T.Text
cabalWrapper ghc =  T.replace (T.pack "ghc") (T.pack ghc) $(embedStringFile "wrappers/cabal")

cabalWrapperHs :: FilePath -> T.Text
cabalWrapperHs ghc = T.replace (T.pack "ghc") (T.pack ghc) $(embedStringFile "wrappers/cabal.hs")


module HIE.Bios.Ghc.Check (
    checkSyntax
  , check
  , expandTemplate
  , expand
  , modGraph
  ) where

import DynFlags (dopt_set, DumpFlag(Opt_D_dump_splices))
import GHC
import qualified Outputable


import HIE.Bios.Ghc.Api
import HIE.Bios.Ghc.Logger
import HIE.Bios.Types
import HIE.Bios.Ghc.Load
import Outputable
import           Data.Maybe                     ( mapMaybe )

----------------------------------------------------------------

-- | Checking syntax of a target file using GHC.
--   Warnings and errors are returned.
checkSyntax :: Options
            -> Cradle
            -> [FilePath]  -- ^ The target files.
            -> IO String
checkSyntax _   _      []    = return ""
checkSyntax opt cradle files = withGhcT $ do
    pprTrace "cradle" (text $ show cradle) (return ())
    initializeFlagsWithCradle (head files) cradle
    either id id <$> check opt files
  where
    {-
    sessionName = case files of
      [file] -> file
      _      -> "MultipleFiles"
      -}

modGraph :: Cradle -> FilePath -> IO [FilePath]
modGraph crdl fp = withGhcT $ do
  initializeFlagsWithCradle fp crdl
  mg <- GHC.getModuleGraph
  Outputable.pprTraceM "modGraph" (Outputable.ppr $ GHC.mgModSummaries mg)
  return $ mapMaybe (GHC.ml_hs_file . GHC.ms_location) (GHC.mgModSummaries mg)

----------------------------------------------------------------

-- | Checking syntax of a target file using GHC.
--   Warnings and errors are returned.
check :: (GhcMonad m)
      => Options
      -> [FilePath]  -- ^ The target files.
      -> m (Either String String)
check opt fileNames = withLogger opt setAllWarningFlags $ setTargetFiles (map dup fileNames)

dup :: a -> (a, a)
dup x = (x, x)

----------------------------------------------------------------

-- | Expanding Haskell Template.
expandTemplate :: Options
               -> Cradle
               -> [FilePath]  -- ^ The target files.
               -> IO String
expandTemplate _   _      []    = return ""
expandTemplate opt cradle files = withGHC sessionName $ do
    initializeFlagsWithCradle (head files) cradle
    either id id <$> expand opt files
  where
    sessionName = case files of
      [file] -> file
      _      -> "MultipleFiles"

----------------------------------------------------------------

-- | Expanding Haskell Template.
expand :: Options
      -> [FilePath]  -- ^ The target files.
      -> Ghc (Either String String)
expand opt fileNames = withLogger opt (setDumpSplices . setNoWarningFlags) $ setTargetFiles (map dup fileNames)

setDumpSplices :: DynFlags -> DynFlags
setDumpSplices dflag = dopt_set dflag Opt_D_dump_splices

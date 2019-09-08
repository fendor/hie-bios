-- | The HIE Bios

module HIE.Bios (
  -- * Find and load a Cradle
    Cradle(..)
  , findCradle
  , loadCradle
  , loadImplicitCradle
  , defaultCradle
  -- * Compiler Options
  , CompilerOptions(..)
  , getCompilerOptions
  -- * Initialise session
  , initSession
  ) where

import HIE.Bios.Cradle
import HIE.Bios.Types
import HIE.Bios.Flags
import HIE.Bios.Environment

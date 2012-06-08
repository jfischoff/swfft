module Paths_traced (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [2009,7,20], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/jonathan.fischoff/Library/Haskell/ghc-7.4.1/lib/traced-2009.7.20/bin"
libdir     = "/Users/jonathan.fischoff/Library/Haskell/ghc-7.4.1/lib/traced-2009.7.20/lib"
datadir    = "/Users/jonathan.fischoff/Library/Haskell/ghc-7.4.1/lib/traced-2009.7.20/share"
libexecdir = "/Users/jonathan.fischoff/Library/Haskell/ghc-7.4.1/lib/traced-2009.7.20/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "traced_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "traced_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "traced_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "traced_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_votacion (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/carlosro/.cabal/bin"
libdir     = "/home/carlosro/.cabal/lib/x86_64-linux-ghc-7.10.3/votacion-0.1.0.0-Gy3RKIie6h4C3R3vwS9sQn"
datadir    = "/home/carlosro/.cabal/share/x86_64-linux-ghc-7.10.3/votacion-0.1.0.0"
libexecdir = "/home/carlosro/.cabal/libexec"
sysconfdir = "/home/carlosro/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "votacion_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "votacion_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "votacion_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "votacion_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "votacion_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Simulador (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/fz/Desktop/8vo ciclo/OS/Tasks/Simulador/.stack-work/install/x86_64-linux/lts-12.17/8.4.4/bin"
libdir     = "/home/fz/Desktop/8vo ciclo/OS/Tasks/Simulador/.stack-work/install/x86_64-linux/lts-12.17/8.4.4/lib/x86_64-linux-ghc-8.4.4/Simulador-0.1.0.0-6uBmKScFMfmJIy2VJlY5Qv-Simulador"
dynlibdir  = "/home/fz/Desktop/8vo ciclo/OS/Tasks/Simulador/.stack-work/install/x86_64-linux/lts-12.17/8.4.4/lib/x86_64-linux-ghc-8.4.4"
datadir    = "/home/fz/Desktop/8vo ciclo/OS/Tasks/Simulador/.stack-work/install/x86_64-linux/lts-12.17/8.4.4/share/x86_64-linux-ghc-8.4.4/Simulador-0.1.0.0"
libexecdir = "/home/fz/Desktop/8vo ciclo/OS/Tasks/Simulador/.stack-work/install/x86_64-linux/lts-12.17/8.4.4/libexec/x86_64-linux-ghc-8.4.4/Simulador-0.1.0.0"
sysconfdir = "/home/fz/Desktop/8vo ciclo/OS/Tasks/Simulador/.stack-work/install/x86_64-linux/lts-12.17/8.4.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Simulador_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Simulador_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Simulador_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Simulador_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Simulador_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Simulador_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_simple_genetic_algorithm (
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

bindir     = "/home/jose/Documents/Escuela/Trimestre 11/Optimizaci\243n II/Parte 2/practica 1/simple-genetic-algorithm/.stack-work/install/x86_64-linux-nix/7e2f51665fa299c8d764337433d2f05cff19919a957f5bcc77bad907e842776f/8.10.4/bin"
libdir     = "/home/jose/Documents/Escuela/Trimestre 11/Optimizaci\243n II/Parte 2/practica 1/simple-genetic-algorithm/.stack-work/install/x86_64-linux-nix/7e2f51665fa299c8d764337433d2f05cff19919a957f5bcc77bad907e842776f/8.10.4/lib/x86_64-linux-ghc-8.10.4/simple-genetic-algorithm-0.1.0.0-38pzb1SFg18DxgHRjPqF6W-simple-genetic-algorithm"
dynlibdir  = "/home/jose/Documents/Escuela/Trimestre 11/Optimizaci\243n II/Parte 2/practica 1/simple-genetic-algorithm/.stack-work/install/x86_64-linux-nix/7e2f51665fa299c8d764337433d2f05cff19919a957f5bcc77bad907e842776f/8.10.4/lib/x86_64-linux-ghc-8.10.4"
datadir    = "/home/jose/Documents/Escuela/Trimestre 11/Optimizaci\243n II/Parte 2/practica 1/simple-genetic-algorithm/.stack-work/install/x86_64-linux-nix/7e2f51665fa299c8d764337433d2f05cff19919a957f5bcc77bad907e842776f/8.10.4/share/x86_64-linux-ghc-8.10.4/simple-genetic-algorithm-0.1.0.0"
libexecdir = "/home/jose/Documents/Escuela/Trimestre 11/Optimizaci\243n II/Parte 2/practica 1/simple-genetic-algorithm/.stack-work/install/x86_64-linux-nix/7e2f51665fa299c8d764337433d2f05cff19919a957f5bcc77bad907e842776f/8.10.4/libexec/x86_64-linux-ghc-8.10.4/simple-genetic-algorithm-0.1.0.0"
sysconfdir = "/home/jose/Documents/Escuela/Trimestre 11/Optimizaci\243n II/Parte 2/practica 1/simple-genetic-algorithm/.stack-work/install/x86_64-linux-nix/7e2f51665fa299c8d764337433d2f05cff19919a957f5bcc77bad907e842776f/8.10.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "simple_genetic_algorithm_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "simple_genetic_algorithm_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "simple_genetic_algorithm_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "simple_genetic_algorithm_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "simple_genetic_algorithm_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "simple_genetic_algorithm_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_monomer_starter (
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

bindir     = "/Users/eric/msc-project/second-semester/.stack-work/install/x86_64-osx/167079fef1aada05b0f885ee3429c9f8879c352f1816c7974bc6eb49c4d381aa/8.10.7/bin"
libdir     = "/Users/eric/msc-project/second-semester/.stack-work/install/x86_64-osx/167079fef1aada05b0f885ee3429c9f8879c352f1816c7974bc6eb49c4d381aa/8.10.7/lib/x86_64-osx-ghc-8.10.7/monomer-starter-0.1.0.0-HkBAnAbL4Ze6ZtV77qUiao-app"
dynlibdir  = "/Users/eric/msc-project/second-semester/.stack-work/install/x86_64-osx/167079fef1aada05b0f885ee3429c9f8879c352f1816c7974bc6eb49c4d381aa/8.10.7/lib/x86_64-osx-ghc-8.10.7"
datadir    = "/Users/eric/msc-project/second-semester/.stack-work/install/x86_64-osx/167079fef1aada05b0f885ee3429c9f8879c352f1816c7974bc6eb49c4d381aa/8.10.7/share/x86_64-osx-ghc-8.10.7/monomer-starter-0.1.0.0"
libexecdir = "/Users/eric/msc-project/second-semester/.stack-work/install/x86_64-osx/167079fef1aada05b0f885ee3429c9f8879c352f1816c7974bc6eb49c4d381aa/8.10.7/libexec/x86_64-osx-ghc-8.10.7/monomer-starter-0.1.0.0"
sysconfdir = "/Users/eric/msc-project/second-semester/.stack-work/install/x86_64-osx/167079fef1aada05b0f885ee3429c9f8879c352f1816c7974bc6eb49c4d381aa/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "monomer_starter_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "monomer_starter_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "monomer_starter_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "monomer_starter_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "monomer_starter_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "monomer_starter_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

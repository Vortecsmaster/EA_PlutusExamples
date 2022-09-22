{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_MathBounty (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/plutus/.cabal/bin"
libdir     = "/home/plutus/.cabal/lib/x86_64-linux-ghc-8.10.4.20210212/MathBounty-0.1.0.0-inplace"
dynlibdir  = "/home/plutus/.cabal/lib/x86_64-linux-ghc-8.10.4.20210212"
datadir    = "/home/plutus/.cabal/share/x86_64-linux-ghc-8.10.4.20210212/MathBounty-0.1.0.0"
libexecdir = "/home/plutus/.cabal/libexec/x86_64-linux-ghc-8.10.4.20210212/MathBounty-0.1.0.0"
sysconfdir = "/home/plutus/.cabal/etc"

getBinDir     = catchIO (getEnv "MathBounty_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "MathBounty_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "MathBounty_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "MathBounty_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "MathBounty_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "MathBounty_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'

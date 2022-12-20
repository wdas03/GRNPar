{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_sbv (
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
version = Version [9,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/Anushka/.cabal/store/ghc-9.0.2/sbv-9.0-7671916e/bin"
libdir     = "/Users/Anushka/.cabal/store/ghc-9.0.2/sbv-9.0-7671916e/lib"
dynlibdir  = "/Users/Anushka/.cabal/store/ghc-9.0.2/lib"
datadir    = "/Users/Anushka/.cabal/store/ghc-9.0.2/sbv-9.0-7671916e/share"
libexecdir = "/Users/Anushka/.cabal/store/ghc-9.0.2/sbv-9.0-7671916e/libexec"
sysconfdir = "/Users/Anushka/.cabal/store/ghc-9.0.2/sbv-9.0-7671916e/etc"

getBinDir     = catchIO (getEnv "sbv_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "sbv_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "sbv_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "sbv_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sbv_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "sbv_sysconfdir") (\_ -> return sysconfdir)




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

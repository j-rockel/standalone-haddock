{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
{-
Work around this warning:
libraries/Cabal/Distribution/Simple.hs:78:0:
    Warning: In the use of `runTests'
             (imported from Distribution.Simple.UserHooks):
             Deprecated: "Please use the new testing interface instead!"
-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- Code copied or derived from Distribution.Simple, because it's not part
-- of Cabal's public API

-- |
-- Module      :  Distribution.Simple
-- Copyright   :  Isaac Jones 2003-2005
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
module Cabal.Simple
  ( simpleUserHooks,
    configureAction,
    haddockAction,
  )
where

import Cabal.Haddock
import Cabal.Prelude
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PreProcess
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Simple.UserHooks
import Distribution.Simple.Utils
import Distribution.Verbosity
import Prelude ()

-- Base
#ifdef mingw32_HOST_OS
import System.FilePath    (normalise, splitDrive)
#endif

import Distribution.PackageDescription.Parsec

configureAction :: UserHooks -> ConfigFlags -> Args -> IO LocalBuildInfo
configureAction hooks flags args = do
  distPref <- findDistPrefOrDefault (configDistPref flags)
  let flags' =
        flags
          { configDistPref = toFlag distPref,
            configArgs = args
          }

  -- See docs for 'HookedBuildInfo'
  pbi <- preConf hooks args flags'

  (mb_pd_file, pkg_descr0) <-
    confPkgDescr
      hooks
      verbosity
      (flagToMaybe (configCabalFilePath flags))

  let epkg_descr = (pkg_descr0, pbi)

  localbuildinfo0 <- confHook hooks epkg_descr flags'

  -- remember the .cabal filename if we know it
  -- and all the extra command line args
  let localbuildinfo =
        localbuildinfo0
          { pkgDescrFile = mb_pd_file,
            extraConfigArgs = args
          }
  writePersistBuildConfig distPref localbuildinfo

  let pkg_descr = localPkgDescr localbuildinfo
  postConf hooks args flags' pkg_descr localbuildinfo
  return localbuildinfo
  where
    verbosity = fromFlag (configVerbosity flags)

confPkgDescr ::
  UserHooks ->
  Verbosity ->
  Maybe FilePath ->
  IO (Maybe FilePath, GenericPackageDescription)
confPkgDescr hooks verbosity mb_path = do
  mdescr <- readDesc hooks
  case mdescr of
    Just descr -> return (Nothing, descr)
    Nothing -> do
      pdfile <- case mb_path of
        Nothing -> defaultPackageDesc verbosity
        Just path -> return path
      info verbosity "Using Parsec parser"
      descr <- readGenericPackageDescription verbosity pdfile
      return (Just pdfile, descr)

haddockAction :: LocalBuildInfo -> UserHooks -> HaddockFlags -> Args -> (PackageId -> FilePath) -> IO ()
haddockAction lbi _hooks flags _args computePath = do
  -- let distPref  = fromFlag $ haddockDistPref flags
  let verbosity = fromFlagOrDefault normal $ haddockVerbosity flags

  _progs <-
    reconfigurePrograms
      verbosity
      (haddockProgramPaths flags)
      (haddockProgramArgs flags)
      (withPrograms lbi)

  let pkg_descr = localPkgDescr lbi
  haddock pkg_descr lbi knownSuffixHandlers flags computePath

simpleUserHooks :: UserHooks
simpleUserHooks = emptyUserHooks {confHook = configure}

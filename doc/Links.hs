{-# LANGUAGE ViewPatterns #-}
module Links
  ( -- * Maps
    NameMap
  , ModuleMap, Module
  , buildPackageMaps

    -- * Hackage
  , hackageName
  , hackageModule
  , hackage

    -- * Name helpers
  , Name
  , nameModule
  , showName
  , nameAnchor

    -- * Module helpers
  , showModule
  , showModuleD
  , showModulePkg
  , showModulePkgV

    -- * GHC
  , getPkgModules
  ) where
-- |
-- Provides maps from modules to packages and functions / data types to
-- modules.
import           Control.Applicative    ((<$>))
import           Data.Foldable          (foldMap)
import           Data.List              (intercalate, isPrefixOf)
import qualified Data.Map               as M
import           Data.Maybe
import           DynFlags               (PackageFlag (ExposePackage),
                                         defaultFatalMessager, defaultFlushOut)
import           GHC                    (ModuleInfo, defaultErrorHandler,
                                         getModuleInfo, getSessionDynFlags,
                                         modInfoExports, noLoc, packageFlags,
                                         parseDynamicFlags, pkgState, runGhc,
                                         setSessionDynFlags)
import           GHC.Paths              (libdir)
import           Module                 (Module, PackageId, mkModule,
                                         moduleName, moduleNameString,
                                         modulePackageId, packageIdString)
import           Name                   (Name, isValName, nameModule,
                                         nameOccName, occNameString)
import           Packages               (exposedModules, getPackageDetails,
                                         initPackages)

import           Control.Arrow
import           Control.Monad.IO.Class
import           Data.Char
import           Data.List.Split
import           Data.Traversable
import           Data.Tuple
import           Prelude                hiding (mapM, sequence)

-- There are two abstract data types used in this module:
--
-- 'Module': Contains infomation about the package the module is in and
-- the module itself.
--
-- 'Name': Contains information about the function or type name and the
-- 'Module' it was defined in.
--
-- These are the only functions the user should deal with.

-- | A mapping from the name of a module (like @Diagrams.TwoD.Text@) to a
--   @Module@.
type ModuleMap = M.Map String Module

-- | A mapping from exported names (like 'scale') to a @Name@ and the
-- @Module@ the name was exported from.
type NameMap = M.Map String Name

------------------------------------------------------------------------
-- Hackage utils
------------------------------------------------------------------------

-- | Make a link to the hackage site to the given
--   'Name'. Includes a hyperlink to the name.
hackageName :: Name -> String
hackageName n = hackageModule (nameModule n) ++ nameAnchor n

-- | Make a link to the hackage site to the given 'Module'
hackageModule :: Module -> String
hackageModule m =
  hackage ++ showModulePkgV m ++ "/docs/" ++ showModuleD m ++ ".html"

-- | @https://hackage.haskell.org/package/@
hackage :: String
hackage = "https://hackage.haskell.org/package/"

------------------------------------------------------------------------
-- Name helpers
------------------------------------------------------------------------

-- | The anchor for a haddock page to the name.

nameAnchor :: Name -> String
nameAnchor name =
   '#' : prefix : ':' : makeAnchorId (showName name)
 where prefix | isValName name = 'v'
              | otherwise      = 't'

-- Taken from haddock.
makeAnchorId :: String -> String
makeAnchorId [] = []
makeAnchorId (f:r) = escape isAlpha f ++ concatMap (escape isLegal) r
  where
    escape p c | p c = [c]
               | otherwise = '-' : show (ord c) ++ "-"
    isLegal ':' = True
    isLegal '_' = True
    isLegal '.' = True
    isLegal c = isAscii c && isAlphaNum c

showName :: Name -> String
showName = occNameString . nameOccName

------------------------------------------------------------------------
-- Module helpers
------------------------------------------------------------------------

-- | Show package with without: @diagrams-lib@.
showModulePkg :: Module -> String
showModulePkg = intercalate "-" . init . splitOn "-" . showModulePkgV

-- | Show package with version: @diagrams-lib-1.2@.
showModulePkgV :: Module -> String
showModulePkgV = packageIdString . modulePackageId

-- | Show a module name with dots: @Diagrams.TwoD.Text@.
showModule :: Module -> String
showModule = moduleNameString . moduleName

-- | Show a module with dashes instead of dots: @Diagrams-TwoD-Text@.
showModuleD :: Module -> String
showModuleD = map dashDot . showModule
  where
    dashDot '.' = '-'
    dashDot c   = c

------------------------------------------------------------------------
-- Get maps
------------------------------------------------------------------------

-- | Given a list of package names, build a mapping from module names to
--   packages so we can look up what package provides a given module.
buildPackageMaps :: [String] -> IO (ModuleMap, NameMap)
buildPackageMaps pkgs = do
  pkgMods <- catMaybes <$> mapM getPkgModules pkgs
  let pkgModPairs = foldMap sequence pkgMods :: [(PackageId, (Module, ModuleInfo))]

      mkModMap (_, (mdl, _)) = (showModule mdl,  mdl)
      modMap = M.fromList $ map mkModMap pkgModPairs

  -- name map
  let infos   = map (snd . snd) pkgModPairs
      names   = foldMap modInfoExports infos
      nameMap = M.fromList $ map (showName &&& id) names
  return (modMap, nameMap)

------------------------------------------------------------------------
-- Low level
------------------------------------------------------------------------

-- At time of writing the ghc haddock documentation is not on hackage
-- but can be found here:
-- https://downloads.haskell.org/~ghc/latest/docs/html/libraries/ghc-7.8.4/index.html

-- getHsenvArgv :: IO [String]
-- getHsenvArgv = do
--   env <- getEnvironment
--   return $ case lookup "HSENV" env of
--              Nothing -> []
--              _       -> hsenvArgv
--                where hsenvArgv = words $ fromMaybe "" (lookup "PACKAGE_DB_FOR_GHC" env)

-- | Get the list of modules provided by a package.
getPkgModules :: String -> IO (Maybe (PackageId, [(Module, ModuleInfo)]))
getPkgModules pkg =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $
    -- ! If package does not exist, this throws an exception
    runGhc (Just libdir) $ do
      dflags0 <- getSessionDynFlags
      let dflags1 = dflags0 { packageFlags = ExposePackage pkg : packageFlags dflags0 }
      -- args <- liftIO getHsenvArgv
      let args = []
      let args' = map noLoc args
      (dflags2, _, _) <- parseDynamicFlags dflags1 args'
      (dflags3, pids) <- liftIO $ initPackages dflags2
      _ <- setSessionDynFlags dflags3
      let pkgSt    = pkgState dflags3
          mpid     = listToMaybe (filter ((pkg `isPrefixOf`) . packageIdString) pids)
      case mpid of
        Nothing    -> return Nothing
        Just pkgid -> do
          let pkgConfig = getPackageDetails pkgSt pkgid
          let modNames = exposedModules pkgConfig
          modInfos <- forM modNames $ \n -> do
            let mdl = mkModule pkgid n
            mInfo <- getModuleInfo mdl
            return $ sequence (mdl,mInfo)
          return $ Just (pkgid, catMaybes modInfos)

-- TODO:
-- Include types with Name


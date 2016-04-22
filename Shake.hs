{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Control.Concurrent          (getNumCapabilities)
import           Control.Monad               (when)
import           Control.Parallel.Strategies (NFData)
import           Data.Functor                ((<$>))
import           Data.List                   (isPrefixOf, (\\))
import           Data.Maybe                  (fromMaybe)
import           Development.Shake           hiding ((<//>))
import           Development.Shake.Classes   (Binary, Hashable)
import           Development.Shake.FilePath  (dropDirectory1, dropExtension,
                                              takeBaseName, takeDirectory,
                                              (-<.>), (<.>), (</>))
import           Safe                        (readMay)
import           System.Console.CmdArgs
import           System.Directory            (canonicalizePath,
                                              createDirectoryIfMissing)
import           System.Environment          (lookupEnv)
import           System.Process              (readProcess, system)
import           System.Exit                 (ExitCode(..))

import           Prelude                     hiding ((*>))

import qualified BuildBanner
import qualified BuildGallery

obj, un, dist :: FilePath -> FilePath
obj = (".make" <//>)
un = dropDirectory1
dist = ("dist" <//>)

runExe :: [CmdOption] -> FilePath -> [String] -> Action ()
runExe options exe args = command_ options "stack" (["exec", exe, "--"] ++ args)

-- Like </>, but retain the first argument when the second starts with
-- a forward slash
(<//>) :: String -> String -> String
d <//> p@('/':_) = d ++ p
d <//> p = d </> p

data MkMode =
    Build    -- Build pre-Hakyll stuff only.  Works well to first run 'Shake preview',
             -- then while it is still running, run 'Shake build' in a loop.
  | BuildH   -- build Hakyll.
  | Preview  -- build everything and run hakyll preview.
  | Clean    -- full clean.
  deriving (Eq, Show, Data, Typeable)

mkModes :: MkMode
mkModes = modes
  [ Build &= auto
  , BuildH
  , Preview
  , Clean
  ]

imgExt :: String
imgExt = "png"

main :: IO ()
main = do
  m <- cmdArgs mkModes
  n <- getNumCapabilities

  threadsStr <- lookupEnv "DIA_DOC_THREADS"

  let numThreads = fromMaybe (max 2 (n-1)) (threadsStr >>= readMay)

  putStrLn $ "Using " ++ show numThreads ++ " threads."
  putStrLn "Using Rasterific backend."

  case m of
    Clean -> mapM_ system
      [ "rm -rf web/_site"
      , "rm -rf web/_cache"
      , "rm -f web/doc"
      , "rm -f web/blog"
      , "rm -f web/gallery/images"
      , "rm -f web/banner/images"
      , "rm -rf .make"
      , "rm -rf dist"
      , "rm -f Shake"
      , "rm -f Shake.o"
      , "rm -f Shake.hi"
      , "rm -f .shake.database"
      ]

    _ -> shake shakeOptions { shakeThreads = numThreads } $ do
      ghcThreads <- newResource "GHC threads" 1

      action $ requireRst "doc"
      action $ requireRst "blog"
      action $ requireIcons
      action $ requireStatic

      webRules

      dist "//*.html" *> \out -> do
        let xml = obj . un $ out -<.> "xml"
        need [xml]
        withResource ghcThreads 1 $ runExe [] "Xml2Html" [xml, "-o", takeDirectory out </> "images", out]

      dist "blog/*.metadata" *> \out -> copyFile' (un out) out
      dist "doc/*.metadata"  *> \out -> copyFile' (un out) out

      obj "//*.xml" *> \out -> do
        let rst = un $ out -<.> "rst"
        need [rst]
        command_ [] "rst2xml" ["--input-encoding=utf8", rst, out]

      let
        makeIcon out = runExe [] exe ["-w", "40", "-h", "40", "-o", out]
          where exe = takeBaseName out

      dist ("doc/icons/Exercises" <.> imgExt) *> makeIcon
      dist ("doc/icons/ToWrite"   <.> imgExt) *> makeIcon
      dist ("doc/icons/Warning"   <.> imgExt) *> makeIcon

      copyFiles "doc/static"

      dist ("web/gallery/*.big" <.> imgExt) *> \out -> do
        need [dropExtension (un out) -<.> "lhs"]
        compileImg False out

      dist ("web/gallery/*.thumb" <.> imgExt) *> \out -> do
        need [dropExtension (un out) -<.> "lhs"]
        compileImg True out

      dist ("web/banner/banner" <.> imgExt) *> \out -> do
        need [dropExtension (un out) -<.> "hs"]
        let name = takeBaseName (takeBaseName out)
            hsName = "web/banner" </> name -<.> "hs"
        liftIO $ BuildBanner.compileExample hsName out

      when (m /= Build) (action $ runWeb m)

      return ()

compileImg :: Bool -> FilePath -> Action ()
compileImg isThumb outPath = do
  let name    = takeBaseName . takeBaseName $ outPath
      lhsName = "web/gallery" </> name -<.> "lhs"
      thumb   = if isThumb then Just 200 else Nothing

  liftIO $ BuildGallery.compileExample thumb lhsName outPath

copyFiles :: String -> Rules ()
copyFiles dir = dist (dir ++ "/*") *> \out -> copyFile' (un out) out

requireIcons :: Action ()
requireIcons = do
  need [ dist "doc/icons" </> name -<.> imgExt
       | name <- ["Warning", "ToWrite", "Exercises"]]

requireStatic :: Action ()
requireStatic = do
  staticSrc <- getDirectoryFiles "doc/static" ["*"]
  let static = map (dist . ("doc/static" </>)) staticSrc
  need static

requireGallery :: Action ()
requireGallery = do
  gallerySrc <- filter (not . (".#" `isPrefixOf`))
                <$> getDirectoryFiles "web/gallery" ["*.lhs"]
  let imgs   = map (dist . ("web/gallery" </>) . (-<.> ("big" <.> imgExt))) gallerySrc
      thumbs = map (dist . ("web/gallery" </>) . (-<.> ("thumb" <.> imgExt))) gallerySrc
  need (imgs ++ thumbs)

requireBanner :: Action ()
requireBanner = do
  need [dist $ "web/banner/banner" <.> imgExt]

requireRst :: String -> Action ()
requireRst dir = do
  rsts <- filter (not . (".#" `isPrefixOf`))
          <$> getDirectoryFiles dir ["*.rst"]
  need (map (dist . (dir </>) . (-<.> "html")) rsts)
  need (map (dist . (dir </>) . (-<.> "html.metadata")) rsts)

webRules :: Rules ()
webRules = do
  "web/doc" *> \out ->
    command_ [] "ln" ["-s", "-f", "../dist/doc", out]

  "web/blog" *> \out ->
    command_ [] "ln" ["-s", "-f", "../dist/blog", out]

  "web/gallery/images" *> \out -> do
    liftIO $ createDirectoryIfMissing True "dist/web/gallery"
    command_ [] "ln" ["-s", "-f", "../../dist/web/gallery", out]

  "web/banner/images" *> \out -> do
    liftIO $ createDirectoryIfMissing True "dist/web/banner"
    command_ [] "ln" ["-s", "-f", "../../dist/web/banner", out]

runWeb :: MkMode -> Action ()
runWeb m = do
  alwaysRerun
  needWeb

  -- work around weird bug(?)
  command_ [] "rm" ["-f", "dist/doc/doc"]
  command_ [] "rm" ["-f", "dist/web/gallery/gallery"]
  command_ [] "rm" ["-f", "dist/web/banner/banner"]
  command_ [] "rm" ["-f", "dist/blog/blog"]

  runExe [Cwd "web"] "Site"
    [ case m of
        BuildH  -> "build"
        Preview -> "watch"
    ]

needWeb :: Action ()
needWeb = do
  need [ "web/doc"
       , "web/blog"
       , "web/banner"
       , "web/gallery/images"
       , "web/banner/images"
       ]
  requireIcons
  requireStatic
  requireGallery
  requireBanner
  requireRst "doc"
  requireRst "blog"

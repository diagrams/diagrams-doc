{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Control.Concurrent         (getNumCapabilities)
import           Control.Monad              (when)
import           Data.List                  (isPrefixOf)
import           Data.Maybe                 (fromMaybe)
import           Development.Shake          hiding ((<//>))
import           Development.Shake.FilePath (dropDirectory1, dropExtension,
                                             takeBaseName, takeDirectory,
                                             (-<.>), (<.>), (</>))
import           Safe                       (readMay)
import           System.Console.CmdArgs
import           System.Directory           (createDirectoryIfMissing)
import           System.Environment         (lookupEnv)
import           System.Exit                (ExitCode (..))
import           System.Process             (system)

import           Text.Docutils.CmdLine      (DocutilOpts (..))

import qualified BuildBanner
import qualified BuildGallery
import qualified Xml2Html

obj, un, dist :: FilePath -> FilePath
obj  = (".make" </>)
un   = dropDirectory1
dist = ("dist" </>)

runExe :: [CmdOption] -> FilePath -> [String] -> Action ()
runExe options exe args = command_ options "stack" (["exec", exe, "--"] ++ args)

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
      , "rm -f .shake.database"
      ]

    _ -> shake shakeOptions { shakeThreads = numThreads } $ do
      ghcThreads <- newResource "GHC threads" 1

      action $ requireRst "doc"
      action $ requireRst "blog"
      action $ requireIcons
      action $ requireStatic
      action $ requireGallery
      action $ requireBanner

      dist "//*.html" %> \out -> do
        let xml = obj . un $ out -<.> "xml"
        need [xml]

        let opts = DocutilOpts
                     { outputDir  = takeDirectory out </> "images"
                     , sourceFile = xml
                     , destFile   = out
                     , keepGoing  = False
                     }

        withResource ghcThreads 1 $ liftIO $ do
          code <- Xml2Html.xml2Html opts
          case code of
            ExitSuccess -> return ()
            ExitFailure code ->
              fail ("Xml2Html exited with code " ++ show code ++ " for " ++ out)

      dist "blog/*.metadata" %> \out -> copyFile' (un out) out
      dist "doc/*.metadata"  %> \out -> copyFile' (un out) out

      obj "//*.xml" %> \out -> do
        let rst = un $ out -<.> "rst"
        need [rst]
        command_ [] "rst2xml" ["--input-encoding=utf8", rst, out]

      let
        makeIcon out = runExe [] exe ["-w", "40", "-h", "40", "-o", out]
          where exe = takeBaseName out

      dist ("doc/icons/Exercises" <.> imgExt) %> makeIcon
      dist ("doc/icons/ToWrite"   <.> imgExt) %> makeIcon
      dist ("doc/icons/Warning"   <.> imgExt) %> makeIcon

      copyFiles "doc/static"

      dist ("web/gallery/*.big" <.> imgExt) %> \out -> do
        need [dropExtension (un out) -<.> "lhs"]
        withResource ghcThreads 1 $ compileImg False out

      dist ("web/gallery/*.thumb" <.> imgExt) %> \out -> do
        need [dropExtension (un out) -<.> "lhs"]
        withResource ghcThreads 1 $ compileImg True out

      dist ("web/banner/banner" <.> imgExt) %> \out -> do
        need [dropExtension (un out) -<.> "hs"]
        let name = takeBaseName (takeBaseName out)
            hsName = "web/banner" </> name -<.> "hs"
        withResource ghcThreads 1 $ liftIO $ BuildBanner.compileExample hsName out

      when (m /= Build) (action $ runWeb m)

compileImg :: Bool -> FilePath -> Action ()
compileImg isThumb outPath = do
  let name    = takeBaseName . takeBaseName $ outPath
      lhsName = "web/gallery" </> name -<.> "lhs"
      thumb   = if isThumb then Just 200 else Nothing

  liftIO $ BuildGallery.compileExample thumb lhsName outPath

copyFiles :: FilePath -> Rules ()
copyFiles dir = dist (dir ++ "/*") %> \out -> copyFile' (un out) out

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

requireRst :: FilePath -> Action ()
requireRst dir = do
  rsts <- filter (not . (".#" `isPrefixOf`))
          <$> getDirectoryFiles dir ["*.rst"]
  need (map (dist . (dir </>) . (-<.> "html")) rsts)
  need (map (dist . (dir </>) . (-<.> "html.metadata")) rsts)

runWeb :: MkMode -> Action ()
runWeb m = do
  alwaysRerun

  requireRst "doc"
  requireRst "blog"
  requireIcons
  requireStatic
  requireGallery
  requireBanner

  liftIO $ createDirectoryIfMissing True "dist/web/gallery"
  liftIO $ createDirectoryIfMissing True "dist/web/banner"

  -- Use ../dist and ../../dist so that link is relative to the directory where
  -- it exists.
  command_ [] "ln" ["-s", "-f", "../dist/doc",            "web/doc"]
  command_ [] "ln" ["-s", "-f", "../dist/blog",           "web/blog"]
  command_ [] "ln" ["-s", "-f", "../../dist/web/gallery", "web/gallery/images"]
  command_ [] "ln" ["-s", "-f", "../../dist/web/banner",  "web/banner/images"]

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

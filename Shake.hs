{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Control.Concurrent          (getNumCapabilities)
import           Control.Monad               (mplus, when)
import           Control.Parallel.Strategies (NFData)
import           Data.Functor                ((<$>))
import           Data.List                   (isPrefixOf, (\\))
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


obj, un, dist :: FilePath -> FilePath
obj = (".make" <//>)
un = dropDirectory1
dist = ("dist" <//>)

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

newtype GhcPkg = GhcPkg ()
               deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

main :: IO ()
main = do
  m <- cmdArgs mkModes
  n <- getNumCapabilities

  threadsStr <- lookupEnv "DIA_DOC_THREADS"

  let Just numThreads = (threadsStr >>= readMay) `mplus` (Just $ max 2 (n - 1))

  putStrLn $ "Using " ++ show numThreads ++ " threads."

  -- Check for diagrams-rasterific
  rasterificPkg <- readProcess "ghc-pkg" ["list", "--simple-output", "diagrams-rasterific"] ""
  let useSVG = null rasterificPkg
      imgExt | useSVG    = "svg"
             | otherwise = "png"

  case useSVG of
    True  -> putStrLn $ "Falling back to SVG backend."
    False -> putStrLn $ "Using rasterific backend."

  case m of
    Clean -> mapM_ system
      [ "rm -rf web/_site"
      , "rm -rf web/_cache"
      , "rm -f web/doc"
      , "rm -f web/blog"
      , "rm -f web/gallery/images"
      , "rm -rf .make"
      , "rm -rf dist"
      , "rm -f Shake"
      , "rm -f Shake.o"
      , "rm -f Shake.hi"
      , "rm -f .shake.database"
      ]

    _ -> shake shakeOptions { shakeThreads = numThreads } $ do
      disk <- newResource "Disk" 4

      action $ requireRst "doc"
      action $ requireRst "blog"
      action $ requireIcons imgExt
      action $ requireStatic

      webRules

      dist "//*.html" *> \out -> do
        let xml = obj . un $ out -<.> "xml"
            exe = obj "doc/Xml2Html.hs.exe"
        need [xml, exe]
        command_ [] exe
          ([xml, "-o", takeDirectory out </> "images", out] ++ ["--keepgoing" | useSVG])

      dist "blog/*.metadata" *> \out -> copyFile' (un out) out
      dist "doc/*.metadata"  *> \out -> copyFile' (un out) out

      obj "//*.xml" *> \out -> do
        let rst = un $ out -<.> "rst"
        need [rst]
        command_ [] "rst2xml.py" ["--input-encoding=utf8", rst, out]

      obj "//*.hs.o" *> \out -> do
        let hs = un $ dropExtension out
        need [hs]
        ghc Compile useSVG disk out hs

      obj "//*.hs.exe" *> \out -> do
        let o  = out -<.> "o"
            hs = un $ dropExtension out
        need [hs,o]
        ghc Link useSVG disk out hs

      dist ("doc/icons/*" <.> imgExt) *> \out -> do
        let exe = obj . un $ out -<.> ".hs.exe"
        need [exe]
        command_ [] exe ["-w", "40", "-h", "40", "-o", out]

      copyFiles "doc/static"

      dist ("web/gallery/*.big" <.> imgExt) *> \out -> do
        need [dropExtension (un out) -<.> "lhs"]
        compileImg False out

      dist ("web/gallery/*.thumb" <.> imgExt) *> \out -> do
        need [dropExtension (un out) -<.> "lhs"]
        compileImg True out

      _ <- addOracle $ \(GhcPkg _) -> do
        Stdout out <- command [] "ghc-pkg" ["dump"]
        return $ words out

      when (m /= Build) (action $ runWeb m imgExt)

      return ()

compileImg :: Bool -> FilePath -> Action ()
compileImg isThumb outPath = do
    systemCwdNorm "web/gallery" (obj "web/gallery/BuildGallery.hs.exe")
      ( (if isThumb then [ "--thumb", "200" ] else [])
        ++ [(takeBaseName . takeBaseName) outPath, "../.." </> outPath]
      )

copyFiles :: String -> Rules ()
copyFiles dir = dist (dir ++ "/*") *> \out -> copyFile' (un out) out

requireIcons :: String -> Action ()
requireIcons imgExt = do
  hsIcons <- getDirectoryFiles "doc/icons" ["*.hs"]
  let icons = map (\i -> dist $ "doc/icons" </> i -<.> imgExt) hsIcons
  need icons

requireStatic :: Action ()
requireStatic = do
  staticSrc <- getDirectoryFiles "doc/static" ["*"]
  let static = map (dist . ("doc/static" </>)) staticSrc
  need static

-- A list of gallery examples which only build with rasterific, to be
-- excluded when building with SVG.
rasterificOnly :: [FilePath]
rasterificOnly = []

requireGallery :: String -> Action ()
requireGallery imgExt = do
  gallerySrc <- ( filter (not . (".#" `isPrefixOf`))
                . if imgExt == "svg" then (\\ rasterificOnly) else id
                )
                <$> getDirectoryFiles "web/gallery" ["*.lhs"]
  let imgs   = map (dist . ("web/gallery" </>) . (-<.> ("big" <.> imgExt))) gallerySrc
      thumbs = map (dist . ("web/gallery" </>) . (-<.> ("thumb" <.> imgExt))) gallerySrc
  need (imgs ++ thumbs)

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

runWeb :: MkMode -> String -> Action ()
runWeb m imgExt = do
  alwaysRerun
  needWeb imgExt

  -- work around weird bug(?)
  command_ [] "rm" ["-f", "dist/doc/doc"]
  command_ [] "rm" ["-f", "dist/web/gallery/gallery"]
  command_ [] "rm" ["-f", "dist/blog/blog"]

  systemCwdNorm "web" (obj "web/Site.hs.exe")
    [ case m of
        BuildH  -> "build"
        Preview -> "preview"
    ]

needWeb :: String -> Action ()
needWeb imgExt = do
  need [ "web/doc"
       , "web/blog"
       , "web/gallery/images"
       , obj "web/Site.hs.exe"
       , obj "web/gallery/BuildGallery.hs.exe"
       ]
  requireIcons imgExt
  requireStatic
  requireGallery imgExt
  requireRst "doc"
  requireRst "blog"

data GhcMode = Compile | Link
  deriving Eq

ghc :: GhcMode -> Bool -> Resource -> String -> String -> Action ()
ghc mode useSVG r out hs = do
  let odir = takeDirectory out
      base = (takeBaseName . takeBaseName) out
      mainIs | head base `elem` ['A'..'Z'] = ["-main-is", base]
             | otherwise                   = []

  -- Rebuild when the package database has changed
  _ <- askOracleWith (GhcPkg ()) [""]

  -- Run GHC, limiting to four linking invocations at a time
  resourced mode $ command_ [] "ghc" $
    concat
    [ ["--make", "-O2", "-outputdir", odir, "-o", out, "-osuf", "hs.o", hs]
    , mainIs
    , ["-c" | mode == Compile ]
    , ["-DUSE_SVG" | useSVG ]
    ]
  where
    resourced Link = withResource r 1
    resourced _    = id

systemCwdNorm :: FilePath -> FilePath -> [String] -> Action ()
systemCwdNorm path exe as = do
  path' <- liftIO (canonicalizePath path)
  exe'  <- liftIO (canonicalizePath exe)
  command_ [Cwd path'] exe' as

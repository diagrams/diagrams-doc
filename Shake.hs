{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Control.Concurrent          (getNumCapabilities)
import           Control.Monad               (when)
import           Control.Parallel.Strategies (NFData)
import           Data.Functor                ((<$>))
import           Data.List                   (isPrefixOf)
import           Development.Shake
import           Development.Shake.Classes   (Binary, Hashable)
import           Development.Shake.FilePath  (dropDirectory1, dropExtension,
                                              takeBaseName, takeDirectory,
                                              (-<.>), (</>))
import           System.Cmd                  (system)
import           System.Console.CmdArgs
import           System.Directory            (canonicalizePath,
                                              createDirectoryIfMissing)

obj, un, dist :: FilePath -> FilePath
obj = (".make" </>)
un = dropDirectory1
dist = ("dist" </>)

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
  case m of
    Clean -> mapM_ system
      [ "rm -rf web/_site"
      , "rm -rf web/_cache"
      , "rm -f web/doc"
      , "rm -f web/gallery/images"
      , "rm -rf .make"
      , "rm -rf dist"
      , "rm -f Shake"
      , "rm -f Shake.o"
      , "rm -f Shake.hi"
      , "rm -f .shake.database"
      ]

    _ -> shake shakeOptions { shakeThreads = 2 `max` (n - 1) } $ do
      action $ requireDoc
      action $ requireIcons
      action $ requireStatic

      webRules
      when (m /= Build) (action $ runWeb m)

      dist "doc/*.html" *> \out -> do
        let xml = obj . un $ out -<.> "xml"
            exe = obj "doc/xml2html.hs.exe"
        need [xml, exe]
        system' exe [xml, "-o", dist "doc/images", out]

      obj "//*.xml" *> \out -> do
        let rst = un $ out -<.> "rst"
        need [rst]
        system' "rst2xml" ["--input-encoding=utf8", rst, out]

      obj "//*.hs.exe" *> \out -> do
        let hs = un $ dropExtension out
        need [hs]
        ghc out hs

      dist "doc/icons/*.png" *> \out -> do
        let exe = obj . un $ out -<.> ".hs.exe"
        need [exe]
        system' exe ["-w", "40", "-h", "40", "-o", out]

      copyFiles "doc/static"

      dist "web/gallery/*.big.png" *> \out -> do
        need [dropExtension (un out) -<.> "lhs"]
        compilePng False out

      dist "web/gallery/*.thumb.png" *> \out -> do
        need [dropExtension (un out) -<.> "lhs"]
        compilePng True out

      addOracle $ \(GhcPkg _) -> do
        (out,_) <- systemOutput "ghc-pkg" ["dump"]
        return $ words out

      return ()

compilePng :: Bool -> FilePath -> Action ()
compilePng isThumb outPath = do
    systemCwdNorm "web/gallery" ("../.." </> obj "web/gallery/build-gallery.hs.exe")
      ( (if isThumb then [ "--thumb", "175" ] else [])
        ++ [(takeBaseName . takeBaseName) outPath, "../.." </> outPath]
      )

copyFiles :: String -> Rules ()
copyFiles dir = dist (dir ++ "/*") *> \out -> copyFile' (un out) out

requireIcons :: Action ()
requireIcons = do
  hsIcons <- getDirectoryFiles "doc/icons" ["*.hs"]
  let icons = map (\i -> dist $ "doc/icons" </> i -<.> "png") hsIcons
  need icons

requireStatic :: Action ()
requireStatic = do
  staticSrc <- getDirectoryFiles "doc/static" ["*"]
  let static = map (dist . ("doc/static" </>)) staticSrc
  need static

requireGallery :: Action ()
requireGallery = do
  gallerySrc <- filter (not . (".#" `isPrefixOf`))
                <$> getDirectoryFiles "web/gallery" ["*.lhs"]
  let pngs   = map (dist . ("web/gallery" </>) . (-<.> "big.png")) gallerySrc
      thumbs = map (dist . ("web/gallery" </>) . (-<.> "thumb.png")) gallerySrc
  need (pngs ++ thumbs)

requireDoc :: Action ()
requireDoc = do
  docs <- filter (not . (".#" `isPrefixOf`))
          <$> getDirectoryFiles "doc" ["*.rst"]
  need (map (dist . ("doc" </>) . (-<.> "html")) docs)

webRules :: Rules ()
webRules = do
  "web/doc" *> \out ->
    system' "ln" ["-s", "-f", "../dist/doc", out]

  "web/gallery/images" *> \out -> do
    liftIO $ createDirectoryIfMissing True "dist/web/gallery"
    system' "ln" ["-s", "-f", "../../dist/web/gallery", out]

runWeb :: MkMode -> Action ()
runWeb m = do
  alwaysRerun
  needWeb

  -- work around weird bug(?)
  system' "rm" ["-f", "dist/doc/doc"]
  system' "rm" ["-f", "dist/web/gallery/gallery"]

  systemCwdNorm "web" (".." </> obj "web/hakyll.hs.exe")
    [ case m of
        BuildH  -> "build"
        Preview -> "preview"
    ]

needWeb :: Action ()
needWeb = do
  need [ "web/doc"
       , "web/gallery/images"
       , obj "web/hakyll.hs.exe"
       , obj "web/gallery/build-gallery.hs.exe"
       ]
  requireIcons
  requireStatic
  requireGallery
  requireDoc

ghc :: String -> String -> Action ()
ghc out hs = do
  askOracleWith (GhcPkg ()) [""]
  let odir = takeDirectory out
      base = (takeBaseName . takeBaseName) out
      mainIs | head base `elem` ['A'..'Z'] = ["-main-is", base]
             | otherwise                   = []
  system' "ghc" (["--make", "-O2", "-outputdir", odir, "-o", out, hs] ++ mainIs)

systemCwdNorm :: FilePath -> FilePath -> [String] -> Action ()
systemCwdNorm path exe as = do
  path' <- liftIO (canonicalizePath path)
  exe'  <- liftIO (canonicalizePath exe)
  systemCwd path' exe' as

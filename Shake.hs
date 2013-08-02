{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import           System.Directory            (createDirectoryIfMissing)

obj, un, dist :: FilePath -> FilePath
obj = (".make" </>)
un = dropDirectory1
dist = ("dist" </>)

data MkMode =
    Build
  | Preview
  | Clean
  deriving (Show, Data, Typeable)

mkModes :: MkMode
mkModes = modes
  [ Build &= auto
  , Preview
  , Clean
  ]

newtype GhcPkg = GhcPkg ()
               deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

main :: IO ()
main = do
  m <- cmdArgs mkModes
  case m of
    Clean -> mapM_ system
      [ "rm -rf web/_site"
      , "rm -rf web/_cache"
      , "rm -f web/manual"
      , "rm -f web/gallery/images"
      , "rm -rf .make"
      , "rm -rf dist"
      , "rm -f Shake"
      , "rm -f Shake.o"
      , "rm -f Shake.hi"
      , "rm .shake.database"
      , "rm -f web/gallery/Build.{hi,o}"
      ]
    _ -> shake shakeOptions { shakeThreads = 2
--                          , shakeVerbosity = Chatty
                            } $ do
      want [dist "manual/diagrams-manual.html"]
      action $ requireIcons
      action $ requireStatic

      webRules
      action $ runWeb m

      -- Cheating a bit here; this rule also generates diagrams images,
      -- BUT we don't even know their names until after running xml2html,
      -- which generates diagrams-manual.html as well.  So we have to be
      -- careful to call 'requireImages' *after* running xml2html.
      dist "manual/diagrams-manual.html" *> \out -> do
        let xml = obj . un $ out -<.> "xml"
            exe = obj "manual/xml2html.hs.exe"
        need [xml, exe]
        system' exe [xml, "-o", obj "manual/images", out]
        requireImages

      obj "//*.xml" *> \out -> do
        let rst = un $ out -<.> "rst"
        need [rst]
        system' "rst2xml" ["--input-encoding=utf8", rst, out]

      obj "//*.hs.exe" *> \out -> do
        let hs = un $ dropExtension out
        need [hs]
        ghc out hs

      dist "manual/icons/*.png" *> \out -> do
        let exe = obj . un $ out -<.> ".hs.exe"
        need [exe]
        system' exe ["-w", "40", "-h", "40", "-o", out]

      copyFiles "manual/static"
      copyImages

      dist "web/gallery/*.big.png" *> compilePng False

      dist "web/gallery/*.thumb.png" *> compilePng True

      addOracle $ \(GhcPkg _) -> do
        (out,_) <- systemOutput "ghc-pkg" ["dump"]
        return $ words out

      return ()

compilePng :: Bool -> FilePath -> Action ()
compilePng isThumb outPath = do
    systemCwd "web/gallery" ("../.." </> obj "web/gallery/Build.hs.exe")
      ( (if isThumb then [ "--thumb", "175" ] else [])
        ++ [(takeBaseName . takeBaseName) outPath, "../.." </> outPath]
      )

copyFiles :: String -> Rules ()
copyFiles dir = dist (dir ++ "/*") *> \out -> copyFile' (un out) out

copyImages :: Rules ()
copyImages = dist ("manual/images/*") *> \out -> copyFile' (obj . un $ out) out

requireIcons :: Action ()
requireIcons = do
  hsIcons <- getDirectoryFiles "manual/icons" ["*.hs"]
  let icons = map (\i -> dist $ "manual/icons" </> i -<.> "png") hsIcons
  need icons

requireStatic :: Action ()
requireStatic = do
  staticSrc <- getDirectoryFiles "manual/static" ["*"]
  let static = map (dist . ("manual/static" </>)) staticSrc
  need static

requireImages :: Action ()
requireImages = do
  images <- getDirectoryFiles (obj "manual/images") ["*.png"]
  let distImages = map (dist . ("manual/images" </>)) images
  need distImages

requireGallery :: Action ()
requireGallery = do
  gallerySrc <- filter (not . (".#" `isPrefixOf`))
                <$> getDirectoryFiles "web/gallery" ["*.lhs"]
  let pngs   = map (dist . ("web/gallery" </>) . (-<.> "big.png")) gallerySrc
      thumbs = map (dist . ("web/gallery" </>) . (-<.> "thumb.png")) gallerySrc
  need (pngs ++ thumbs)

  -- XXX add rule to link them into the real web dir?

webRules :: Rules ()
webRules = do
  "web/manual" *> \out ->
    system' "ln" ["-s", "-f", "../dist/manual", out]

  "web/gallery/images" *> \out -> do
    liftIO $ createDirectoryIfMissing True "dist/web/gallery"
    system' "ln" ["-s", "-f", "../../dist/web/gallery", out]

runWeb :: MkMode -> Action ()
runWeb m = do
  alwaysRerun
  needWeb

  -- work around weird bug(?)
  system' "rm" ["-f", "dist/manual/manual"]
  system' "rm" ["-f", "dist/web/gallery/gallery"]

  systemCwd "web" (".." </> obj "web/hakyll.hs.exe")
    [ case m of
        Build   -> "build"
        Preview -> "preview"
    ]

needWeb :: Action ()
needWeb = do
  need [ "web/manual"
       , "web/gallery/images"
       , dist "manual/diagrams-manual.html"
       , obj "web/hakyll.hs.exe"
       , obj "web/gallery/Build.hs.exe"
       ]
  requireIcons
  requireStatic
  requireImages
  requireGallery

ghc :: String -> String -> Action ()
ghc out hs = do
  askOracleWith (GhcPkg ()) [""]
  let odir = takeDirectory out
  system' "ghc" ["--make", "-O2", "-outputdir", odir, "-o", out, hs]

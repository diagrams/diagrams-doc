#! /usr/bin/env stack
{-
  stack runghc
    --resolver lts-20.23

    --package basic-prelude
    --package process
    --package yaml
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           BasicPrelude
import qualified Data.Text         as Text
import           Data.Yaml.Builder ((.=))
import qualified Data.Yaml.Builder as Yaml
import qualified System.Process    as Process

data Dep = Dep
  { name :: Text
  , sha  :: Text
  }

main :: IO ()
main = do
  deps <- depsWithShas
  Yaml.writeYamlFile "stack.yaml" (mkYamlFile deps)

mkYamlFile :: [Dep] -> Yaml.YamlBuilder
mkYamlFile deps = Yaml.mapping
  [ "resolver"   .= Yaml.string "lts-20.23"
  , "extra-deps" .= Yaml.array (
    (map Yaml.string
    [ "tuple-0.3.0.2"
    , "cubicbezier-0.6.0.6"
    , "mfsolve-0.3.2.1"
    , "haskell-src-exts-1.23.1"
    , "haskell-src-exts-simple-1.23.0.0"
    , "fast-math-1.0.2"
    , "pandoc-3.1.2"
    , "pandoc-types-1.23"
    , "force-layout-0.4.0.6"
    , "svg-builder-0.1.1"
    , "SVGFonts-1.8.0.1"
    , "hakyll-4.16.0.0"
    , "palette-0.3.0.2"
    , "active-0.2.0.17"
    , "citeproc-0.8.1"
    , "doctemplates-0.11"
    , "skylighting-0.13.2.1"
    , "skylighting-core-0.13.2.1"
    ])
    ++
    map mkDepObject deps
    )
  , "packages"   .= Yaml.array [Yaml.string "."]
  ]

      -- : Yaml.mapping
      --     [ "location" .= Yaml.mapping
      --       [ "git"    .= Yaml.string "git@github.com:st3ll1s/haddock.git"
      --       , "commit" .= Yaml.string "b3912d70f74b0693f1ea8cffb8f547b1303ef325"
      --       ]
      --     , "subdirs" .= Yaml.array
      --       [ Yaml.string "haddock-library" ]
      --     , "extra-dep" .= Yaml.bool True
      --     ]
      -- : map mkDepObject deps
      -- )

  {- - location:
    git: git@github.com:st3ll1s/haddock.git
    commit: b3912d70f74b0693f1ea8cffb8f547b1303ef325
  subdirs:
    - haddock-library
  extra-dep: true

-}
  where
    mkDepObject :: Dep -> Yaml.YamlBuilder
    mkDepObject (Dep name sha) =
      Yaml.mapping
      [ "git"    .= Yaml.string ("https://github.com/diagrams/" <> name)
      , "commit" .= Yaml.string sha
      ]

depsWithShas :: IO [Dep]
depsWithShas =
  forM repoNames $ \repoName -> do
    [sha, _] <- words . Text.pack <$> Process.readProcess "git" ["ls-remote", textToString $ "git://github.com/diagrams/" <> repoName, "master"] ""
    return (Dep repoName sha)

repoNames :: [Text]
repoNames =
  [ "diagrams-builder"
  , "diagrams-haddock"
  , "diagrams-core"
  , "diagrams-lib"
  , "diagrams-rasterific"
  , "diagrams-svg"
  , "diagrams-solve"
  , "diagrams-contrib"
  , "docutils"
  , "dual-tree"
  , "monoid-extras"
  ]

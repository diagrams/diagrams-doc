#! /usr/bin/env stack
{-
  stack runghc
    --resolver lts-5.17

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
  [ "resolver"   .= Yaml.string "lts-6.22"
  , "extra-deps" .= (Yaml.array . map Yaml.string $
    [ "tuple-0.3.0.2"
    , "OneTuple-0.2.1"
    , "SVGFonts-1.6.0.0"
    , "cubicbezier-0.4.0.2"
    , "mfsolve-0.3.2.0"
    , "haskell-src-exts-1.18.2"
    , "haskell-src-exts-simple-1.18.0.1.1"
    , "optparse-applicative-0.13.0.0"
    , "svg-builder-0.1.0.1"
    ])
  , "packages"   .= Yaml.array (Yaml.string "." : map mkDepObject deps)
  ]
  where
    mkDepObject :: Dep -> Yaml.YamlBuilder
    mkDepObject (Dep name sha) =
      Yaml.mapping
        [ "location" .= Yaml.mapping
          [ "git"    .= Yaml.string ("https://github.com/diagrams/" <> name)
          , "commit" .= Yaml.string sha
          ]
        , "extra-dep" .= Yaml.bool True
        ]

depsWithShas :: IO [Dep]
depsWithShas =
  forM repoNames $ \repoName -> do
    [sha, _] <- words . Text.pack <$> Process.readProcess "git" ["ls-remote", textToString $ "git://github.com/diagrams/" <> repoName, "master"] ""
    return (Dep repoName sha)

repoNames :: [Text]
repoNames =
  [ "active"
  , "diagrams-builder"
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
  , "statestack"
  ]

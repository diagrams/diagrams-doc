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
  [ "resolver"   .= Yaml.string "lts-5.17"
  , "extra-deps" .= Yaml.array [ Yaml.string "tuple-0.3.0.2"
                               , Yaml.string "OneTuple-0.2.1"
                               , Yaml.string "SVGFonts-1.5.0.0"
                               ]
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
  , "diagrams-core"
  , "diagrams-lib"
  , "diagrams-rasterific"
  , "diagrams-solve"
  , "diagrams-contrib"
  , "docutils"
  , "dual-tree"
  , "monoid-extras"
  , "statestack"
  ]

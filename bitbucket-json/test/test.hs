{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import qualified Test.Irreverent.Bitbucket.Json

import Lab.Core.Main

import Preamble

main :: IO ()
main = labMain
    [ Test.Irreverent.Bitbucket.Json.tests
    ]

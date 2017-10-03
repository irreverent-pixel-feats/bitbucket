{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Cli.Main
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Cli.Main (bitbMain) where

import BuildInfo_bitb

import Irreverent.Bitbucket.Cli.Commands.ListRepos

import Irreverent.Bitbucket.Core (Username)
import Irreverent.Bitbucket.Core.Data.Auth (Auth)
import Irreverent.Bitbucket.Options (authP, ownerP)

import Ultra.Cli
import qualified Ultra.Data.Text as T
import Ultra.Options.Applicative (
    Parser
  , command'
  , commandParser
  , parseAndRun
  )

import Data.Monoid ((<>))

import System.Environment (getEnvironment)
import System.IO (putStrLn)

import Preamble hiding ((<>))

versionString :: String
versionString = "bitb: " <> buildInfoVersion

data Command =
    Version
  | ListRepos !Auth !Username
    deriving (Show, Eq)

foldCommand
  :: a
  -> (Auth -> Username -> a)
  -> Command
  -> a
foldCommand v ls = \case
  Version             -> v
  ListRepos auth user -> ls auth user

commandParser' :: [(T.Text, T.Text)] -> Parser Command
commandParser' env = commandParser Version [
    command' "ls" "List Repos" (ListRepos <$> authP env <*> ownerP "lists repos owned by this owner")
  ]

runCommand :: Command -> IO ()
runCommand =
  renderErrorAndDie renderListReposError . foldCommand (lift printVersion) listRepos

printVersion :: IO ()
printVersion = putStrLn versionString

bitbMain :: IO ()
bitbMain = do
  env <- fmap (bimap T.pack T.pack) <$> getEnvironment
  parseAndRun "bitb" "Bitbucket cli" (commandParser' env) runCommand

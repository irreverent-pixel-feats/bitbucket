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

import BuildInfo_irreverent_bitbucket_cli

import Irreverent.Bitbucket.Cli.Commands.ListRepos

import Irreverent.Bitbucket.Core (GitURLType(..), RepoName(..), Username(..))
import Irreverent.Bitbucket.Core.Data.Auth (Auth)
import Irreverent.Bitbucket.Options (authP, ownerP, repoNameP, gitURLTypeP)

import Ultra.Cli
import qualified Ultra.Data.Text as T
import qualified Ultra.Data.Text.Encoding as T
import Ultra.Options.Applicative (
    Parser
  , command'
  , commandParser
  , parseAndRun
  )

import qualified Data.ByteString as BS
import Data.Monoid ((<>))

import System.Environment (getEnvironment)
import System.IO (putStrLn, stdout)

import Preamble hiding ((<>))

versionString :: String
versionString = "bitb: " <> buildInfoVersion

data Command =
    Version
  | ListRepos !Auth !Username
  | GitURL !GitURLType !Username !RepoName
    deriving (Show, Eq)

foldCommand
  :: a
  -> (Auth -> Username -> a)
  -> (GitURLType -> Username -> RepoName -> a)
  -> Command
  -> a
foldCommand v ls giturl = \case
  Version               -> v
  ListRepos auth user   -> ls auth user
  GitURL urlt user repo -> giturl urlt user repo

commandParser' :: [(T.Text, T.Text)] -> Parser Command
commandParser' env = commandParser Version [
    command' "ls" "List Repos" (ListRepos <$> authP env <*> ownerP "lists repos owned by this owner")
  , command' "git-url" "Git URL" (GitURL <$> gitURLTypeP <*> ownerP "The owner/org for the desired project" <*> repoNameP "The project you want the git url for")
  ]

runCommand :: Command -> IO ()
runCommand =
  renderErrorAndDie renderListReposError . foldCommand (lift printVersion) listRepos printGitURL

gitURL
  :: GitURLType
  -> Username
  -> RepoName
  -> T.Text
gitURL HTTPSGitURLType (Username user) (RepoName repo) = T.concat ["https://bitbucket.org/", user, "/", repo, ".git"]
gitURL SSHGitURLType (Username user) (RepoName repo) = T.concat ["git@bitbucket.org:", user, "/", repo, ".git"]

printGitURL
  :: (MonadIO m)
  => GitURLType
  -> Username
  -> RepoName
  -> m ()
printGitURL typ user repo =
  liftIO . BS.hPutStr stdout . T.encodeUtf8 $ gitURL typ user repo

printVersion :: IO ()
printVersion = putStrLn versionString

bitbMain :: IO ()
bitbMain = do
  env <- fmap (bimap T.pack T.pack) <$> getEnvironment
  parseAndRun "bitb" "Bitbucket cli" (commandParser' env) runCommand

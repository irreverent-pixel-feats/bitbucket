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
import Irreverent.Bitbucket.Cli.Commands.CreateRepo
import Irreverent.Bitbucket.Cli.Commands.PipelineConfig
import Irreverent.Bitbucket.Cli.Commands.UpdatePipelineConfig
import Irreverent.Bitbucket.Cli.Error

import Irreverent.Bitbucket.Core (GitURLType(..), RepoName(..), Username(..))
import Irreverent.Bitbucket.Core.Data.Auth (Auth)
import Irreverent.Bitbucket.Core.Data.NewRepository (NewRepository(..))
import Irreverent.Bitbucket.Core.Data.Pipelines.UpdateConfig (UpdatePipelinesConfig(..))
import Irreverent.Bitbucket.Options (
    authP
  , newRepoP
  , ownerP
  , ownerArgP
  , repoNameP
  , repoNameArgP
  , gitURLTypeP
  , pipelineCfgUpdateP
  )

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
  | NewRepo !Auth !Username !RepoName !NewRepository
  | UpdatePipelineCfg !Auth !Username !RepoName !UpdatePipelinesConfig
  | GetPipelineCfg !Auth !Username !RepoName
    deriving (Show, Eq)

foldCommand
  :: a
  -> (Auth -> Username -> a)
  -> (GitURLType -> Username -> RepoName -> a)
  -> (Auth -> Username -> RepoName -> NewRepository -> a)
  -> (Auth -> Username -> RepoName -> UpdatePipelinesConfig -> a)
  -> (Auth -> Username -> RepoName -> a)
  -> Command
  -> a
foldCommand v ls giturl newrepo updatePipelinesCfg' getPipelineCfg' = \case
  Version                      -> v
  ListRepos auth user          -> ls auth user
  GitURL urlt user repo        -> giturl urlt user repo
  NewRepo auth user rname repo -> newrepo auth user rname repo
  UpdatePipelineCfg auth user rname cfg -> updatePipelinesCfg' auth user rname cfg
  GetPipelineCfg auth user rname -> getPipelineCfg' auth user rname

commandParser' :: [(T.Text, T.Text)] -> Parser Command
commandParser' env = commandParser Version [
    command' "ls" "List Repos" (ListRepos <$> authP env <*> ownerP "lists repos owned by this owner")
  , command' "git-url" "Git URL" (GitURL <$> gitURLTypeP <*> ownerArgP "The owner/org for the desired project" <*> repoNameArgP "The project you want the git url for")
  , command' "new-repo" "Create a new repository" (NewRepo <$> authP env <*> ownerP "The user/org to create this repo under" <*> repoNameP "The full name for the repo" <*> newRepoP)
  , command' "get-pipelines-cfg" "Retrieve the Pipelines settings for a repo" (GetPipelineCfg <$> authP env <*> ownerP "The user/org that owns the repo" <*> repoNameP "The name of the repo for the pipeline")
  , command' "update-pipelines-cfg" "Update the Pipelines settings for a repo" (UpdatePipelineCfg <$> authP env <*> ownerP "The user/org that owns the repo" <*> repoNameP "The name of the repo for the pipeline" <*> pipelineCfgUpdateP)
  ]

runCommand :: Command -> IO ()
runCommand =
  renderErrorAndDie renderCliError . foldCommand (lift printVersion) listRepos printGitURL newRepo updatePipelineCfg getPipelineCfg

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
  parseAndRun "bitb: Dom De Re" "Bitbucket cli" (commandParser' env) runCommand

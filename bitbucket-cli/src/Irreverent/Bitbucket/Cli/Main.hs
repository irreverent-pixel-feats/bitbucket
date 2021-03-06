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

import Irreverent.Bitbucket.Cli.Commands.AddAccessKey
import Irreverent.Bitbucket.Cli.Commands.ListRepos
import Irreverent.Bitbucket.Cli.Commands.CreateRepo
import Irreverent.Bitbucket.Cli.Commands.PipelineConfig
import Irreverent.Bitbucket.Cli.Commands.UpdatePipelineConfig
import Irreverent.Bitbucket.Cli.Commands.SetGroupPrivs
import Irreverent.Bitbucket.Cli.Commands.SetPipelineEnvironmentVariable
import Irreverent.Bitbucket.Cli.Commands.SetPipelineSSHKeys
import Irreverent.Bitbucket.Cli.Commands.DeletePipelineEnvironmentVariable
import Irreverent.Bitbucket.Cli.Commands.GetPipelineEnvironmentVariables
import Irreverent.Bitbucket.Cli.Error

import Irreverent.Bitbucket.Core (
    GitURLType(..)
  , GroupName(..)
  , GroupOwner(..)
  , PrivilegeLevel(..)
  , RepoName(..)
  , RepoSlug(..)
  , Username(..)
  )
import Irreverent.Bitbucket.Core.Data.Auth (Auth)
import Irreverent.Bitbucket.Core.Data.NewRepository (NewRepository(..))
import Irreverent.Bitbucket.Core.Data.Pipelines.UpdateConfig (UpdatePipelinesConfig(..))
import Irreverent.Bitbucket.Core.Data.Pipelines.NewEnvironmentVariable (NewPipelinesEnvironmentVariable(..))
import Irreverent.Bitbucket.Options (
    authP
  , envvarNameToDeleteP
  , forceEnvVarSetP
  , newRepoP
  , ownerP
  , ownerArgP
  , repoNameP
  , repoNameArgP
  , repoSlugP
  , gitURLTypeP
  , groupNameP
  , groupOwnerP
  , newPipelineEnvVarP
  , pipelineCfgUpdateP
  , accessKeyLabelP
  , privateKeyPathP
  , privilegeLevelP
  , publicKeyPathP
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

import System.Environment (getEnvironment)
import System.IO (putStrLn, stdout)

import Preamble hiding ((<>))

versionString :: String
versionString = buildInfoVersion

data Command =
    Version
  | ListRepos !Auth !Username
  | GitURL !GitURLType !Username !RepoName
  | NewRepo !Auth !Username !RepoName !NewRepository
  | UpdatePipelineCfg !Auth !Username !RepoName !UpdatePipelinesConfig
  | GetPipelineCfg !Auth !Username !RepoName
  | SetPipelineEnvironmentVariable !Auth !Username !RepoName !(Maybe ()) !NewPipelinesEnvironmentVariable
  | GetPipelineEnvironmentVariables !Auth !Username !RepoName
  | RmPipelineEnvironmentVariable !Auth !Username !RepoName !T.Text
  | AddAccessKey !Auth !Username !RepoName !(Maybe T.Text) T.Text
  | SetPipelineSSHKeys !Auth !Username !RepoName !T.Text T.Text
  | SetGroupPrivs !Auth !Username !RepoSlug !GroupOwner !GroupName !PrivilegeLevel
    deriving (Show, Eq)

foldCommand
  :: a
  -> (Auth -> Username -> a)
  -> (GitURLType -> Username -> RepoName -> a)
  -> (Auth -> Username -> RepoName -> NewRepository -> a)
  -> (Auth -> Username -> RepoName -> UpdatePipelinesConfig -> a)
  -> (Auth -> Username -> RepoName -> a)
  -> (Auth -> Username -> RepoName -> Maybe () -> NewPipelinesEnvironmentVariable -> a)
  -> (Auth -> Username -> RepoName -> a)
  -> (Auth -> Username -> RepoName -> T.Text -> a)
  -> (Auth -> Username -> RepoName -> Maybe T.Text -> T.Text -> a)
  -> (Auth -> Username -> RepoName -> T.Text -> T.Text -> a)
  -> (Auth -> Username -> RepoSlug -> GroupOwner -> GroupName -> PrivilegeLevel -> a)
  -> Command
  -> a
foldCommand v ls giturl newrepo updatePipelinesCfg' getPipelineCfg' addEnvvar' pipelineEnvs' rmPipelineEnv' addAccessKey' setPipelineSSHKeys' setGroupPriv' = \case
  Version                      -> v
  ListRepos auth user          -> ls auth user
  GitURL urlt user repo        -> giturl urlt user repo
  NewRepo auth user rname repo -> newrepo auth user rname repo
  UpdatePipelineCfg auth user rname cfg -> updatePipelinesCfg' auth user rname cfg
  GetPipelineCfg auth user rname -> getPipelineCfg' auth user rname
  SetPipelineEnvironmentVariable auth user rname force var -> addEnvvar' auth user rname force var
  GetPipelineEnvironmentVariables auth user rname -> pipelineEnvs' auth user rname
  RmPipelineEnvironmentVariable auth user rname var -> rmPipelineEnv' auth user rname var
  AddAccessKey auth user rname label keyPath -> addAccessKey' auth user rname label keyPath
  SetPipelineSSHKeys auth user rname privPath pubPath -> setPipelineSSHKeys' auth user rname privPath pubPath
  SetGroupPrivs auth owner rname gowner grp priv -> setGroupPriv' auth owner rname gowner grp priv

commandParser' :: [(T.Text, T.Text)] -> Parser Command
commandParser' env = commandParser Version [
    command' "ls" "List Repos" (ListRepos <$> authP env <*> ownerP "lists repos owned by this owner")
  , command' "git-url" "Git URL" (GitURL <$> gitURLTypeP <*> ownerArgP "The owner/org for the desired project" <*> repoNameArgP "The project you want the git url for")
  , command' "new-repo" "Create a new repository" (NewRepo <$> authP env <*> ownerP "The user/org to create this repo under" <*> repoNameP "The full name for the repo" <*> newRepoP)
  , command' "get-pipelines-cfg" "Retrieve the Pipelines settings for a repo" (GetPipelineCfg <$> authP env <*> ownerP "The user/org that owns the repo" <*> repoNameP "The name of the repo for the pipeline")
  , command' "update-pipelines-cfg" "Update the Pipelines settings for a repo" (UpdatePipelineCfg <$> authP env <*> ownerP "The user/org that owns the repo" <*> repoNameP "The name of the repo for the pipeline" <*> pipelineCfgUpdateP)
  , command' "set-envvar" "Sets an Environment Variable to a pipeline configuration" (SetPipelineEnvironmentVariable <$> authP env <*> ownerP "The user/org that owns the repo" <*> repoNameP "The name of the repo for the pipeline" <*> forceEnvVarSetP <*> newPipelineEnvVarP)
  , command' "ls-envvar" "List all the Environment Variables for a bitbucket pipeline" (GetPipelineEnvironmentVariables <$> authP env <*> ownerP "The user/org that owns the repo" <*> repoNameP "The name of the repo for the pipeline")
  , command' "rm-envvar" "Remove an Environment Variable from a bitbucket pipeline" (RmPipelineEnvironmentVariable <$> authP env <*> ownerP "The user/org that owns the repo" <*> repoNameP "The name of the repo for the pipeline" <*> envvarNameToDeleteP)
  , command' "add-access-key" "Add an SSH Access Key for the repo for deploys or CI access to the repo" (AddAccessKey <$> authP env <*> ownerP "The user/org that owns the repo" <*> repoNameP "The repo to add the Access Key to" <*> optional accessKeyLabelP <*> publicKeyPathP)
  , command' "set-pipeline-ssh-keys" "Set the SSH keys for the bitbucket pipelines build environment" (SetPipelineSSHKeys <$> authP env <*> ownerP "The user/org that owns the repo" <*> repoNameP "The repo for the pipeline you want to add the keys to" <*> privateKeyPathP <*> publicKeyPathP)
  , command' "set-group-privilege" "Sets the group privilege on a repository" (SetGroupPrivs <$> authP env <*> ownerP "The user/org that owns the repo the privilege is concerned with" <*> repoSlugP "The repo for the privilege" <*> groupOwnerP "The org that owns the group for the privilege" <*> groupNameP "The group for the privilege" <*> privilegeLevelP)
  ]

runCommand :: Command -> IO ()
runCommand =
  renderErrorAndDie renderCliError . foldCommand (lift printVersion) listRepos printGitURL newRepo updatePipelineCfg getPipelineCfg setPipelineEnvironmentVariable listPipelineEnvironmentVariables deletePipelineEnvironmentVariable addAccessKey setPipelinesSSHKeys addGroupPrivilege

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

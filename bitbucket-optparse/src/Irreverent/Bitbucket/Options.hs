{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Options
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Options (
  -- * Parsers
    ownerP
  , ownerArgP
  , repoNameP
  , repoNameArgP
  , authP
  , envvarNameToDeleteP
  , forceEnvVarSetP
  , gitURLTypeP
  , newRepoP
  , newPipelineEnvVarP
  , pipelineCfgUpdateP
  , privateKeyPathP
  , publicKeyPathP
  , accessKeyLabelP
  ) where

import Irreverent.Bitbucket.Core.Data.Common (
    Username(..)
  , ForkPolicy(..)
  , GitURLType(..)
  , HasWiki(..)
  , HasIssues(..)
  , Language(..)
  , PipelinesEnvironmentVariableSecurity(..)
  , Privacy(..)
  , ProjectKey(..)
  , RepoDescription(..)
  , RepoName(..)
  , Scm(..)
  )

import Irreverent.Bitbucket.Core.Data.Auth (Auth(..))
import Irreverent.Bitbucket.Core.Data.NewRepository (NewRepository(..))
import Irreverent.Bitbucket.Core.Data.Pipelines.NewEnvironmentVariable (NewPipelinesEnvironmentVariable(..))
import Irreverent.Bitbucket.Core.Data.Pipelines.UpdateConfig (UpdatePipelinesConfig(..))

import qualified Ultra.Data.Text as T
import Ultra.Options.Applicative (
    Parser
  , argument
  , eitherTextReader
  , envvar
  , flag
  , flag'
  , help
  , long
  , metavar
  , option
  , short
  , str
  )

import Data.Monoid ((<>))

import Preamble hiding ((<>))

httpsURLTypeP :: Parser GitURLType
httpsURLTypeP = flag' HTTPSGitURLType $
      long "https"
  <>  help "Provide HTTPS Git URL"

sshGitURLTypeP :: Parser GitURLType
sshGitURLTypeP = flag' SSHGitURLType $
      long "ssh"
  <>  help "Provide SSH Git URL"

gitURLTypeP :: Parser GitURLType
gitURLTypeP = httpsURLTypeP <|> sshGitURLTypeP

ownerP :: T.Text -> Parser Username
ownerP htext = option (Username . T.pack <$> str) $
      short 'o'
  <>  long "owner"
  <>  help (T.unpack htext)

ownerArgP :: T.Text -> Parser Username
ownerArgP htext = argument (Username . T.pack <$> str) $
      metavar "OWNER"
  <>  help (T.unpack htext)

repoNameP :: T.Text -> Parser RepoName
repoNameP htext = option (RepoName . T.pack <$> str) $
      short 'r'
  <>  long "repository"
  <>  help (T.unpack htext)

repoNameArgP :: T.Text -> Parser RepoName
repoNameArgP htext = argument (RepoName . T.pack <$> str) $
      metavar "REPONAME"
  <>  help (T.unpack htext)

authP :: [(T.Text, T.Text)] -> Parser Auth
authP env = option (eitherTextReader authReader) $
      short 'a'
  <>  long "auth"
  <>  envvar (either (const Nothing) pure . authReader) env "BITBUCKET_API_AUTH" "Bitbucket app password login information, in the form of \"username:password\""

repoDescP :: Parser RepoDescription
repoDescP = option (RepoDescription . T.pack <$> str) $
      short 'd'
  <>  long "description"
  <>  metavar "DESCRIPTION"
  <>  help "Description to use for the repo"

gitScmP :: Parser Scm
gitScmP = flag' Git $
      long "git"
  <>  help "Sets the Scm to Git (Default)"

mercurialScmP :: Parser Scm
mercurialScmP = flag' Mercurial $
      short 'm'
  <>  long "mercurial"
  <>  help "Sets the Scm to Mercurial"

scmP :: Parser Scm
scmP = gitScmP <|> mercurialScmP <|> pure Git 

projectKeyP :: Parser ProjectKey
projectKeyP = option (ProjectKey . T.pack <$> str) $
      short 'p'
  <>  long "project"
  <>  metavar "PROJECT_KEY"
  <>  help "The key for the project you wish to assign this repo to"

enumWithDefaultP :: forall a. (Eq a) => a -> NonEmpty (a, T.Text, T.Text) -> Parser a
enumWithDefaultP def =
  let
    p :: a -> T.Text -> T.Text -> Parser a
    p fp long' help' = flag' fp $
          long (T.unpack long')
      <>  help (T.unpack help')
    f :: (a, T.Text, T.Text) -> Parser a -> Parser a
    f (x, long', help') parser =
      p x long' (help' <> (if x == def then " (Default)" else "")) <|> parser
  in foldr f (pure def)

forkPolicyP :: Parser ForkPolicy
forkPolicyP = enumWithDefaultP NoPublicForksPolicy
  [ (NoForksPolicy, "no-forks", "Allows No Forks on this repo")
  , (NoPublicForksPolicy, "no-public-forks", "Allows only private forks on the project")
  , (ForkAwayPolicy, "forks", "Allows public and private forks")
  ]

privacyP :: Parser Privacy
privacyP = enumWithDefaultP Private
  [ (Public, "public", "Repo will be public")
  , (Private, "private", "Repo will be private")
  ]

languageP :: Parser Language
languageP = option (Language . T.pack <$> str) $
      short 'l'
  <>  long "lang"
  <>  metavar "LANGUAGE"
  <>  help "The predominant language that is to be used"

hasWikiP :: Parser HasWiki
hasWikiP = enumWithDefaultP NoWiki
 [ (HasWiki, "has-wiki", "A Wiki will be created for this repo")
 , (NoWiki, "no-wiki", "No Wiki will be created for this wiki, if there previously was one, it will be deleted")
 ]

hasIssuesP :: Parser HasIssues
hasIssuesP = enumWithDefaultP HasIssues
  [ (HasIssues, "has-issues", "Issue tracking will be setup for this repo")
  , (NoIssues, "no-issues", "There will be no issue tracking for this repo")
  ]

newRepoP :: Parser NewRepository
newRepoP = NewRepository
  <$> repoDescP
  <*> scmP
  <*> optional projectKeyP
  <*> forkPolicyP
  <*> privacyP
  <*> languageP
  <*> hasWikiP
  <*> hasIssuesP

pipelineCfgUpdateP :: Parser UpdatePipelinesConfig
pipelineCfgUpdateP = UpdatePipelinesConfig
  <$> pipelineEnabledP

enablePipelineP :: Parser Bool
enablePipelineP = flag' True $
      long "enabled"
  <>  help "Turns the pipeline on"

disablePipelineP :: Parser Bool
disablePipelineP = flag' False $
      long "disabled"
  <>  help "Turns the pipeline off"

pipelineEnabledP :: Parser Bool
pipelineEnabledP = enablePipelineP <|> disablePipelineP

authReader :: T.Text -> Either T.Text Auth
authReader t = case T.splitOn ":" t of
  [] -> Left t
  username:password -> pure . Basic username $ T.intercalate ":" password

newPipelineEnvVarP :: Parser NewPipelinesEnvironmentVariable
newPipelineEnvVarP = NewPipelinesEnvironmentVariable
  <$> envvarNameP
  <*> envvarValueP
  <*> envvarSecurityP

forceEnvVarSetP :: Parser (Maybe ())
forceEnvVarSetP = flag Nothing (pure ()) $
      short 'f'
  <>  long "force"
  <>  help "If environment variable already exists, overwrite it, otherwise operation will fail if environment variable already exists"

envvarNameP :: Parser T.Text
envvarNameP = option (T.pack <$> str) $
      short 'n'
  <>  long "name"
  <>  metavar "NAME"
  <>  help "The name for the environment variable you wish to add to pipelines"

envvarNameToDeleteP :: Parser T.Text
envvarNameToDeleteP = option (T.pack <$> str) $
      short 'n'
  <>  long "name"
  <>  metavar "NAME"
  <>  help "The name for the environment variable you wish to remove"

envvarValueP :: Parser T.Text
envvarValueP = option (T.pack <$> str) $
      long "value"
  <>  metavar "VALUE"
  <>  help "The value you wish to set the pipeline environment variable to"

envvarSecurityP :: Parser PipelinesEnvironmentVariableSecurity
envvarSecurityP = flag SecuredVariable UnsecuredVariable $
      long "unsecured"
  <>  help "Will expose the environment variable in the REST API, GUI and Logs, the default behaviour if this is unspecified is \"secured\" behaviour where you cannot retrieve the value via the REST API, GUI or logs, the value is only exposed to the builds"

publicKeyPathP :: Parser T.Text
publicKeyPathP = option (T.pack <$> str) $
      long "public-key"
  <>  metavar "PATH"
  <>  help "Path to the public SSH key"

privateKeyPathP :: Parser T.Text
privateKeyPathP = option (T.pack <$> str) $
      long "private-key"
  <>  metavar "PATH"
  <>  help "Path to the private SSH key"

accessKeyLabelP :: Parser T.Text
accessKeyLabelP = option (T.pack <$> str) $
      long "label"
  <>  help "A label for the Access Key"

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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
  , gitURLTypeP
  ) where

import Irreverent.Bitbucket.Core.Data.Common (
    Username(..)
  , RepoName(..)
  , GitURLType(..)
  )

import Irreverent.Bitbucket.Core.Data.Auth (Auth(..))

import qualified Ultra.Data.Text as T
import Ultra.Options.Applicative (
    Parser
  , argument
  , eitherTextReader
  , envvar
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

authReader :: T.Text -> Either T.Text Auth
authReader t = case T.splitOn ":" t of
  [] -> Left t
  username:password -> pure . Basic username $ T.intercalate ":" password




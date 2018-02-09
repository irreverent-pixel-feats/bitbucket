{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Cli.Commands.PipelineConfig
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Cli.Commands.PipelineConfig (
  -- * Functions
    getPipelineCfg
  ) where

import Irreverent.Bitbucket.Cli.Error

import Irreverent.Bitbucket.Core (Username(..), RepoName(..))
import Irreverent.Bitbucket.Core.Control (BitbucketT(..))
import Irreverent.Bitbucket.Core.Data.Auth (Auth(..))
import Irreverent.Bitbucket.Http.Error (BitbucketAPIError)
import Irreverent.Bitbucket.Http.Repositories.Pipelines.GetConfig (getPipelinesConfig)
import Irreverent.Bitbucket.Json.Pipelines.Config (PipelinesConfigJsonV2(..))

import Ultra.Control.Monad.Catch (MonadCatch)
import Ultra.Control.Monad.Trans.Either (EitherT, firstEitherT, mapEitherT)
import qualified Ultra.Data.Text.Encoding as T

import Data.Aeson.Encode.Pretty (Config(..), Indent(..), encodePretty', defConfig)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Network.Wreq.Session as S

import System.IO (stdout)

import Preamble

data ListReposError =
  ListRepoAPIFail !BitbucketAPIError
  deriving (Show)

prettyCfg :: Config
prettyCfg = defConfig {
    confIndent = Spaces 2,
    confTrailingNewline = True
  }

getPipelineCfg
  :: (MonadCatch m, MonadIO m)
  => Auth
  -> Username
  -> RepoName
  -> EitherT CliError m ()
getPipelineCfg auth owner repo  = firstEitherT BitbucketAPIFail . mapEitherT (flip runReaderT auth . runBitbucketT) $ do
  session <- liftIO S.newSession
  cfg <- getPipelinesConfig session owner repo
  let printCfg     = BSL.hPut stdout . encodePretty' prettyCfg . PipelinesConfigJsonV2
  let printNothing = BS.hPut stdout . T.encodeUtf8 $ "No pipeline config found"
  liftIO . maybe printNothing printCfg $ cfg

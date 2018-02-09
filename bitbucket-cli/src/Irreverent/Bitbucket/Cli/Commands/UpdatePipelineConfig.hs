{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Cli.Commands.UpdatePipelineConfig
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Cli.Commands.UpdatePipelineConfig (
  -- * Functions
    updatePipelineCfg
  ) where

import Irreverent.Bitbucket.Cli.Error

import Irreverent.Bitbucket.Core (Username(..), RepoName(..))
import Irreverent.Bitbucket.Core.Control (BitbucketT(..))
import Irreverent.Bitbucket.Core.Data.Auth (Auth(..))
import Irreverent.Bitbucket.Core.Data.Pipelines.UpdateConfig (UpdatePipelinesConfig(..))
import Irreverent.Bitbucket.Http.Repositories.Pipelines.UpdateConfig (updatePipelinesConfig)
import Irreverent.Bitbucket.Json.Pipelines.Config (PipelinesConfigJsonV2(..))

import Ultra.Control.Monad.Catch (MonadCatch)
import Ultra.Control.Monad.Trans.Either (EitherT, firstEitherT, mapEitherT)

import Data.Aeson.Encode.Pretty (Config(..), Indent(..), encodePretty', defConfig)
import qualified Data.ByteString.Lazy as BSL
import qualified Network.Wreq.Session as S

import System.IO (stdout)

import Preamble

prettyCfg :: Config
prettyCfg = defConfig {
    confIndent = Spaces 2,
    confTrailingNewline = True
  }

updatePipelineCfg
  :: (MonadCatch m, MonadIO m)
  => Auth
  -> Username
  -> RepoName
  -> UpdatePipelinesConfig
  -> EitherT CliError m ()
updatePipelineCfg auth owner repo cfgUpdate = firstEitherT BitbucketAPIFail . mapEitherT (flip runReaderT auth . runBitbucketT) $ do
  session <- liftIO S.newSession
  cfg <- updatePipelinesConfig session owner repo cfgUpdate
  liftIO . BSL.hPut stdout . encodePretty' prettyCfg . PipelinesConfigJsonV2 $ cfg

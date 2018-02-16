{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Cli.Commands.SetPipelineEnvironmentVariable
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Cli.Commands.SetPipelineEnvironmentVariable (
  -- * Functions
    setPipelineEnvironmentVariable
  ) where

import Irreverent.Bitbucket.Cli.Error
import Irreverent.Bitbucket.Cli.Commands.DeletePipelineEnvironmentVariable

import Irreverent.Bitbucket.Core (Username(..), RepoName(..))
import Irreverent.Bitbucket.Core.Control (BitbucketT(..))
import Irreverent.Bitbucket.Core.Data.Auth (Auth(..))
import Irreverent.Bitbucket.Core.Data.Pipelines.NewEnvironmentVariable (NewPipelinesEnvironmentVariable(..))
import Irreverent.Bitbucket.Http.Repositories.Pipelines.AddEnvironmentVariable (addPipelinesEnvironmentVariable)
import Irreverent.Bitbucket.Json.Pipelines.EnvironmentVariable (PipelinesEnvironmentVariableJsonV2(..))

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

setPipelineEnvironmentVariable
  :: (MonadCatch m, MonadIO m)
  => Auth
  -> Username
  -> RepoName
  -> Maybe ()
  -> NewPipelinesEnvironmentVariable
  -> EitherT CliError m ()
setPipelineEnvironmentVariable auth owner repo force var = do
  session <- liftIO S.newSession
  forM_ force $ \_ ->
    deletePipelineEnvironmentVariable auth owner repo (npevKey var)
  firstEitherT BitbucketAPIFail . mapEitherT (flip runReaderT auth . runBitbucketT) $ do
    out <- addPipelinesEnvironmentVariable session owner repo var
    liftIO . BSL.hPut stdout . encodePretty' prettyCfg . PipelinesEnvironmentVariableJsonV2 $ out

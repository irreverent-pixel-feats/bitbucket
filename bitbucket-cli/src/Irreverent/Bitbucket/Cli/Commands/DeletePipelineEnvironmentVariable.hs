{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Cli.Commands.DeletePipelineEnvironmentVariable
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Cli.Commands.DeletePipelineEnvironmentVariable (
  -- * Functions
    deletePipelineEnvironmentVariable
  ) where

import Irreverent.Bitbucket.Cli.Error

import Irreverent.Bitbucket.Core (Username(..), RepoName(..))
import Irreverent.Bitbucket.Core.Control (BitbucketT(..))
import Irreverent.Bitbucket.Core.Data.Auth (Auth(..))
import Irreverent.Bitbucket.Core.Data.Pipelines.EnvironmentVariable (PipelinesEnvironmentVariable(..))
import Irreverent.Bitbucket.Http.Repositories.Pipelines.DeleteEnvironmentVariable (deletePipelinesEnvironmentVariable)
import Irreverent.Bitbucket.Http.Repositories.Pipelines.GetEnvironmentVariables (getPipelineEnvironmentVariables)

import Ultra.Control.Monad.Catch (MonadCatch)
import Ultra.Control.Monad.Trans.Either (EitherT, firstEitherT, mapEitherT)
import qualified Ultra.Data.Text as T

import qualified Data.List as L
import qualified Network.Wreq.Session as S

import Preamble

deletePipelineEnvironmentVariable
  :: (MonadCatch m, MonadIO m)
  => Auth
  -> Username
  -> RepoName
  -> T.Text
  -> EitherT CliError m ()
deletePipelineEnvironmentVariable auth owner repo var = firstEitherT BitbucketAPIFail . mapEitherT (flip runReaderT auth . runBitbucketT) $ do
  session <- liftIO S.newSession
  vars <- getPipelineEnvironmentVariables session owner repo
  let existingVar = L.lookup var . fmap ((,) <$> pevKey <*> pevUuid) $ vars
  forM_ existingVar $ \v' ->
    deletePipelinesEnvironmentVariable session owner repo v'

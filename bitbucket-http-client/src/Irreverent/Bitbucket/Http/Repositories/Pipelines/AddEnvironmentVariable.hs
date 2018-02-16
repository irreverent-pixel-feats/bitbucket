{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Http.Repositories.Pipelines.AddEnvironmentVariable
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Http.Repositories.Pipelines.AddEnvironmentVariable (
  -- * Functions
    addPipelinesEnvironmentVariable
  ) where

import Irreverent.Bitbucket.Http.Common
import Irreverent.Bitbucket.Http.Error
import Irreverent.Bitbucket.Http.Methods

import Irreverent.Bitbucket.Core.Control (BitbucketT(..))
import Irreverent.Bitbucket.Core.Data.Common (
    Username(..)
  , RepoName(..)
  )
import Irreverent.Bitbucket.Core.Data.Pipelines.EnvironmentVariable (PipelinesEnvironmentVariable(..))
import Irreverent.Bitbucket.Core.Data.Pipelines.NewEnvironmentVariable (NewPipelinesEnvironmentVariable(..))

import Irreverent.Bitbucket.Json.Pipelines.EnvironmentVariable (pipelineEnvironmentVariableFromJson)
import Irreverent.Bitbucket.Json.Pipelines.NewEnvironmentVariable (PipelinesNewEnvironmentVariableJsonV2(..))

import Ultra.Control.Monad.Catch (MonadCatch(..))
import Ultra.Control.Monad.Trans.Either (EitherT)
import Ultra.Data.Aeson (toJSON)
import qualified Ultra.Data.Text as T

import qualified Network.Wreq.Session as S

import Preamble

addPipelinesEnvironmentVariable
  :: (MonadCatch m, MonadIO m)
  => S.Session
  -> Username
  -> RepoName
  -> NewPipelinesEnvironmentVariable
  -> EitherT BitbucketAPIError (BitbucketT m) PipelinesEnvironmentVariable
addPipelinesEnvironmentVariable sess owner reponame var =
  let
    endpoint :: T.Text
    endpoint = T.concat [
        baseV2URL
      , "/repositories/"
      , getUsername owner
      , "/"
      , getRepoName reponame
      , "/pipelines_config/variables/"
      ]
  in pipelineEnvironmentVariableFromJson <$> bitbucketPostJson sess endpoint (toJSON $ PipelinesNewEnvironmentVariableJsonV2 var)

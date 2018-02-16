{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Http.Repositories.Pipelines.GetEnvironmentVariables
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Http.Repositories.Pipelines.GetEnvironmentVariables (
  -- * Functions
    getPipelineEnvironmentVariables
  , pipelineEnvConduit
  ) where

import Irreverent.Bitbucket.Http.Common
import Irreverent.Bitbucket.Http.Error
import Irreverent.Bitbucket.Http.Paginated

import Irreverent.Bitbucket.Core.Control (BitbucketT(..))
import Irreverent.Bitbucket.Core.Data.Common (Username(..), RepoName(..))
import Irreverent.Bitbucket.Core.Data.Pipelines.EnvironmentVariable (PipelinesEnvironmentVariable(..))

import Irreverent.Bitbucket.Json.Pipelines.EnvironmentVariable (pipelineEnvironmentVariableFromJson)

import Ultra.Control.Monad.Trans.Either (EitherT)
import Ultra.Control.Monad.Catch (MonadCatch(..))
import Ultra.Data.Conduit (($$), (=$=))
import qualified Ultra.Data.Conduit as C
import qualified Ultra.Data.Conduit.List as C
import qualified Ultra.Data.Text as T

import qualified Network.Wreq.Session as S

import Preamble

pipelineEnvConduit
  :: forall m. (MonadCatch m, MonadIO m)
  => S.Session
  -> Username
  -> RepoName
  -> C.Producer (EitherT BitbucketAPIError (BitbucketT m)) [PipelinesEnvironmentVariable]
pipelineEnvConduit sess owner repo =
  let
    initReq :: T.Text
    initReq = T.concat [
        baseV2URL
      , "/repositories/"
      , getUsername owner
      , "/"
      , getRepoName repo
      , "/pipelines_config/variables/"
      ]

  in paginatedGetConduit sess initReq =$= C.map (fmap pipelineEnvironmentVariableFromJson)

getPipelineEnvironmentVariables
  :: (MonadCatch m, MonadIO m)
  => S.Session
  -> Username
  -> RepoName
  -> EitherT BitbucketAPIError (BitbucketT m) [PipelinesEnvironmentVariable]
getPipelineEnvironmentVariables sess owner repo = join <$> (pipelineEnvConduit sess owner repo $$ C.consume)

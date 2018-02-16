{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Http.Repositories.Pipelines.AddSSHKeyPair
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Http.Repositories.Pipelines.AddSSHKeyPair (
  -- * Functions
    addSSHKeyPair
  ) where

import Irreverent.Bitbucket.Http.Common
import Irreverent.Bitbucket.Http.Error
import Irreverent.Bitbucket.Http.Methods

import Irreverent.Bitbucket.Core.Control (BitbucketT(..))
import Irreverent.Bitbucket.Core.Data.Common (
    Username(..)
  , RepoName(..)
  )
import Irreverent.Bitbucket.Core.Data.Pipelines.SSHKeyPair (PipelinesSSHKeyPair(..))
import Irreverent.Bitbucket.Core.Data.Pipelines.NewSSHKeyPair (NewPipelinesSSHKeyPair(..))

import Irreverent.Bitbucket.Json.Pipelines.SSHKeyPair (pipelineSSHKeyPairFromJson)
import Irreverent.Bitbucket.Json.Pipelines.NewSSHKeyPair (PipelinesNewSSHKeyPairJsonV2(..))

import Ultra.Control.Monad.Catch (MonadCatch(..))
import Ultra.Control.Monad.Trans.Either (EitherT)
import qualified Ultra.Data.Text as T

import qualified Network.Wreq.Session as S

import Preamble

addSSHKeyPair
  :: (MonadCatch m, MonadIO m)
  => S.Session
  -> Username
  -> RepoName
  -> NewPipelinesSSHKeyPair
  -> EitherT BitbucketAPIError (BitbucketT m) PipelinesSSHKeyPair
addSSHKeyPair sess owner reponame pair =
  let
    endpoint :: T.Text
    endpoint = T.concat [
        baseV2URL
      , "/repositories/"
      , getUsername owner
      , "/"
      , getRepoName reponame
      , "/pipelines_config/ssh/key_pair"
      ]
  in pipelineSSHKeyPairFromJson <$> bitbucketPostJson sess endpoint (PipelinesNewSSHKeyPairJsonV2 pair)

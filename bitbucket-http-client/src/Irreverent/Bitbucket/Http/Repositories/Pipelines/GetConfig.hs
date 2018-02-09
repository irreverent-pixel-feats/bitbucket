{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Http.Repositories.Pipelines.GetConfig
-- Copyright    : (C) 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Http.Repositories.Pipelines.GetConfig (
  -- * Functions
    getPipelinesConfig
  ) where

import Irreverent.Bitbucket.Http.Common
import Irreverent.Bitbucket.Http.Error

import Irreverent.Bitbucket.Core.Control (BitbucketT(..), getAuth)
import Irreverent.Bitbucket.Core.Data.Auth (Auth(..))
import Irreverent.Bitbucket.Core.Data.Common (RepoName(..), Username(..))
import Irreverent.Bitbucket.Core.Data.Pipelines.Config (PipelinesConfig(..))

import Irreverent.Bitbucket.Json.Pipelines.Config (pipelineConfigFromJson)

import Ultra.Control.Monad.Trans.Either (EitherT, left)
import Ultra.Control.Monad.Catch (MonadCatch(..))
import Ultra.Control.Lens ((?~))
import Ultra.Data.Aeson (eitherDecode)
import qualified Ultra.Data.Text as T
import qualified Ultra.Data.Text.Encoding as T
import qualified Ultra.Network.HTTP.Client as H

import qualified Data.ByteString.Lazy as BSL

import qualified Network.Wreq as W
import qualified Network.Wreq.Session as S

import Preamble

getPipelinesConfig
  :: (MonadCatch m, MonadIO m)
  => S.Session
  -> Username
  -> RepoName
  -> EitherT BitbucketAPIError (BitbucketT m) (Maybe PipelinesConfig)
getPipelinesConfig sess (Username owner) (RepoName repo) =
  let
    initReq :: T.Text
    initReq = T.concat [baseV2URL, "/repositories/", owner, "/" , repo, "/pipelines_config"]

    httpHandler :: (Monad m) => H.HttpException -> EitherT BitbucketAPIError m a
    httpHandler = left . HttpError
  in do
    auth <- lift getAuth
    opts <- case auth of
      (Basic username password) -> pure (W.auth ?~ W.basicAuth (T.encodeUtf8 username) (T.encodeUtf8 password) $ baseReq)
    resp <- fmap H.optionalHttpClientResponse ((liftIO $ S.getWith opts sess (T.unpack initReq)) `catch` httpHandler)
    forM resp $ \case
      H.HttpNotOk r -> left $ NotOkHttpResponse (BSL.toStrict <$> r)
      H.HttpOk r    -> either (left . JsonError . T.pack) (pure . pipelineConfigFromJson) . eitherDecode . H.responseBody $ r



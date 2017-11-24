{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Http.Repositories.New
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Http.Repositories.New (
  -- * Functions
    createRepo
  ) where

import Irreverent.Bitbucket.Http.Common
import Irreverent.Bitbucket.Http.Error

import Irreverent.Bitbucket.Core.Control (BitbucketT(..), getAuth)
import Irreverent.Bitbucket.Core.Data.Auth (Auth(..))
import Irreverent.Bitbucket.Core.Data.Common (
    Username(..)
  , RepoName(..)
  )
import Irreverent.Bitbucket.Core.Data.NewRepository (NewRepository(..))
import Irreverent.Bitbucket.Core.Data.Repository (Repository(..))

import Irreverent.Bitbucket.Json.NewRepository (NewRepositoryJsonV2(..))
import Irreverent.Bitbucket.Json.Repository (repoFromJson)

import Ultra.Control.Lens ((?~))
import Ultra.Control.Monad.Catch (MonadCatch(..))
import Ultra.Control.Monad.Trans.Either (EitherT, left)
import Ultra.Data.Aeson (eitherDecode, toJSON)
import qualified Ultra.Data.Text as T
import qualified Ultra.Data.Text.Encoding as T
import qualified Ultra.Network.HTTP.Client as H

import qualified Data.ByteString.Lazy as BSL

import qualified Network.Wreq as W
import qualified Network.Wreq.Session as S

import Preamble

createRepo
  :: (MonadCatch m, MonadIO m)
  => S.Session
  -> Username
  -> RepoName
  -> NewRepository
  -> EitherT BitbucketAPIError (BitbucketT m) Repository
createRepo sess owner reponame newRepo =
  let
    httpHandler :: (Monad m) => H.HttpException -> EitherT BitbucketAPIError m a
    httpHandler = left . HttpError

    endpoint :: T.Text
    endpoint = T.concat [baseV2URL, "/repositories/", getUsername owner, "/", getRepoName reponame, "/"]
  in do
    auth <- lift getAuth
    opts <- case auth of
      (Basic username password) -> pure (W.auth ?~ (W.basicAuth `on` T.encodeUtf8) username password $ baseReq)
    resp <- fmap H.httpClientResponse ((liftIO $ S.postWith opts sess (T.unpack endpoint) (toJSON $ NewRepositoryJsonV2 newRepo)) `catch` httpHandler)
    json <- case resp of
      H.HttpNotOk r -> left $ NotOkHttpResponse (BSL.toStrict <$> r)
      H.HttpOk r    -> either (left . JsonError . T.pack) pure . eitherDecode . H.responseBody $ r 
    pure (repoFromJson json)

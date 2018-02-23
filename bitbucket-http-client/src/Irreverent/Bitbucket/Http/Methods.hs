{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Http.Methods
-- Copyright    : (C) 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Http.Methods (
  -- * Functions
    bitbucketDelete
  , bitbucketGet
  , bitbucketPostJson
  , bitbucketPutJson
  ) where

import Irreverent.Bitbucket.Http.Common
import Irreverent.Bitbucket.Http.Error

import Irreverent.Bitbucket.Core.Control (BitbucketT(..), getAuth)
import Irreverent.Bitbucket.Core.Data.Auth (Auth(..))

import Ultra.Control.Lens ((?~))
import Ultra.Control.Monad.Catch (MonadCatch(..))
import Ultra.Control.Monad.Trans.Either (EitherT, left)
import Ultra.Data.Aeson (FromJSON, ToJSON(..), eitherDecode)
import qualified Ultra.Data.Text as T
import qualified Ultra.Data.Text.Encoding as T
import qualified Ultra.Network.HTTP.Client as H

import qualified Data.ByteString.Lazy as BSL

import qualified Network.Wreq as W
import qualified Network.Wreq.Session as S

import Preamble

bitbucketGet
  :: (FromJSON a, MonadCatch m, MonadIO m)
  => S.Session
  -> T.Text
  -> EitherT BitbucketAPIError (BitbucketT m) a
bitbucketGet sess req =
  bitbucketReq $ \opts ->
    S.getWith opts sess (T.unpack req)

bitbucketDelete
  :: (MonadCatch m, MonadIO m)
  => S.Session
  -> T.Text
  -> EitherT BitbucketAPIError (BitbucketT m) ()
bitbucketDelete sess endpoint =
  let
    httpHandler :: (Monad m) => H.HttpException -> EitherT BitbucketAPIError m a
    httpHandler = left . HttpError

  in do
    auth <- lift getAuth
    opts <- case auth of
      (Basic username password) -> pure (W.auth ?~ W.basicAuth (T.encodeUtf8 username) (T.encodeUtf8 password) $ baseReq)
    void ((liftIO $ S.deleteWith opts sess (T.unpack endpoint)) `catch` httpHandler)

bitbucketPostJson
  :: (ToJSON a, FromJSON b, MonadCatch m, MonadIO m)
  => S.Session
  -> T.Text
  -> a
  -> EitherT BitbucketAPIError (BitbucketT m) b
bitbucketPostJson sess req body =
  bitbucketReq $ \opts ->
    S.postWith opts sess (T.unpack req) (toJSON body)

bitbucketPutJson
  :: (ToJSON a, FromJSON b, MonadCatch m, MonadIO m)
  => S.Session
  -> T.Text
  -> a
  -> EitherT BitbucketAPIError (BitbucketT m) b
bitbucketPutJson sess req body =
  bitbucketReq $ \opts ->
    S.putWith opts sess (T.unpack req) (toJSON body)

bitbucketReq
  :: (FromJSON a, MonadCatch m, MonadIO m)
  => (W.Options -> IO (W.Response BSL.ByteString))
  -> EitherT BitbucketAPIError (BitbucketT m) a
bitbucketReq method =
  let
    httpHandler :: (Monad m) => H.HttpException -> EitherT BitbucketAPIError m a
    httpHandler = left . HttpError

  in do
    auth <- lift getAuth
    opts <- case auth of
      (Basic username password) -> pure (W.auth ?~ W.basicAuth (T.encodeUtf8 username) (T.encodeUtf8 password) $ baseReq)
    resp <- fmap H.httpClientResponse ((liftIO $ method opts) `catch` httpHandler)
    case resp of
      H.HttpNotOk r -> left $ NotOkHttpResponse (BSL.toStrict <$> r)
      H.HttpOk r    -> either (left . JsonError . T.pack) pure . eitherDecode . H.responseBody $ r

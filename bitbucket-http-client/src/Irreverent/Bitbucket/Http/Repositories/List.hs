{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Http.Repositories.List
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Http.Repositories.List (
  -- * Functions
    listRepositories
  , listRepositoriesPaged
  , repoConduit
  , renderBitbucketAPIError
  ) where

import Irreverent.Bitbucket.Http.Common
import Irreverent.Bitbucket.Http.Error

import Irreverent.Bitbucket.Core.Control (BitbucketT(..), getAuth)
import Irreverent.Bitbucket.Core.Data.Auth (Auth(..))
import Irreverent.Bitbucket.Core.Data.Common (Username(..), Uri(..))
import Irreverent.Bitbucket.Core.Data.Paginated (Paginated(..))
import Irreverent.Bitbucket.Core.Data.Repository (Repository(..))

import Irreverent.Bitbucket.Json.Paginated (PaginatedJsonV2(..))
import Irreverent.Bitbucket.Json.Repository (RepositoryJsonV2(..))

import Ultra.Control.Monad.Trans.Either (EitherT, left)
import Ultra.Control.Monad.Catch (MonadCatch(..))
import Ultra.Control.Lens ((?~))
import Ultra.Data.Aeson (eitherDecode)
import Ultra.Data.Conduit (($$))
import qualified Ultra.Data.Conduit as C
import qualified Ultra.Data.Conduit.List as C
import qualified Ultra.Data.Text as T
import qualified Ultra.Data.Text.Encoding as T
import qualified Ultra.Network.HTTP.Client as H

import qualified Data.ByteString.Lazy as BSL

import qualified Network.Wreq as W
import qualified Network.Wreq.Session as S

import Preamble


listRepositoriesPaged
  :: (MonadCatch m, MonadIO m)
  => S.Session
  -> Username
  -> EitherT BitbucketAPIError (BitbucketT m) (Paginated Repository)
listRepositoriesPaged sess owner =
  let
    initReq :: T.Text
    initReq = T.concat [baseV2URL, "/repositories/", getUsername owner, "/"]

  in listRepositoriesPagedReq sess initReq

listRepositoriesPagedReq
  :: (MonadCatch m, MonadIO m)
  => S.Session
  -> T.Text
  -> EitherT BitbucketAPIError (BitbucketT m) (Paginated Repository)
listRepositoriesPagedReq sess req =
  let
    httpHandler :: (Monad m) => H.HttpException -> EitherT BitbucketAPIError m a
    httpHandler = left . HttpError

  in do
    auth <- lift getAuth
    opts <- case auth of
      (Basic username password) -> pure (W.auth ?~ W.basicAuth (T.encodeUtf8 username) (T.encodeUtf8 password) $ baseReq)
    resp <- fmap H.httpClientResponse ((liftIO $ S.getWith opts sess (T.unpack req)) `catch` httpHandler)
    json <- case resp of
      H.HttpNotOk r -> left $ NotOkHttpResponse (BSL.toStrict <$> r)
      H.HttpOk r    -> either (left . JsonError . T.pack) pure . eitherDecode . H.responseBody $ r
    pure (fromRepositoryJsonV2 <$> (fromPaginatedJsonV2 json))


repoConduit
  :: forall m. (MonadCatch m, MonadIO m)
  => S.Session
  -> Username
  -> C.Producer (EitherT BitbucketAPIError (BitbucketT m)) [Repository]
repoConduit sess owner =
  let
    loop :: Paginated Repository -> C.Producer (EitherT BitbucketAPIError (BitbucketT m)) [Repository]
    loop pg = do
      C.yield $ pageValues pg
      next <- forM (nextPage pg) $ \case
        Uri uri -> lift $ listRepositoriesPagedReq sess uri
      maybe (pure ()) loop $ next

  in lift (listRepositoriesPaged sess owner) >>= loop

-- FIXME: (Dom De Re) Have to resolve this exception
-- Left (HttpError (HttpExceptionRequest Request {
--   host                 = "api.bitbucket.org"
--   port                 = 443
--   secure               = True
--   requestHeaders       = [("Authorization","Basic ******")]
--   path                 = "/2.0/repositories/cammyrepo/"
--   queryString          = "?page=2"
--   method               = "GET"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
-- }
--  (InternalException (Terminated True "received fatal error: BadRecordMac" (Error_Protocol ("remote side fatal error",True,BadRecordMac))))))
--
listRepositories
  :: (MonadCatch m, MonadIO m)
  => S.Session
  -> Username
  -> EitherT BitbucketAPIError (BitbucketT m) [Repository]
listRepositories sess owner = join <$> (repoConduit sess owner $$ C.consume)

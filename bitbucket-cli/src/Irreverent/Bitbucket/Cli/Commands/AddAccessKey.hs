{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Cli.Commands.AddAccessKey
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Cli.Commands.AddAccessKey (
  -- * Functions
    addAccessKey
  ) where

import Irreverent.Bitbucket.Cli.Error

import Irreverent.Bitbucket.Core (Username(..), RepoName(..))
import Irreverent.Bitbucket.Core.Control (BitbucketT(..))
import Irreverent.Bitbucket.Core.Data.Auth (Auth(..))
import Irreverent.Bitbucket.Core.Data.Common (PublicSSHKey(..))
import Irreverent.Bitbucket.Core.Data.NewAccessKey (NewAccessKey(..))
import qualified Irreverent.Bitbucket.Http.Repositories.AddAccessKey as H
import Irreverent.Bitbucket.Json.AccessKey (AccessKeyJsonV1(..))

import Ultra.Control.Monad.Bracket (MonadBracket)
import Ultra.Control.Monad.Catch (MonadCatch)
import Ultra.Control.Monad.Trans.Either (EitherT, firstEitherT, mapEitherT)
import qualified Ultra.Data.Text as T
import qualified Ultra.Data.Text.Encoding as T
import Ultra.System.IO (IOMode(..), withBinaryFile)

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Network.Wreq.Session as S

import System.IO (stdout)

import Preamble

addAccessKey
  :: (MonadBracket m, MonadCatch m, MonadIO m)
  => Auth
  -> Username
  -> RepoName
  -> Maybe T.Text
  -> T.Text
  -> EitherT CliError m ()
addAccessKey auth owner rname label keyPath = do
  key <- firstEitherT CliFileOpenError $
    withBinaryFile keyPath ReadMode $ \h -> do
      liftIO . fmap (PublicSSHKey . T.decodeUtf8) $ BS.hGetContents h
  firstEitherT BitbucketAPIFail . mapEitherT (flip runReaderT auth . runBitbucketT) $ do
    s <- liftIO S.newSession
    H.addAccessKey s owner rname (NewAccessKey label key)
      >>= liftIO . BSL.hPutStr stdout . encodePretty . AccessKeyJsonV1


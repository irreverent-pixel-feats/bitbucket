{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Cli.Commands.SetPipelineSSHKeys
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Cli.Commands.SetPipelineSSHKeys (
  -- * Functions
    setPipelinesSSHKeys
  ) where

import Irreverent.Bitbucket.Cli.Error

import Irreverent.Bitbucket.Core (Username(..), RepoName(..))
import Irreverent.Bitbucket.Core.Control (BitbucketT(..))
import Irreverent.Bitbucket.Core.Data.Auth (Auth(..))
import Irreverent.Bitbucket.Core.Data.Common (PublicSSHKey(..), PrivateSSHKey(..))
import Irreverent.Bitbucket.Core.Data.Pipelines.NewSSHKeyPair (NewPipelinesSSHKeyPair(..))
import Irreverent.Bitbucket.Http.Repositories.Pipelines.AddSSHKeyPair (addSSHKeyPair)
import Irreverent.Bitbucket.Json.Pipelines.SSHKeyPair (PipelinesSSHKeyPairJsonV2(..))

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

setPipelinesSSHKeys
  :: (MonadBracket m, MonadCatch m, MonadIO m)
  => Auth
  -> Username
  -> RepoName
  -> T.Text
  -> T.Text
  -> EitherT CliError m ()
setPipelinesSSHKeys auth owner rname privatePath publicPath = do
  privateKey <- firstEitherT CliFileOpenError $
    withBinaryFile privatePath ReadMode $ \h -> do
      liftIO . fmap (PrivateSSHKey . T.decodeUtf8) $ BS.hGetContents h
  publicKey <- firstEitherT CliFileOpenError $
    withBinaryFile publicPath ReadMode $ \h -> do
      liftIO . fmap (PublicSSHKey . T.decodeUtf8) $ BS.hGetContents h
  firstEitherT BitbucketAPIFail . mapEitherT (flip runReaderT auth . runBitbucketT) $ do
    s <- liftIO S.newSession
    addSSHKeyPair s owner rname (NewPipelinesSSHKeyPair privateKey publicKey)
      >>= liftIO . BSL.hPutStr stdout . encodePretty . PipelinesSSHKeyPairJsonV2


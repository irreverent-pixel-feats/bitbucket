{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Cli.Commands.GetPipelineEnvironmentVariables
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Cli.Commands.GetPipelineEnvironmentVariables (
  -- * Functions
    listPipelineEnvironmentVariables
  ) where

import Irreverent.Bitbucket.Cli.Error

import Irreverent.Bitbucket.Core (Username(..), RepoName(..))
import Irreverent.Bitbucket.Core.Control (BitbucketT(..))
import Irreverent.Bitbucket.Core.Data.Auth (Auth(..))
import Irreverent.Bitbucket.Core.Data.Common (Uuid(..))
import Irreverent.Bitbucket.Core.Data.Pipelines.EnvironmentVariable (PipelinesEnvironmentVariable(..), PipelinesEnvironmentVariableValue(..))
import Irreverent.Bitbucket.Http.Repositories.Pipelines.GetEnvironmentVariables (pipelineEnvConduit)

import Ultra.Control.Monad.Catch (MonadCatch)
import Ultra.Control.Monad.Trans.Either (EitherT, firstEitherT, mapEitherT)
import Ultra.Data.Conduit (Conduit, ($$), (=$=), awaitForever,  yield)
import qualified Ultra.Data.Conduit.List as C
import qualified Ultra.Data.Conduit.Binary as C
import qualified Ultra.Data.Text as T
import qualified Ultra.Data.Text.Encoding as T

import qualified Data.ByteString as BS
import qualified Network.Wreq.Session as S

import System.IO (stdout)

import Preamble

renderPipe
  :: (Monad m)
  => Conduit PipelinesEnvironmentVariable m BS.ByteString
renderPipe = C.map $ \case
  PipelinesEnvironmentVariable SecuredPipelinesVariableValue name uuid -> T.encodeUtf8 $ T.concat [name, ": ", "******* (", getUuid uuid,  ")\n"]
  PipelinesEnvironmentVariable (UnsecuredPipelinesVariableValue val) name uuid -> T.encodeUtf8 $ T.concat [name, ": ", val, " (", getUuid uuid,  ")\n"]

listPipelineEnvironmentVariables
  :: (MonadCatch m, MonadIO m)
  => Auth
  -> Username
  -> RepoName
  -> EitherT CliError m ()
listPipelineEnvironmentVariables auth owner repo = firstEitherT BitbucketAPIFail . mapEitherT (flip runReaderT auth . runBitbucketT) $ do
  session <- liftIO S.newSession
  (pipelineEnvConduit session owner repo =$= awaitForever (traverse yield) =$= renderPipe $$ C.sinkHandle stdout)

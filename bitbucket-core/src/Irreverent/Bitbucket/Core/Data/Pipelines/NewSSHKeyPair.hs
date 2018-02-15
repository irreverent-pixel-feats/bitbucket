{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitucket.Core.Data.Pipelines.NewSSHKeyPair
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Core.Data.Pipelines.NewSSHKeyPair (
  -- * Types
    NewPipelinesSSHKeyPair(..)
  ) where

import Irreverent.Bitbucket.Core.Data.Common (
    PublicSSHKey(..)
  , PrivateSSHKey(..)
  )

import Preamble

data NewPipelinesSSHKeyPair = NewPipelinesSSHKeyPair {
    npskpPrivateKey :: !PrivateSSHKey
  , npskpPublicKey  :: !PublicSSHKey
  } deriving (Show, Eq)

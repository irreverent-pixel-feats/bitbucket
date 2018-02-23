{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Core.Data.AccessKey
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Core.Data.AccessKey (
  -- * Types
    AccessKey(..)
  ) where

import Irreverent.Bitbucket.Core.Data.Common

import qualified Ultra.Data.Text as T

import Preamble

data AccessKey = AccessKey {
    akLabel :: !(Maybe T.Text)
  , akKey   :: !PublicSSHKey
  , akId    :: !Integer
  } deriving (Show, Eq)

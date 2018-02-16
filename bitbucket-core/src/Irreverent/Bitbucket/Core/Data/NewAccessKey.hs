{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Core.Data.NewAccessKey
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Core.Data.NewAccessKey (
  -- * Types
    NewAccessKey(..)
  ) where

import Irreverent.Bitbucket.Core.Data.Common

import qualified Ultra.Data.Text as T

import Preamble

data NewAccessKey = NewAccessKey {
    nakLabel :: !(Maybe T.Text)
  , nakKey :: !PublicSSHKey
  } deriving (Show, Eq)

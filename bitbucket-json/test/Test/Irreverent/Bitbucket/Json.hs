{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Irreverent.Bitbucket.Json where

import Irreverent.Bitbucket.Json

import Test.Irreverent.Bitbucket.Core.Arbitraries

import Ultra.Data.Aeson (eitherDecodeStrict', encode)

import Lab.Core.Control.RoundTrip (roundTripProp)

import Lab.Core.QuickCheck (Property, forAll)
import Lab.Core.QuickCheck.TH (quickCheckAll)

import qualified Data.ByteString.Lazy as BSL

import Preamble

prop_newRepository :: Property
prop_newRepository = forAll newRepositories $
  roundTripProp
    (BSL.toStrict . encode . NewRepositoryJsonV2)
    (fmap newRepoFromJson . eitherDecodeStrict')

prop_repository :: Property
prop_repository = forAll repositories $
  roundTripProp
    (BSL.toStrict . encode . RepositoryJsonV2)
    (fmap repoFromJson . eitherDecodeStrict')

return []
tests :: IO Bool
tests = $quickCheckAll


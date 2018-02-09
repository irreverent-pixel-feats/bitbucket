{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Irreverent.Bitbucket.Json where

import Irreverent.Bitbucket.Json

import Test.Irreverent.Bitbucket.Core.Arbitraries

import Ultra.Data.Aeson (eitherDecodeStrict', encode, parseEither)

import Lab.Core.Control.RoundTrip (roundTripProp)

import Lab.Core.QuickCheck (Property, forAll)
import Lab.Core.QuickCheck.TH (quickCheckAll)

import qualified Data.ByteString.Lazy as BSL

import Preamble

prop_projectKeySolo :: Property
prop_projectKeySolo = forAll bitbucketProjectKeys $
  roundTripProp
    (BSL.toStrict . encode . jsonSoloProjectKey)
    (parseEither parseSoloProjectKeyJson <=< eitherDecodeStrict')

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

prop_repositorySummary :: Property
prop_repositorySummary = forAll repositorySummaries $
  roundTripProp
    (BSL.toStrict . encode . RepositorySummaryJsonV2)
    (fmap repoSummaryFromJson . eitherDecodeStrict')

prop_pipelineConfig :: Property
prop_pipelineConfig = forAll pipelineConfigs $
  roundTripProp
    (BSL.toStrict . encode . PipelinesConfigJsonV2)
    (fmap pipelineConfigFromJson . eitherDecodeStrict')

prop_updatePipelineConfig :: Property
prop_updatePipelineConfig = forAll updatePipelineConfigs $
  roundTripProp
    (BSL.toStrict . encode . UpdatePipelinesConfigJsonV2)
    (fmap updatePipelineConfigFromJson . eitherDecodeStrict')

return []
tests :: IO Bool
tests = $quickCheckAll


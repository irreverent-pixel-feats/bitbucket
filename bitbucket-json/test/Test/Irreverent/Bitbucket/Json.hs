{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Irreverent.Bitbucket.Json where

import Irreverent.Bitbucket.Json

import Test.Irreverent.Bitbucket.Core.Arbitraries

import Ultra.Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict', encode, parseEither)

import Lab.Core.Control.RoundTrip (roundTripProp)

import Lab.Core.QuickCheck (Gen, Property, forAll)
import Lab.Core.QuickCheck.TH (quickCheckAll)

import qualified Data.ByteString.Lazy as BSL

import Preamble

test
  :: (Show a, Eq a, FromJSON b, ToJSON b)
  => Gen a
  -> (a -> b)
  -> (b -> a)
  -> Property
test gen toJson fromJson = forAll gen $
  roundTripProp
    (BSL.toStrict . encode . toJson)
    (fmap fromJson . eitherDecodeStrict')

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

prop_newAccessKey :: Property
prop_newAccessKey = forAll newAccessKeys $
  roundTripProp
    (BSL.toStrict . encode . NewAccessKeyJsonV1)
    (fmap newAccessKeyFromJson . eitherDecodeStrict')

prop_accessKey :: Property
prop_accessKey = test
  accessKeys
  AccessKeyJsonV1
  accessKeyFromJson

prop_newEnvironmentVariable :: Property
prop_newEnvironmentVariable =
  test
    newEnvironmentVariables
    PipelinesNewEnvironmentVariableJsonV2
    pipelineNewEnvironmentVariableFromJson

prop_environmentVariables :: Property
prop_environmentVariables =
  test
    environmentVariables
    PipelinesEnvironmentVariableJsonV2
    pipelineEnvironmentVariableFromJson

prop_SSHKeyPair :: Property
prop_SSHKeyPair =
  test
    sshKeyPairs
    PipelinesSSHKeyPairJsonV2
    pipelineSSHKeyPairFromJson

prop_newSSHKeyPair :: Property
prop_newSSHKeyPair =
  test
    newSSHKeyPairs
    PipelinesNewSSHKeyPairJsonV2
    pipelineNewSSHKeyPairFromJson

return []
tests :: IO Bool
tests = $quickCheckAll


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | This module is similare to `Network.Docker.Registry` but uses HUnit
-- assertions to check the returned HTTP status codes.
module Network.Docker.Registry.Checks where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Test.HUnit

import Network.Docker.Registry
import Network.Docker.Registry.Internal
import Network.Docker.Registry.Types
import Network.Http.Checks

-- TODO There is plenty of ways to mis-construct the queries, e.g. removing
-- the content-type, having the content-length wrong, or interrupting a
-- transfer-encoding: chunked, ...

----------------------------------------------------------------------
-- Repositories
----------------------------------------------------------------------

checkPushRepository :: Int -> String -> Repository -> Test
checkPushRepository n title r = TestLabel title $ TestCase $
  pushRepository r >>= expectCode n
  -- TODO 200 "" or true
  -- TODO 400 Test the payload for something like {"error": "malformed json"}.
  -- TODO 200 This returns a token, even if not requested.

checkPushRepository' :: Int -> String -> Repository -> LB.ByteString -> Test
checkPushRepository' n title Repository{..} jsonOverride = TestLabel title $ TestCase $
  putRepository repositoryCredentials repositoryHost
    repositoryNamespace repositoryName jsonOverride >>= expectCode n

checkPushRepositoryTag :: Int -> String -> Repository -> Image -> ByteString -> Test
checkPushRepositoryTag n title r i tag = TestLabel title $ TestCase $
  pushRepositoryTag r i tag >>= expectCode n

----------------------------------------------------------------------
-- Images
----------------------------------------------------------------------

-- | Pushing an image is done in three steps: first the JSON meta-data, the
-- layer, then the checksum.
-- Note that we can start pushing an image even if it is not listed in a
-- repository.
checkPushImageJson :: Int -> String -> Repository -> Image -> Test
checkPushImageJson n title r i = TestLabel title $ TestCase $
  pushImageJson r i >>= expectCode n
  -- TODO 200 "" or true
  -- TODO 400 Test the payload for something like {"error": "malformed json"}.
  -- TODO 409 Test the payload for something like {"error": "image already exists"}.

checkPullImageJson :: Int -> String -> Repository -> Image -> Test
checkPullImageJson n title r i = TestLabel title $ TestCase $ do
  (code, _) <- pullImageJson r i
  expectCode n code

checkPushImageLayer :: Int -> String -> Repository -> Image -> Test
checkPushImageLayer n title r i = TestLabel title $ TestCase $
  pushImageLayer r i >>= expectCode n

checkPushImageChecksum :: Int -> String -> Repository -> Image -> Test
checkPushImageChecksum n title r i = TestLabel title $ TestCase $ do
  pushImageChecksum r i >>= expectCode n

checkPushImageChecksum' :: Int -> String -> Repository -> Image -> LB.ByteString -> Test
checkPushImageChecksum' n title Repository{..} Image{..} checksumOverride =
  TestLabel title $ TestCase $ do
    putImageChecksum repositoryCredentials repositoryHost imageName checksumOverride
      >>= expectCode n

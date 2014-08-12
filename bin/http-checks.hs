{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Digest.Pure.SHA (showDigest, sha256)
import Data.Version (showVersion)
import Paths_http_checks (version)
import Network.Http.Client
import OpenSSL (withOpenSSL)
import System.Console.CmdArgs.Implicit
import qualified System.IO.Streams as Streams
import Test.HUnit

main :: IO ()
main = (runCmd =<<) $ cmdArgs $
  modes
    [ cmdSelfCheck
    , cmdDockerPushJson
    , cmdDockerPushRepository
    , cmdDockerFlow
    ]
  &= summary versionString
  &= program "http-checks"

-- | String with the program name, version and copyright.
versionString :: String
versionString =
  "http-checks " ++ showVersion version ++ " - Copyright (c) 2013-2014 Vo Minh Thu."

----------------------------------------------------------------------
-- Command-line interface.
----------------------------------------------------------------------

-- | Data type representing the different command-line subcommands.
data Cmd =
    CmdSelfCheck
  | CmdDockerPushJson
  { cmdImage :: String
  }
  | CmdDockerPushRepository
  | CmdDockerFlow
  deriving (Data, Typeable)

-- | Create a 'SelfCheck' command.
cmdSelfCheck :: Cmd
cmdSelfCheck = CmdSelfCheck
    &= help "Run self-tests against httpbin and httpstat."
    &= explicit
    &= name "self-checks"

-- | Create a 'DockerPushJson' command.
cmdDockerPushJson :: Cmd
cmdDockerPushJson = CmdDockerPushJson
  { cmdImage = def
    &= explicit
    &= name "image"
    &= typ "IMAGE_ID"
  } &= help "Try pushing meta data to a Docker registry. \
      \The image must already exist."
    &= explicit
    &= name "docker-push-json"

-- | Create a 'DockerPushRepository' command.
cmdDockerPushRepository :: Cmd
cmdDockerPushRepository = CmdDockerPushRepository
    &= help "Push an empty repository to a Docker registry. \
      \There is no difference if the repository already exists."
    &= explicit
    &= name "docker-push-repository"

-- | Create a 'DockerFlow' command.
cmdDockerFlow :: Cmd
cmdDockerFlow = CmdDockerFlow
    &= help "Push an empty repository to a Docker registry, \
      \ push image meta data, image layer, image checksum. \
      \The repository must not exist."
    &= explicit
    &= name "docker-flow"

-- | Run a sub-command.
runCmd :: Cmd -> IO ()
runCmd CmdSelfCheck{..} = do
  _ <- runTestTT $ TestList
    [ testList "Self-tests against httpbin"
      [ is200 "http://httpbin.org" "/status/200"
      , is301 "http://httpbin.org" "/status/301"
      , is307 "http://httpbin.org" "/status/307"
      ]

    , testList "Self-tests against httpstat"
      [ is200 "http://httpstat.us" "/200"
      , is301 "http://httpstat.us" "/301"
      , is307 "http://httpstat.us" "/307"
      ]
    ]
  return ()

runCmd CmdDockerPushJson{..} = do
  let image = B.pack cmdImage
  _ <- runTestTT $ TestList
    [ testList "Check pushing mal-formed meta-data"
      [ malformedImageJson "Empty body" image ""
      , malformedImageJson "Empty JSON object" image "{}"
      , malformedImageJson "Invalid JSON" image "{"
      , malformedImageJson "Invalid image ID in JSON" image
        "{\"id\":\"a\"}"
      ]

    , testList "Check pushing meta-data for already existing image"
      [ existingImage "Existing image" image $
        LB.fromChunks ["{\"id\":\"", image, "\"}"]
      ]
    ]
  return ()

runCmd CmdDockerPushRepository{..} = do
  _ <- runTestTT $ TestList
    [ testList "Check pushing mal-formed repository data"
      [ malformedRepository "Empty body" ""
      , malformedRepository "Empty JSON object" "{}"
      , malformedRepository "Invalid JSON" "{"
      ]

    , testList "Check pushing mal-formed repository data, but it is accepted."
      [ checkRepository 200 "Partial image ID"
        "[{\"id\":\"aaaaaaaaaaaa\"}]"
      , checkRepository 200 "Replaying exactly the same request."
        "[{\"id\":\"aaaaaaaaaaaa\"}]"
      ]

    , testList "Check pushing correctly formed repository data."
      [ checkRepository 200 "Just an ID." $
        LB.fromChunks ["[{\"id\":\"", B.replicate 64 'a', "\"}]"]
      , checkRepository 200 "Replaying exactly the same request." $
        LB.fromChunks ["[{\"id\":\"", B.replicate 64 'a', "\"}]"]
      , checkRepository 200 "Empty image list" "[]"
      ]
    ]
  return ()

runCmd CmdDockerFlow{..} = do
  -- TODO Official repository accepts non-hexadecimal characters for the
  -- image ID.
  -- TODO Official repository does 500 on non-ascii checksums.
  let image = B.replicate 64 '6'
      json = LB.fromChunks ["{\"id\":\"", image, "\"}"]
  -- TODO The layer is done with make-layer.sh.
  layer <- LB.readFile "etc.tar.gz"
  let checksum' = LB.fromChunks ["sha256:" `B.append` B.replicate 64 '9']
  -- TODO Use the incremental interface.
  -- The checksum used by Docker depends on the exact string representation of
  -- JSON meta-data...
  let checksum = ("sha256:" `LB.append`) . LC.pack . showDigest . sha256 $
        json `LB.append` "\n" `LB.append` layer
  _ <- runTestTT $ TestList
    [ testList "Full repository creation."
      [ checkGetImage 404 "Check the image doesn't exist." image
      , checkRepository 200 "Push the repository." $
        LB.fromChunks ["[{\"id\":\"", image, "\"}]"]
      , checkImage 200 "Push image meta-data" image json
      , checkGetImage 400 "Check the image is being uploaded." image
      , checkPutImageLayer 200 "Push image layer." image layer
      , checkPutImageChecksum 400 "Push wrong image checksum." image checksum'
      , checkPutImageChecksum 200 "Push correct image checksum." image checksum
      -- TODO Push also the tags.
      -- TODO Push also v1/repositories/quux/bar/images.
      ]
    ]
  return ()

testList :: String -> [Test] -> Test
testList title list = TestLabel title $ TestList list

----------------------------------------------------------------------
-- HTTP checks as HUnit tests.
----------------------------------------------------------------------

-- | Ok.
is200 :: ByteString -> ByteString -> Test
is200 = is' 200

-- | Permanent redirect.
is301 :: ByteString -> ByteString -> Test
is301 = is' 301

-- | Temporary redirect.
is307 :: ByteString -> ByteString -> Test
is307 = is' 307

is' :: StatusCode -> ByteString -> ByteString -> Test
is' n base uri = TestLabel (B.unpack (base `B.append` uri) ++ " " ++ show n) $
  TestCase $ is n base uri []

-- TODO There is plenty of ways to mis-construct the queries, e.g. removing
-- the content-type, having the content-length wrong, or interrupting a
-- transfer-encoding: chunked, ...

malformedImageJson :: String -> ByteString -> LB.ByteString -> Test
malformedImageJson = checkImage 400

existingImage :: String -> ByteString -> LB.ByteString -> Test
existingImage = checkImage 409

checkGetImage :: Int -> String -> ByteString -> Test
checkGetImage n title image = TestLabel title $ TestCase $
  getImageJson "registry.local" image
    $ \response -> do
      checkCode n response

checkImage :: Int -> String -> ByteString -> LB.ByteString -> Test
checkImage n title image json = TestLabel title $ TestCase $
  putImageJson "registry.local" image json
    $ \response -> do
      checkCode n response
      -- TODO 200 "" or true
      -- TODO 400 Test the payload for something like {"error": "malformed json"}.
      -- TODO 409 Test the payload for something like {"error": "image already exists"}.

checkPutImageLayer :: Int -> String -> ByteString -> LB.ByteString -> Test
checkPutImageLayer n title image layer = TestLabel title $ TestCase $
  putImageLayer "registry.local" image layer
    $ \response -> do
      checkCode n response

checkPutImageChecksum :: Int -> String -> ByteString -> LB.ByteString -> Test
checkPutImageChecksum n title image checksum = TestLabel title $ TestCase $
  putImageChecksum "registry.local" image checksum
    $ \response -> do
      checkCode n response

malformedRepository :: String -> LB.ByteString -> Test
malformedRepository = checkRepository 400

checkRepository :: Int -> String -> LB.ByteString -> Test
checkRepository n title json = TestLabel title $ TestCase $
  putRepository "registry.local" json
    $ \response -> do
      checkCode n response
      -- TODO 200 "" or true
      -- TODO 400 Test the payload for something like {"error": "malformed json"}.
      -- TODO 200 This returns a token, even if not requested.

----------------------------------------------------------------------
-- HTTP checks.
----------------------------------------------------------------------

is :: StatusCode -> ByteString -> ByteString
  -> [(ByteString, Maybe ByteString)] -> Assertion
is n = checkResponse $ checkCode n

checkCode :: StatusCode -> Response -> IO ()
checkCode n response = do
  let code = getStatusCode response
  unless (code == n) $
    assertFailure $ "Expected HTTP code " ++ show n ++
      " but received " ++ show code ++ " instead."

-- | Execute a GET agains the specified URI (e.g. `/echo`) using the
-- supplied parameters.
checkResponse ::
  (Response -> Assertion)
  -> ByteString
  -> ByteString
  -> [(ByteString, Maybe ByteString)]
  -> Assertion
checkResponse f base uri parameters = withOpenSSL $ do
  let url = B.concat [uri, queryString parameters]
  q <- buildRequest $ do
    http GET url

  c <- establishConnection base
  -- print q
  sendRequest c q emptyBody
  receiveResponse c $ \response _ -> f response
  -- TODO Connection won't be closed if `f` fails.
  closeConnection c

queryString :: [(ByteString, Maybe ByteString)] -> ByteString
queryString [] = ""
queryString xs = B.cons '?' . B.intercalate "&" . map f $ xs
  where f (a, Just b) = B.concat [a, "=", b]
        f (a, _) = a

getImageJson :: ByteString -> ByteString
  -> (Response -> Assertion) -> IO ()
getImageJson host image f = withOpenSSL $ do
  let url = B.concat ["/v1/images/" `B.append` image `B.append` "/json"]
  q <- buildRequest $ do
    http GET url
    setAuthorizationBasic "quux" "thud"

  c <- establishConnection $ "https://" `B.append` host
  sendRequest c q emptyBody
  receiveResponse c $ \response _ -> f response
  closeConnection c

putImageJson :: ByteString -> ByteString -> LB.ByteString
  -> (Response -> Assertion) -> IO ()
putImageJson host image json f = withOpenSSL $ do
  let url = B.concat ["/v1/images/" `B.append` image `B.append` "/json"]
      body = json
  q <- buildRequest $ do
    http PUT url
    setAuthorizationBasic "quux" "thud"
    setContentLength (fromIntegral $ LB.length body)
    setContentType "application/json"

  c <- establishConnection $ "https://" `B.append` host
  body' <- Streams.fromLazyByteString body
  sendRequest c q (inputStreamBody body')
  receiveResponse c $ \response _ -> f response
  closeConnection c

putImageLayer :: ByteString -> ByteString -> LB.ByteString
  -> (Response -> Assertion) -> IO ()
putImageLayer host image layer f = withOpenSSL $ do
  let namespace = "quux"
      url = B.concat ["/v1/images/" `B.append` image `B.append` "/layer"]
      body = layer
  q <- buildRequest $ do
    http PUT url
    -- TODO The official registry does a 500 if there is no version.
    setHeader "User-Agent" "fake-docker/1.1.2"
    setAuthorizationBasic namespace "thud"
    setTransferEncoding -- "chunked"

  c <- establishConnection $ "https://" `B.append` host
  body' <- Streams.fromLazyByteString body
  sendRequest c q (inputStreamBody body')
  receiveResponse c $ \response _ -> f response
  closeConnection c

putImageChecksum :: ByteString -> ByteString -> LB.ByteString
  -> (Response -> Assertion) -> IO ()
putImageChecksum host image checksum f = withOpenSSL $ do
  let namespace = "quux"
      url = B.concat ["/v1/images/" `B.append` image `B.append` "/checksum"]
  q <- buildRequest $ do
    http PUT url
    -- TODO The official registry does a 500 if there is no version.
    setHeader "User-Agent" "fake-docker/1.1.2"
    setAuthorizationBasic namespace "thud"
    -- TODO Older clients use tarsum+sha256
    setHeader "X-Docker-Checksum-Payload" $ B.concat $ LB.toChunks checksum
    setContentLength 0

  c <- establishConnection $ "https://" `B.append` host
  sendRequest c q emptyBody
  receiveResponse c $ \response _ -> f response
  closeConnection c

putRepository :: ByteString -> LB.ByteString
  -> (Response -> Assertion) -> IO ()
putRepository host json f = withOpenSSL $ do
  let namespace = "quux"
      -- TODO The official registry accepts also quuxbar instead of quux/bar.
      url = B.concat ["/v1/repositories/" `B.append` namespace `B.append` "/bar/"]
      body = json
  q <- buildRequest $ do
    http PUT url
    setAuthorizationBasic namespace "thud"
    setContentLength (fromIntegral $ LB.length body)
    setContentType "application/json"

  c <- establishConnection $ "https://" `B.append` host
  body' <- Streams.fromLazyByteString body
  sendRequest c q (inputStreamBody body')
  receiveResponse c $ \response _ -> f response
  closeConnection c

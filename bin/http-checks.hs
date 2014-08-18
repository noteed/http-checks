{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.List (unfoldr)
import Data.Version (showVersion)
import Paths_http_checks (version)
import System.Console.CmdArgs.Implicit
import Test.HUnit

import Network.Docker.Registry.Checks
import Network.Docker.Registry.Types
import Network.Docker.Remote (getContainers, GetContainers(..))
import Network.Http.Checks

hardCodedImage :: ByteString
hardCodedImage = B.replicate 64 '1'

hardCodedRepository :: IO Repository
hardCodedRepository = do
  -- TODO Official repository accepts non-hexadecimal characters for the
  -- image ID.
  -- TODO Official repository does 500 on non-ascii checksums.
  let image = hardCodedImage
      json = LB.fromChunks ["{\"id\":\"", image, "\"}"]
  -- TODO The layer is done with make-layer.sh.
  layer <- LB.readFile "etc.tar.gz"

  let i = Image
        { imageName = image
        , imageJson = json
        , imageLayer = layer
        }
      r = Repository
        { repositoryHost = "registry.local"
        , repositoryCredentials = Just ("quux", "thud")
        , repositoryNamespace = "quux"
        , repositoryName = "bar"
        , repositoryImages = [(i, ["alpha", "beta"])]
        }
  return r

exampleImage :: ByteString -> IO Image
exampleImage i = do
  return Image
    { imageName = i
    , imageLayer = ""
    , imageJson = LB.fromChunks ["{\"id\":\"", i, "\"}"]
    }

exampleRepository :: ByteString -> IO Repository
exampleRepository repo = do
  return Repository
    { repositoryHost = "registry.local"
    , repositoryCredentials = Just ("quux", "thud")
    , repositoryNamespace = "quux"
    , repositoryName = repo
    , repositoryImages = []
    }

main :: IO ()
main = (runCmd =<<) $ cmdArgs $
  modes
    [ cmdSelfCheck
    , cmdDockerPushJson
    , cmdDockerPushRepository
    , cmdDockerFlow
    , cmdDockerSuite
    , cmdDockerGenerate
    , cmdDockerAll
    , cmdDockerRemote
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
  | CmdDockerSuite
  | CmdDockerGenerate
  | CmdDockerAll
  | CmdDockerRemote
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
      \push image meta data, image layer, image checksum. \
      \The repository must not exist."
    &= explicit
    &= name "docker-flow"

-- | Create a 'DockerSuite' command.
cmdDockerSuite :: Cmd
cmdDockerSuite = CmdDockerSuite
    &= help "A suite of hand-crafted tests (hopefully `docker-generate` \
      \will be more exhaustive in the future)."
    &= explicit
    &= name "docker-suite"

-- | Create a 'DockerGenerate' command.
cmdDockerGenerate :: Cmd
cmdDockerGenerate = CmdDockerGenerate
    &= help "Generate queries to a Docker registry."
    &= explicit
    &= name "docker-generate"

-- | Create a 'DockerAll' command.
cmdDockerAll :: Cmd
cmdDockerAll = CmdDockerAll
    &= help "Combine all the Docker tests."
    &= explicit
    &= name "docker-all"

-- | Create a 'DockerRemote' command.
cmdDockerRemote :: Cmd
cmdDockerRemote = CmdDockerRemote
    &= help "Dummy command to try the bindings."
    &= explicit
    &= name "docker-remote"

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
  r <- hardCodedRepository
  let i = fst . head $ repositoryImages r
  _ <- runTestTT $ TestList
    [ testList "Check pushing mal-formed meta-data"
      [ checkPushImageJson 400 "Empty body" r i { imageJson = "" }
      , checkPushImageJson 400 "Empty JSON object" r i { imageJson = "{}" }
      , checkPushImageJson 400 "Invalid JSON" r i { imageJson = "{" }
      , checkPushImageJson 400 "Invalid image ID in JSON" r i
        { imageJson = "{\"id\":\"a\"}" }
      ]

    , testList "Check pushing meta-data for already existing image"
      [ checkPushImageJson 409 "Existing image" r i
      ]
    ]
  return ()

runCmd CmdDockerPushRepository{..} = do
  r <- hardCodedRepository
  _ <- runTestTT $ TestList
    [ testList "Check pushing mal-formed repository data"
      [ checkPushRepository' 400 "Empty body" r ""
      , checkPushRepository' 400 "Empty JSON object" r "{}"
      , checkPushRepository' 400 "Invalid JSON" r "{"
      ]

    , testList "Check pushing mal-formed repository data, but it is accepted."
      [ checkPushRepository' 200 "Partial image ID" r
        "[{\"id\":\"aaaaaaaaaaaa\"}]"
      , checkPushRepository' 200 "Replaying exactly the same request." r
        "[{\"id\":\"aaaaaaaaaaaa\"}]"
      ]

    , testList "Check pushing correctly formed repository data."
      [ checkPushRepository 200 "Just an ID." r
      , checkPushRepository 200 "Replaying exactly the same request." r
      , checkPushRepository' 200 "Empty image list" r "[]"
      ]
    ]
  return ()

runCmd CmdDockerFlow{..} = do
  -- TODO Official repository accepts non-hexadecimal characters for the
  -- image ID.
  -- TODO Official repository does 500 on non-ascii checksums.
  let checksum' = "sha256:" `LB.append` LC.replicate 64 '9'

  r <- hardCodedRepository
  let i = fst . head $ repositoryImages r
  _ <- runTestTT $ TestList
    [ testList "Full repository creation."
      [ checkPushRepository 200 "Push the repository." r
      , checkPullImageJson 404 "Check the image doesn't exist." r i
      , checkPushImageJson 200 "Push image meta-data" r i
      , checkPullImageJson 400 "Check the image is being uploaded." r i
      , checkPushImageLayer 200 "Push image layer." r i
      , checkPullImageJson 400 "Check the image is still being uploaded." r i
      , checkPushImageChecksum' 400 "Push wrong image checksum." r i checksum'
      , checkPullImageJson 400 "Check the image is still being uploaded." r i
      , checkPushImageChecksum 404 "Push checksum for non-existing image." r i
          { imageName = "DEAD" `B.append` B.replicate 60 '0' }
      , checkPushImageChecksum 200 "Push correct image checksum." r i
      -- TODO I thought the image would be invalidated by the wrong checksum.
      , checkPullImageJson 200 "Check the image is available." r i
      , checkPushImageChecksum 409 "Already pushed image checksum." r i
      , checkPushRepositoryTag 200 "Push image tag." r i "alpha"
      , checkPushRepositoryTag 200 "Push image tag." r i "beta"
      -- TODO Push also v1/repositories/quux/bar/images.
      -- https://docs.docker.com/reference/api/hub_registry_spec/
      -- says it should be a list of id/tags/checksum, but I only see
      -- an empty list in my tests. Or it was replaced by the individual
      -- checksum and tag uploads.
      ]
    ]
  return ()

runCmd CmdDockerSuite{..} = do
  r <- exampleRepository (error "Only the credentials are used.")
  etc <- LB.readFile "etc.tar.gz"

  -- The three 404 in the following tests demonstrate that the protocol
  -- requires a strict ordering json -> layer -> checksum when pushing an
  -- image.
  --
  -- This also shows that images can be pushed without reference to a
  -- repository, or without being referenced by a repository.

  i0 <- exampleImage "non-existing-0"
  _ <- runTestTT $ TestList
    [ testList "Pushing checksum only."
      [ checkPushImageChecksum 404 "Push checksum for non-existing image." r i0
      ]
    ]

  i1 <- exampleImage "non-existing-1"
  _ <- runTestTT $ TestList
    [ testList "Pushing json and checksum only."
      -- TODO This test generates a 500:
      -- https://github.com/docker/docker-registry/issues/527
      [ checkPushImageJson 200 "Push image meta-data." r i1
      , checkPushImageChecksum 404 "Push checksum for non-existing image." r i1
      ]
    ]

  i2 <- exampleImage "non-existing-2"
  _ <- runTestTT $ TestList
    [ testList "Pushing layer only."
      [ checkPushImageLayer 404 "Push image layer." r i2 { imageLayer = etc }
      ]
    ]

  -- The following tests do the json -> layer -> checksum pushes in the correct
  -- order, but try to repeat previous step(s).
  --
  -- Repeated jsons, or repeated layers are possible. On the other hand,
  -- repeated checksums are not possible.
  --
  -- Pushing again json after the layer is possible.
  --
  -- Once the checksum is pushed, neither json, layer, or checksum can be
  -- pushed again.

  i3 <- exampleImage "non-existing-3"
  _ <- runTestTT $ TestList
    [ testList "Pushing json more than once."
      [ checkPushImageJson 200 "Push image meta-data 1/3." r i3
      , checkPushImageJson 200 "Push image meta-data 2/3." r i3
      , checkPushImageJson 200 "Push image meta-data 3/3." r i3
      ]
    ]

  i4 <- exampleImage "non-existing-4"
  _ <- runTestTT $ TestList
    [ testList "Pushing json then layer more than once."
      [ checkPushImageJson 200 "Push image meta-data." r i4
      , checkPushImageLayer 200 "Push image layer 1/3." r i4 { imageLayer = etc }
      , checkPushImageLayer 200 "Push image layer 2/3." r i4 { imageLayer = etc }
      , checkPushImageLayer 200 "Push image layer 3/3." r i4 { imageLayer = etc }
      ]
    ]

  i5_ <- exampleImage "non-existing-5"
  let i5 = i5_ { imageLayer = etc }
  _ <- runTestTT $ TestList
    [ testList "Pushing json, layer then checksum more than once."
      [ checkPushImageJson 200 "Push image meta-data." r i5
      , checkPushImageLayer 200 "Push image layer." r i5
      , checkPushImageChecksum 200 "Push checksum for non-existing image 1/3." r i5
      , checkPushImageChecksum 409 "Push checksum for non-existing image 2/3." r i5
      , checkPushImageChecksum 409 "Push checksum for non-existing image 3/3." r i5
      ]
    ]

  i6 <- exampleImage "non-existing-6"
  _ <- runTestTT $ TestList
    [ testList "Pushing json then layer, then json again."
      [ checkPushImageJson 200 "Push image meta-data." r i6
      , checkPushImageLayer 200 "Push image layer." r i6 { imageLayer = etc }
      , checkPushImageJson 200 "Push image meta-data again." r i6
      ]
    ]

  i7_ <- exampleImage "non-existing-7"
  let i7 = i7_ { imageLayer = etc }
  _ <- runTestTT $ TestList
    [ testList "Pushing json, layer, checksum then json again."
      [ checkPushImageJson 200 "Push image meta-data." r i7
      , checkPushImageLayer 200 "Push image layer." r i7
      , checkPushImageChecksum 200 "Push checksum for non-existing image." r i7
      , checkPushImageJson 409 "Push image meta-data 1/3." r i7
      , checkPushImageJson 409 "Push image meta-data 2/3." r i7
      , checkPushImageJson 409 "Push image meta-data 3/3." r i7
      ]
    ]

  return ()

runCmd CmdDockerGenerate{..} = do
  layer <- LB.readFile "etc.tar.gz"
  let image0 = B.replicate 64 'b'
      image1 = B.replicate 64 'c'
  fromInitial Repository
    { repositoryHost = "registry.local"
    , repositoryCredentials = Just ("quux", "thud")
    , repositoryNamespace = "quux"
    , repositoryName = "foo"
    , repositoryImages =
      [ ( Image
          { imageName = image0
          , imageJson = LB.fromChunks ["{\"id\":\"", image0, "\"}"]
          , imageLayer = layer
          }
        , [] )
      , ( Image
          { imageName = image1
          , imageJson = LB.fromChunks ["{\"id\":\"", image1, "\"}"]
          , imageLayer = layer
          }
        , [] )
      ]
    }

runCmd CmdDockerAll{..} = do
  runCmd CmdDockerPushRepository
  runCmd CmdDockerFlow
  runCmd CmdDockerSuite
  runCmd CmdDockerGenerate
  runCmd CmdDockerPushJson { cmdImage = B.unpack hardCodedImage }

runCmd CmdDockerRemote{..} = do
  getContainers AllContainers >>= print

testList :: String -> [Test] -> Test
testList title list = TestLabel title $ TestList list

----------------------------------------------------------------------
-- Trying to model the protocol
----------------------------------------------------------------------

-- | Protocol state
-- I am not happy with this. Instead I need to model the server state (e.g.
-- does it know already about that repository or that image) and be able to
-- construct queries that put it in a new valid or invalid state, or leave it
-- as-is.
data S =
    Initial
  | RepositoryPushed [Image]
  | ImageDoesntExist [Image]
  | ImageJsonPushed [Image]
  | ImageLayerPushed [Image]
  | ImagesPushed
  | TagsPushed

fromInitial :: Repository -> IO ()
fromInitial r = do
  let steps = unfoldr (step r) Initial
  _ <- runTestTT $ TestLabel "Some title" $ TestList
    steps
  return ()

-- | Return a test that when executed, should let you in the new state.
step :: Repository -> S -> Maybe (Test, S)
step r Initial = Just
  ( checkPushRepository 200 "Push the repository." r
  , RepositoryPushed $ map fst $ repositoryImages r )
  -- TODO If repositoryImages is empty, directly generate ImagesPushed.

-- This one is normally not generated as we skip creating that state.
step _ (RepositoryPushed []) = Just
  ( TestLabel "Trivial RepositoryPushed" $ TestCase $ return (), ImagesPushed )

step r (RepositoryPushed (i:is)) = Just
  ( checkPullImageJson 404 "Check the image doesn't exist." r i
  , ImageDoesntExist (i:is) )

-- This one is normally not generated as we skip creating that state.
step _ (ImageDoesntExist []) = Just
  ( TestLabel "Trivial ImageDoesntExist" $ TestCase $ return (), ImagesPushed )

step r (ImageDoesntExist (i:is)) = Just
  ( checkPushImageJson 200 "Push image meta-data" r i
  , ImageJsonPushed (i:is) )

-- This one is normally not generated as we skip creating that state.
step _ (ImageJsonPushed []) = Just
  ( TestLabel "Trivial ImageJsonPushed" $ TestCase $ return (), ImagesPushed )

step r (ImageJsonPushed (i:is)) = Just
  ( checkPushImageLayer 200 "Push image layer." r i
  , ImageLayerPushed (i:is) )

-- This one is normally not generated as we skip creating that state.
step _ (ImageLayerPushed []) = Just
  ( TestLabel "Trivial ImageLayerPushed" $ TestCase $ return (), ImagesPushed )

step r (ImageLayerPushed [i]) = Just
  -- TODO The official registry accepts re-pushing the json.
  --( checkPushImageJson 200 "Push correct image json." r i
  ( checkPushImageChecksum 200 "Push correct image checksum." r i
  , ImagesPushed )

step r (ImageLayerPushed (i:is)) = Just
  ( checkPushImageChecksum 200 "Push correct image checksum." r i
  , RepositoryPushed is )

step _ _ = Nothing

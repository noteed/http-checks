{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Bindings to the Docker Registry API. If you want more control on the
-- exact data passed to the server, use `Network.Docker.Registry.Internal`.
module Network.Docker.Registry where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Digest.Pure.SHA (showDigest, sha256)
import Data.List (intersperse)

import Network.Docker.Registry.Internal
import Network.Docker.Registry.Types

----------------------------------------------------------------------
-- Repositories
----------------------------------------------------------------------

pushRepository :: Repository -> IO Int
pushRepository r@Repository{..} =
  let json = repositoryJson r in
  putRepository repositoryCredentials repositoryHost
    repositoryNamespace repositoryName json

pushRepositoryTag :: Repository -> Image -> Tag -> IO Int
pushRepositoryTag Repository{..} Image{..} tag =
  putRepositoryTag repositoryCredentials repositoryHost repositoryNamespace
    repositoryName tag imageName

----------------------------------------------------------------------
-- Images
----------------------------------------------------------------------

pushImageJson :: Repository -> Image -> IO Int
pushImageJson Repository{..} Image{..} =
  putImageJson repositoryCredentials repositoryHost imageName imageJson

pullImageJson :: Repository -> Image -> IO (Int, ByteString)
pullImageJson Repository{..} Image{..} =
  getImageJson repositoryCredentials repositoryHost imageName

pushImageLayer :: Repository -> Image -> IO Int
pushImageLayer Repository{..} Image{..} =
  putImageLayer repositoryCredentials repositoryHost imageName imageLayer

pushImageChecksum :: Repository -> Image -> IO Int
pushImageChecksum Repository{..} i@Image{..} =
  let checksum = imageChecksum i in
  putImageChecksum repositoryCredentials repositoryHost imageName checksum

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

repositoryJson :: Repository -> LC.ByteString
repositoryJson Repository{..} = LB.fromChunks $
    "[" :
    (concat . (intersperse [","]) . map (\i -> ["{\"id\":\"", imageName $ fst i, "\"}"]))
    repositoryImages
    ++ ["]"]

imageChecksum :: Image -> LC.ByteString
imageChecksum i = "sha256:" `LB.append` digest256
  (LB.concat [imageJson i, "\n", imageLayer i])
  -- TODO Use the incremental interface.
  -- The checksum used by Docker depends on the exact string representation of
  -- JSON meta-data...

digest256 :: LC.ByteString -> LC.ByteString
digest256 = LC.pack . showDigest . sha256

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
import System.IO.Streams.SHA
import qualified System.IO.Streams as Streams

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
pushImageChecksum Repository{..} i@Image{..} = do
  -- TODO Compute the checksum when the layer is uploaded, and cache it
  -- to re-use it here.
  checksum <- imageChecksum i
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

imageChecksum :: Image -> IO LC.ByteString
imageChecksum i = do
  digest256 <- imageLayer i (\is -> do
    j <- Streams.fromLazyByteString $ imageJson i `LB.append` "\n"
    is' <- Streams.appendInputStream j is
    (is'', getSha256) <- sha256Input is'
    Streams.skipToEof is''
    getSha256)
  return $ "sha256:" `LB.append` (LC.pack $ showDigest digest256)
  -- The checksum used by Docker depends on the exact string representation of
  -- JSON meta-data...

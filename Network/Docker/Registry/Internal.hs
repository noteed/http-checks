{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This is the bindings per-se to the Docker Registry API but you probably
-- want to use the module `Network.Docker.Registry`. Still this module is
-- available if you have your own data types to represent repositories and
-- images and don't want to use the ones defined in
-- `Network.Docker.Registry.Types`.
module Network.Docker.Registry.Internal where

import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Builder (toByteString)
import qualified Blaze.ByteString.Builder.ByteString as Builder
import Control.Exception (throwIO, Exception)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Monoid (mappend, mempty)
import Data.Typeable (Typeable)
import Network.Http.Client
import OpenSSL (withOpenSSL)
import System.IO.Streams (OutputStream)
import qualified System.IO.Streams as Streams

import Network.Docker.Registry.Types

getImageJson :: Credentials -> ByteString -> ByteString -> IO (Int, ByteString)
getImageJson credentials host image = withOpenSSL $ do
  let url = B.concat ["/v1/images/" `B.append` image `B.append` "/json"]
  q <- buildRequest $ do
    http GET url
    maybe (return ()) (uncurry setAuthorizationBasic) credentials

  let handler response i1 = do
        i2 <- Streams.map Builder.fromByteString i1
        x <- Streams.fold mappend mempty i2
        return (getStatusCode response, Builder.toByteString x)

  sendAndReceive host q emptyBody handler

putImageJson :: Credentials -> ByteString -> ByteString -> LB.ByteString -> IO Int
putImageJson credentials host image json = withOpenSSL $ do
  let url = B.concat ["/v1/images/" `B.append` image `B.append` "/json"]
      body = json
  q <- buildRequest $ do
    http PUT url
    maybe (return ()) (uncurry setAuthorizationBasic) credentials
    setContentLength (fromIntegral $ LB.length body)
    setContentType "application/json"

  body' <- Streams.fromLazyByteString body
  sendAndReceive host q (inputStreamBody body') getStatusCode'

-- | Upload an image layer.
putImageLayer :: Credentials -> ByteString -> ByteString -> LB.ByteString -> IO Int
putImageLayer credentials host image layer = do
  let upload o = do
        body <- Streams.fromLazyByteString layer
        inputStreamBody body o
  putImageLayer' credentials host image upload

-- | Upload an image layer but don't send the terminating chunk.
putImageLayerBroken :: Credentials -> ByteString -> ByteString -> LB.ByteString
  -> IO Int
putImageLayerBroken credentials host image layer = do
  let brokenUpload o = do
        Streams.write (Just (Builder.fromLazyByteString layer)) o
        throwIO MisconductException
  putImageLayer' credentials host image brokenUpload

putImageLayer' :: Credentials -> ByteString -> ByteString
  -> (OutputStream Builder -> IO ()) -> IO Int
putImageLayer' credentials host image upload = withOpenSSL $ do
  let url = B.concat ["/v1/images/" `B.append` image `B.append` "/layer"]
  q <- buildRequest $ do
    http PUT url
    -- TODO The official registry does a 500 if there is no version.
    setHeader "User-Agent" "docker/1.1.2-fake"
    maybe (return ()) (uncurry setAuthorizationBasic) credentials
    setTransferEncoding -- "chunked"

  sendAndReceive host q upload getStatusCode'

putImageChecksum :: Credentials -> ByteString -> ByteString -> LB.ByteString
  -> IO Int
putImageChecksum credentials host image checksum = withOpenSSL $ do
  let url = B.concat ["/v1/images/" `B.append` image `B.append` "/checksum"]
  q <- buildRequest $ do
    http PUT url
    -- TODO The official registry does a 500 if there is no version.
    setHeader "User-Agent" "docker/1.1.2-fake"
    maybe (return ()) (uncurry setAuthorizationBasic) credentials
    -- TODO Older clients use tarsum+sha256
    setHeader "X-Docker-Checksum-Payload" $ B.concat $ LB.toChunks checksum
    setContentLength 0

  sendAndReceive host q emptyBody getStatusCode'

putRepository :: Credentials -> ByteString -> ByteString -> ByteString
  -> LB.ByteString -> IO Int
putRepository credentials host namespace repo json = withOpenSSL $ do
      -- TODO The official registry accepts also quuxbar instead of quux/bar.
  let url = B.concat ["/v1/repositories/" `B.append` namespace `B.append`
        "/" `B.append` repo `B.append` "/"]
      body = json
  q <- buildRequest $ do
    http PUT url
    maybe (return ()) (uncurry setAuthorizationBasic) credentials
    setContentLength (fromIntegral $ LB.length body)
    setContentType "application/json"

  body' <- Streams.fromLazyByteString body
  sendAndReceive host q (inputStreamBody body') getStatusCode'

putRepositoryTag :: Credentials -> ByteString -> ByteString -> ByteString
  -> ByteString -> ByteString -> IO Int
putRepositoryTag credentials host namespace repo tag image = withOpenSSL $ do
      -- TODO The official registry accepts also quuxbar instead of quux/bar.
  let url = B.concat ["/v1/repositories/" `B.append` namespace `B.append` "/"
        `B.append` repo `B.append` "/tags/" `B.append` tag]
      body = B.concat ["\"", image, "\""]
  q <- buildRequest $ do
    http PUT url
    maybe (return ()) (uncurry setAuthorizationBasic) credentials
    setContentLength (fromIntegral $ B.length body)
    setContentType "application/json"

  body' <- Streams.fromByteString body
  sendAndReceive host q (inputStreamBody body') getStatusCode'

sendAndReceive host q body handler = do
  -- TODO Use bracket to close the connection.
  c <- establishConnection $ "https://" `B.append` host
  sendRequest c q body
  r <- receiveResponse c handler
  closeConnection c
  return r

getStatusCode' response _ = return $ getStatusCode response

-- | Exception used in a handler (for `sendRequest`) to intentionnally brake
-- an upload.
data MisconductException = MisconductException
        deriving (Typeable, Show)

instance Exception MisconductException

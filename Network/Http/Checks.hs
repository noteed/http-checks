{-# LANGUAGE OverloadedStrings #-}
-- | Basic HTTP checks as HUnit tests.
-- This is intended to be a small library to write HUnit checks against live
-- services.
-- TODO Extract this module into its own library.
module Network.Http.Checks where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Network.Http.Client
import OpenSSL (withOpenSSL)
import Test.HUnit

-- | Ok.
is200 :: ByteString -> ByteString -> Test
is200 = hasCode' 200

-- | Permanent redirect.
is301 :: ByteString -> ByteString -> Test
is301 = hasCode' 301

-- | Temporary redirect.
is307 :: ByteString -> ByteString -> Test
is307 = hasCode' 307

hasCode' :: StatusCode -> ByteString -> ByteString -> Test
hasCode' n base uri = TestLabel (B.unpack (base `B.append` uri) ++ " " ++ show n) $
  TestCase $ hasCode n base uri []

hasCode :: StatusCode -> ByteString -> ByteString
  -> [(ByteString, Maybe ByteString)] -> Assertion
hasCode n = checkResponse $ checkCode n

checkCode :: StatusCode -> Response -> Assertion
checkCode n response = expectCode n $ getStatusCode response

expectCode :: Int -> Int -> Assertion
expectCode n m =
  unless (n == m) $
    assertFailure $ "Expected HTTP code " ++ show n ++
      " but received " ++ show m ++ " instead."

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


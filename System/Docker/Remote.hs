{-# LANGUAGE OverloadedStrings #-}
-- | Bindings to the Docker Remote API.
module System.Docker.Remote where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson (object, FromJSON(..), ToJSON(..), Value(..), (.=), (.:), (.:?))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Int (Int64)

import Network.Aeson.Client (apiGet)

getContainers :: GetContainers -> IO [Container]
getContainers query = do
  mxs <- apiGet Nothing "unix:///var/run/docker.sock"
    "/containers/json" params
  case mxs of
    Nothing -> error "Error in apiGet result, in getContainers."
    Just xs -> return xs
  where
  -- Always include the size (that's what our Container data type
  -- expects) -- I don't know if there is a serious performance cost
  -- to always include it.
  params = ("size", Just "true") : case query of
  -- TODO Maybe it is possible to combine since/before/limit ?
    RunningContainers -> []
    AllContainers -> [("all", Just "true")]
    LastContainers n -> [("limit", Just . B.pack $ show n)]
    SinceContainer (ContainerId i) -> [("since", Just i)]
    BeforeContainer (ContainerId i) -> [("before", Just i)]

newtype ContainerId = ContainerId ByteString
  deriving Show

-- | Possible modifiers for the `getContainers` query.
data GetContainers =
    RunningContainers
  -- ^ Return running containers.
  | AllContainers
  -- ^ Return all containers, i.e. including non-running ones.
  | LastContainers Int
  -- ^ Return the n last containers, including non-running ones.
  | SinceContainer ContainerId
  -- ^ Return all containers created since the given ID, including
  -- non-running ones.
  | BeforeContainer ContainerId
  -- ^ Return all containers created before the given ID, including
  -- non-running ones.

-- | Result of `GET /containers/json`.
data Container = Container
  { containerId :: ContainerId
  , containerImage :: String
  , containerCommand :: String
  , containerCreated :: Int64
  , containerStatus :: String
  -- , containerPorts :: String -- TODO
  , containerSizeRw :: Int64
  , containerSizeRootFs :: Int64
  }
  deriving Show

-- | Attempts to parse JSON into a Container.
instance FromJSON Container where
  parseJSON (Object v) = Container
    <$> (ContainerId <$> v .: "Id")
    <*> v .: "Image"
    <*> v .: "Command"
    <*> v .: "Created"
    <*> v .: "Status"
    <*> v .: "SizeRw"
    <*> v .: "SizeRootFs"
  parseJSON _ = mzero

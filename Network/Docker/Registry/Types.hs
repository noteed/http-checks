{-# LANGUAGE RankNTypes #-}
module Network.Docker.Registry.Types where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import System.IO.Streams (InputStream)

type Credentials = Maybe (ByteString, ByteString)

data Repository = Repository
  { repositoryHost :: ByteString
  , repositoryCredentials :: Credentials
  , repositoryNamespace :: ByteString
  , repositoryName :: ByteString
  , repositoryImages :: [(Image, [Tag])]
  }

data Image = Image
  { imageName :: ByteString
  , imageJson :: LB.ByteString
  , imageLayer :: LayerAsInput
  -- ^ The image layer is fed to the given function when this attribute is run.
  }

type Tag = ByteString

type LayerAsInput = forall a. (InputStream ByteString -> IO a) -> IO a

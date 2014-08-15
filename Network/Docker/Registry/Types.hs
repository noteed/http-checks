module Network.Docker.Registry.Types where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB

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
  , imageLayer :: LB.ByteString
  }

type Tag = ByteString

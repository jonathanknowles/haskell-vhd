module SharedTypes where

import Control.Exception
import qualified Data.ByteString.Char8 as B
import Data.Word

data Cookie  = Cookie B.ByteString deriving Show
cookie value = assert (B.length value <= 8) $ Cookie value

data Version      = Version VersionMajor VersionMinor deriving Show
type VersionMajor = Word16
type VersionMinor = Word16
version           = Version

data CheckSum = CheckSum Word32 deriving Show
checkSum      = CheckSum

data UniqueId  = UniqueId B.ByteString deriving Show
uniqueId value = assert (B.length value == 16) $ UniqueId value

data TimeStamp = TimeStamp Word32 deriving Show
timeStamp      = TimeStamp

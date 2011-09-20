module SharedTypes where

import Control.Exception
import qualified Data.ByteString.Char8 as B
import Data.UUID
import Data.Word

newtype Cookie = Cookie B.ByteString deriving Show
cookie value   = assert (B.length value <= 8) $ Cookie value
cookieDefault  = cookie B.empty

data Version      = Version VersionMajor VersionMinor deriving Show
type VersionMajor = Word16
type VersionMinor = Word16
versionDefault    = Version 1 0

type CheckSum = Word32

newtype UniqueId = UniqueId B.ByteString deriving Show
uniqueId value   = assert (B.length value == 16) $ UniqueId value
uniqueIdDefault  = uniqueId $ B.replicate 16 '\0'

type TimeStamp   = Word32
timeStampDefault = 0


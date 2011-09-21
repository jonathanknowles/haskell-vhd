module SharedTypes where

import Control.Exception
import qualified Data.ByteString as B
import Data.Word

type CheckSum  = Word32
type Offset    = Word64
type Size      = Word64
type TimeStamp = Word32

newtype Cookie   = Cookie   B.ByteString deriving Show
newtype UniqueId = UniqueId B.ByteString deriving Show

data Version      = Version VersionMajor VersionMinor deriving Show
type VersionMajor = Word16
type VersionMinor = Word16

cookie   value = assert (B.length value <=  8) $ Cookie   value
uniqueId value = assert (B.length value == 16) $ UniqueId value

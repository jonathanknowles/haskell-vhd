module HeaderTypes where

import Control.Exception
import qualified Data.ByteString.Char8 as B
import Data.Word

import SharedTypes

data Offset = Offset Word64 deriving Show

dataOffset  = Offset
tableOffset = Offset

headerVersion = Version

data EntryCount = EntryCount Word32 deriving Show
maxTableEntries = EntryCount

data BlockSize = BlockSize Word32 deriving Show
blockSize = BlockSize

data ParentUnicodeName = ParentUnicodeName B.ByteString deriving Show
-- TODO: We should check the value is valid UTF-16
parentUnicodeName value   = assert (B.length value <= 512) $ ParentUnicodeName value

parentUniqueId = UniqueId

parentTimeStamp = TimeStamp

data ParentLocatorEntries = ParentLocatorEntries
	{ parentLocatorEntry1 :: ParentLocatorEntry
	, parentLocatorEntry2 :: ParentLocatorEntry
	, parentLocatorEntry3 :: ParentLocatorEntry
	, parentLocatorEntry4 :: ParentLocatorEntry
	, parentLocatorEntry5 :: ParentLocatorEntry
	, parentLocatorEntry6 :: ParentLocatorEntry
	, parentLocatorEntry7 :: ParentLocatorEntry
	, parentLocatorEntry8 :: ParentLocatorEntry
	} deriving Show

data ParentLocatorEntry  = ParentLocatorEntry B.ByteString deriving Show
parentLocatorEntry value = assert (B.length value == 24) $ ParentLocatorEntry value


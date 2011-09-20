module HeaderTypes where

import Control.Exception
import qualified Data.ByteString.Char8 as B
import Data.Word
import SharedTypes

type DataOffset   = Word64
dataOffsetDefault = 0xFFFFFFFF

type TableOffset   = Word64
tableOffsetDefault = 0

type HeaderVersion = Version
headerVersionDefault = versionDefault

type MaxTableEntries = Word32
maxTableEntriesDefault = 0

type BlockSize = Word32
blockSizeDefault = 0x00200000

newtype ParentUnicodeName = ParentUnicodeName B.ByteString
-- TODO: We should check the value is valid UTF-16
parentUnicodeName value   = assert (B.length value <= 512) $ ParentUnicodeName value
parentUnicodeNameDefault  = parentUnicodeName B.empty

data ParentLocatorEntries = ParentLocatorEntries
	{ parentLocatorEntry1 :: ParentLocatorEntry
	, parentLocatorEntry2 :: ParentLocatorEntry
	, parentLocatorEntry3 :: ParentLocatorEntry
	, parentLocatorEntry4 :: ParentLocatorEntry
	, parentLocatorEntry5 :: ParentLocatorEntry
	, parentLocatorEntry6 :: ParentLocatorEntry
	, parentLocatorEntry7 :: ParentLocatorEntry
	, parentLocatorEntry8 :: ParentLocatorEntry
	}

data ParentLocatorEntry  = ParentLocatorEntry B.ByteString
parentLocatorEntry value = assert (B.length value == 24) $ ParentLocatorEntry value


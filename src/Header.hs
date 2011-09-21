module Header where

import HeaderTypes
import SharedTypes

data Header = Header
	{ cookie               :: Cookie
	, dataOffset           :: Offset
	, tableOffset          :: Offset
	, headerVersion        :: Version
	, maxTableEntries      :: EntryCount
	, blockSize            :: BlockSize
	, checkSum             :: CheckSum
	, parentUniqueId       :: UniqueId
	, parentTimeStamp      :: TimeStamp
	, parentUnicodeName    :: ParentUnicodeName
	, parentLocatorEntries :: ParentLocatorEntries
	} deriving Show

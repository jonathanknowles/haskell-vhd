module Data.VHD.Header where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Bits
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import Data.VHD.HeaderTypes hiding (parentLocatorEntry, parentLocatorEntries, parentUnicodeName)
import Data.VHD.SharedTypes hiding (cookie, uniqueId)
import qualified Data.VHD.HeaderTypes as H
import qualified Data.VHD.SharedTypes as S
import Data.Serialize
import Data.Serialize.Get
import Data.Serialize.Put

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

instance Serialize Header where
	get = Header
		<$> getCookie
		<*> getDataOffset
		<*> getTableOffset
		<*> getHeaderVersion
		<*> getMaxTableEntries
		<*> getBlockSize
		<*> getCheckSum
		<*> getParentUniqueId
		<*> getParentTimeStamp
		<*> getParentUnicodeName
		<*> getParentLocatorEntries
		<*  getPadding
	put h = do
		putCookie               $ cookie               h
		putDataOffset           $ dataOffset           h
		putTableOffset          $ tableOffset          h
		putHeaderVersion        $ headerVersion        h
		putMaxTableEntries      $ maxTableEntries      h
		putBlockSize            $ blockSize            h
		putCheckSum             $ checkSum             h
		putParentUniqueId       $ parentUniqueId       h
		putParentTimeStamp      $ parentTimeStamp      h
		putParentUnicodeName    $ parentUnicodeName    h
		putParentLocatorEntries $ parentLocatorEntries h
		putPadding

getPadding = getByteString 427
putPadding = putByteString $ B.replicate 427 0

--duplicated
getPaddedByteString length = removePadding <$> getByteString length where
	removePadding = B.takeWhile (> 0)
putPaddedByteString length string = putByteString paddedString where
	paddedString  = assert (paddingLength > 0) $ B.append string padding
	paddingLength = length - B.length string
	padding       = B.replicate paddingLength 0

--duplicated
getCookie = S.cookie <$> getPaddedByteString 8
putCookie (Cookie c) = putPaddedByteString 8 c

getBlockSize       = getWord32be
putBlockSize       = putWord32be
getCheckSum        = getWord32be
putCheckSum        = putWord32be
getDataOffset      = getWord64be
putDataOffset      = putWord64be
getMaxTableEntries = getWord32be
putMaxTableEntries = putWord32be
getTableOffset     = getWord64be
putTableOffset     = putWord64be
getParentTimeStamp = getWord32be
putParentTimeStamp = putWord32be

getParentUniqueId = UniqueId <$> getByteString 16
putParentUniqueId (UniqueId i) = putByteString i

getVersion = Version <$> getWord16be <*> getWord16be
putVersion (Version major minor) = putWord16be major >> putWord16be minor

getHeaderVersion = getVersion
putHeaderVersion = putVersion

getParentUnicodeName = H.parentUnicodeName <$> getPaddedByteString 512
putParentUnicodeName (ParentUnicodeName c) = putPaddedByteString 512 c

getParentLocatorEntry = H.parentLocatorEntry <$> getByteString 24
putParentLocatorEntry (ParentLocatorEntry e) = putByteString e

getParentLocatorEntries = H.parentLocatorEntries <$> replicateM 8 getParentLocatorEntry
putParentLocatorEntries (ParentLocatorEntries es) = mapM_ putParentLocatorEntry es


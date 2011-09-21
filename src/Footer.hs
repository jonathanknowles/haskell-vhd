module Footer where

import Control.Applicative
import Control.Exception
import Data.Serialize
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Bits
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL

import FooterTypes hiding (creatorApplication)
import SharedTypes hiding (cookie, uniqueId)

import qualified SharedTypes as S
import qualified FooterTypes as F

data Footer = Footer
	{ cookie             :: Cookie
	, isTemporaryDisk    :: Bool
	, formatVersion      :: Version
	, dataOffset         :: Offset
	, timeStamp          :: TimeStamp
	, creatorApplication :: CreatorApplication
	, creatorVersion     :: Version
	, creatorHostOs      :: CreatorHostOs
	, originalSize       :: Size
	, currentSize        :: Size
	, diskGeometry       :: DiskGeometry
	, diskType           :: DiskType
	, checkSum           :: CheckSum
	, uniqueId           :: UniqueId
	, isSavedState       :: Bool
	} deriving Show

instance Serialize Footer where
	get = Footer
		<$> getCookie
		<*> getIsTemporaryDisk
		<*> getFormatVersion
		<*> getDataOffset
		<*> getTimeStamp
		<*> getCreatorApplication
		<*> getCreatorVersion
		<*> getCreatorHostOs
		<*> getOriginalSize
		<*> getCurrentSize
		<*> getDiskGeometry
		<*> getDiskType
		<*> getCheckSum
		<*> getUniqueId
		<*> getIsSavedState
		<*  getPadding
	put f = do
		putCookie             $ cookie             f
		putIsTemporaryDisk    $ isTemporaryDisk    f
		putFormatVersion      $ formatVersion      f
		putDataOffset         $ dataOffset         f
		putTimeStamp          $ timeStamp          f
		putCreatorApplication $ creatorApplication f
		putCreatorVersion     $ creatorVersion     f
		putCreatorHostOs      $ creatorHostOs      f
		putOriginalSize       $ originalSize       f
		putCurrentSize        $ currentSize        f
		putDiskGeometry       $ diskGeometry       f
		putDiskType           $ diskType           f
		putCheckSum           $ checkSum           f
		putUniqueId           $ uniqueId           f
		putIsSavedState       $ isSavedState       f
		putPadding

getPadding = getByteString 427
putPadding = putByteString $ B.replicate 427 0

getCheckSum     = getWord32be
putCheckSum     = putWord32be
getCurrentSize  = getWord64be
putCurrentSize  = putWord64be
getDataOffset   = getWord64be
putDataOffset   = putWord64be
getOriginalSize = getWord64be
putOriginalSize = putWord64be
getTimeStamp    = getWord32be
putTimeStamp    = putWord32be

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

getCreatorApplication = F.creatorApplication <$> getPaddedByteString 4
putCreatorApplication (CreatorApplication c) = putPaddedByteString 4 c

getCreatorHostOs = convert <$> getWord32be where
	convert 0x4D616320 = CreatorHostOsMacintosh
	convert 0x5769326B = CreatorHostOsWindows
	convert _          = CreatorHostOsUnknown
putCreatorHostOs v = putWord32be $ case v of
	CreatorHostOsMacintosh -> 0x4D616320
	CreatorHostOsWindows   -> 0x5769326B
	CreatorHostOsUnknown   -> 0

getDiskType = convert <$> getWord32be where
	convert 2 = DiskTypeFixed
	convert 3 = DiskTypeDynamic
	convert 4 = DiskTypeDifferencing
	convert _ = error "invalid disk type"
putDiskType v = putWord32be $ case v of
	DiskTypeFixed        -> 2
	DiskTypeDynamic      -> 3
	DiskTypeDifferencing -> 4

getDiskGeometry = DiskGeometry <$> getWord16be <*> getWord8 <*> getWord8
putDiskGeometry (DiskGeometry c h s) = putWord16be c >> putWord8 h >> putWord8 s

getIsTemporaryDisk = (\n -> n .&. 1 == 1) <$> getWord32be
putIsTemporaryDisk i = putWord32be (if i then 1 else 0)

getIsSavedState = (== 1) <$> getWord8
putIsSavedState i = putWord8 (if i then 1 else 0)

getUniqueId = UniqueId <$> getByteString 16
putUniqueId (UniqueId i) = putByteString i

getVersion = Version <$> getWord16be <*> getWord16be
putVersion (Version major minor) = putWord16be major >> putWord16be minor

getCreatorVersion = getVersion
putCreatorVersion = putVersion
getFormatVersion  = getVersion
putFormatVersion  = putVersion


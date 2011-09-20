module FooterTypes where

import Control.Exception
import qualified Data.ByteString.Char8 as B
import Data.Word
import SharedTypes

data IsTemporaryDisk = IsTemporaryDisk Bool deriving Show
isTemporaryDisk      = IsTemporaryDisk

formatVersion  = Version
creatorVersion = Version

data DataOffset = DataOffset Word64 deriving Show
dataOffset      = DataOffset

data CreatorApplication  = CreatorApplication B.ByteString deriving Show
creatorApplication value = assert (B.length value <= 4) $ CreatorApplication value

data CreatorHostOs     = CreatorHostOs Word32 deriving Show
creatorHostOs          = CreatorHostOs
creatorHostOsWindows   = creatorHostOs 0x5769326B
creatorHostOsMacintosh = creatorHostOs 0x4D616320

data Size = Size Word64 deriving Show

originalSize = Size
currentSize  = Size

data DiskGeometry = DiskGeometry
	DiskGeometryCylinders
	DiskGeometryHeads
	DiskGeometrySectorsPerTrack
	deriving Show
type DiskGeometryCylinders       = Word16
type DiskGeometryHeads           = Word8
type DiskGeometrySectorsPerTrack = Word8
diskGeometry = DiskGeometry

data DiskType
	= DiskTypeFixed
	| DiskTypeDynamic
	| DiskTypeDifferencing
	deriving Show
diskType :: DiskType -> DiskType
diskType = id

data IsSavedState = IsSavedState Bool deriving Show
isSavedState = IsSavedState

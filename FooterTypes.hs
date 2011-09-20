module FooterTypes where

import Control.Exception
import qualified Data.ByteString.Char8 as B
import Data.Word
import SharedTypes

type IsTemporaryDisk    = Bool
isTemporaryDiskDefault  = False

type FormatVersion   = Version
formatVersionDefault = versionDefault

type DataOffset   = Word64
dataOffsetDefault = 0xFFFFFFFF

newtype CreatorApplication = CreatorApplication B.ByteString deriving Show
creatorApplication value   = assert (B.length value <= 4) $ CreatorApplication value
creatorApplicationDefault  = creatorApplication B.empty

type CreatorVersion = Version
creatorVersionDefault  = versionDefault

type CreatorHostOs     = Word32
creatorHostOsWindows   = 0x5769326B
creatorHostOsMacintosh = 0x4D616320

type OriginalSize   = Word64
originalSizeDefault = 0

type CurrentSize   = Word64
currentSizeDefault = 0

data DiskGeometry = DiskGeometry
	DiskGeometryCylinders
	DiskGeometryHeads
	DiskGeometrySectorsPerTrack
	deriving Show
type DiskGeometryCylinders       = Word16
type DiskGeometryHeads           = Word8
type DiskGeometrySectorsPerTrack = Word8

data DiskType
	= DiskTypeFixed
	| DiskTypeDynamic
	| DiskTypeDifferencing
	deriving Show

type IsSavedState = Bool


module FooterTypes where

import Control.Exception
import qualified Data.ByteString as B
import Data.Word
import SharedTypes

newtype CreatorApplication = CreatorApplication B.ByteString deriving Show

data CreatorHostOs
	= CreatorHostOsUnknown
	| CreatorHostOsWindows
	| CreatorHostOsMacintosh deriving Show

data DiskGeometry = DiskGeometry
	DiskGeometryCylinders
	DiskGeometryHeads
	DiskGeometrySectorsPerTrack deriving Show

type DiskGeometryCylinders       = Word16
type DiskGeometryHeads           = Word8
type DiskGeometrySectorsPerTrack = Word8

data DiskType
	= DiskTypeFixed
	| DiskTypeDynamic
	| DiskTypeDifferencing deriving Show

creatorApplication value = assert (B.length value <= 4) $ CreatorApplication value


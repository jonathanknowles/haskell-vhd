module Data.Vhd.Types where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.List
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Word
import System.Random
import Text.Printf

data Header = Header
	{ headerCookie               :: Cookie
	, headerDataOffset           :: PhysicalByteAddress
	, headerTableOffset          :: PhysicalByteAddress
	, headerVersion              :: Version
	, headerMaxTableEntries      :: VirtualBlockCount
	, headerBlockSize            :: BlockByteCount
	, headerChecksum             :: Checksum
	, headerParentUniqueId       :: UniqueId
	, headerParentTimeStamp      :: TimeStamp
	, headerReserved1            :: B.ByteString
	, headerParentUnicodeName    :: ParentUnicodeName
	, headerParentLocatorEntries :: ParentLocatorEntries
	} deriving (Show, Eq)

data Footer = Footer
	{ footerCookie             :: Cookie
	, footerIsTemporaryDisk    :: Bool
	, footerFormatVersion      :: Version
	, footerDataOffset         :: PhysicalByteAddress
	, footerTimeStamp          :: TimeStamp
	, footerCreatorApplication :: CreatorApplication
	, footerCreatorVersion     :: Version
	, footerCreatorHostOs      :: CreatorHostOs
	, footerOriginalSize       :: VirtualByteCount
	, footerCurrentSize        :: VirtualByteCount
	, footerDiskGeometry       :: DiskGeometry
	, footerDiskType           :: DiskType
	, footerChecksum           :: Checksum
	, footerUniqueId           :: UniqueId
	, footerIsSavedState       :: Bool
	} deriving (Show, Eq)

data BatmapHeader = BatmapHeader
	{ batmapHeaderCookie       :: Cookie
	, batmapHeaderOffset       :: PhysicalByteAddress
	, batmapHeaderSize         :: Word32
	, batmapHeaderVersion      :: Version
	, batmapHeaderChecksum     :: Checksum
	} deriving (Show, Eq)

type BlockByteAddress            = Word32
type BlockByteCount              = Word32
type BlockSectorAddress          = Word32
type BlockSectorCount            = Word32
type DiskGeometryCylinders       = Word16
type DiskGeometryHeads           = Word8
type DiskGeometrySectorsPerTrack = Word8
type Checksum                    = Word32
type PhysicalByteAddress         = Word64
type PhysicalByteCount           = Word64
type PhysicalSectorAddress       = Word32
type PhysicalSectorCount         = Word32
type TimeStamp                   = Word32
type VirtualBlockAddress         = Word32
type VirtualBlockCount           = Word32
type VirtualByteAddress          = Word64
type VirtualByteCount            = Word64
type VirtualSectorAddress        = Word32
type VirtualSectorCount          = Word32

data Version      = Version VersionMajor VersionMinor deriving (Show, Eq)
type VersionMajor = Word16
type VersionMinor = Word16

data CreatorHostOs
	= CreatorHostOsUnknown
	| CreatorHostOsWindows
	| CreatorHostOsMacintosh deriving (Show, Eq)

data DiskGeometry = DiskGeometry
	DiskGeometryCylinders
	DiskGeometryHeads
	DiskGeometrySectorsPerTrack deriving (Show, Eq)

data DiskType
	= DiskTypeFixed
	| DiskTypeDynamic
	| DiskTypeDifferencing deriving (Show, Eq)

newtype Cookie               = Cookie             B.ByteString deriving (Show, Eq)
newtype CreatorApplication   = CreatorApplication B.ByteString deriving (Show, Eq)
newtype ParentLocatorEntry   = ParentLocatorEntry B.ByteString deriving (Show, Eq)
newtype ParentUnicodeName    = ParentUnicodeName  String       deriving (Show, Eq)
newtype UniqueId             = UniqueId           B.ByteString deriving (Eq)

instance Show UniqueId where
	show (UniqueId b) = intercalate "-" $ map disp [[0 .. 3], [4, 5], [6, 7], [8, 9], [10 .. 15]]
		where disp = concatMap (printf "%02x" . B.index b)

newtype ParentLocatorEntries = ParentLocatorEntries [ParentLocatorEntry] deriving (Show, Eq)

cookie               c = assert (B.length c ==   8) $ Cookie               c
creatorApplication   a = assert (B.length a ==   4) $ CreatorApplication   a
parentLocatorEntries e = assert (  length e ==   8) $ ParentLocatorEntries e
parentLocatorEntry   e = assert (B.length e ==  24) $ ParentLocatorEntry   e
uniqueId             i = assert (B.length i ==  16) $ UniqueId             i

parentUnicodeName n
	| encodedLength > 512 = error "parent unicode name length must be <= 512 bytes"
	| otherwise           = ParentUnicodeName n
	where
		encodedLength = B.length $ encodeUtf16BE $ T.pack n

randomUniqueId :: IO UniqueId
randomUniqueId
	= liftM (uniqueId . B.pack)
	$ replicateM 16
	$ liftM fromIntegral
	$ randomRIO (0 :: Int, 255)

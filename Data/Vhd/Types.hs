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
	, headerDataOffset           :: Offset
	, headerTableOffset          :: Offset
	, headerVersion              :: Version
	, headerMaxTableEntries      :: EntryCount
	, headerBlockSize            :: BlockSize
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
	, footerDataOffset         :: Offset
	, footerTimeStamp          :: TimeStamp
	, footerCreatorApplication :: CreatorApplication
	, footerCreatorVersion     :: Version
	, footerCreatorHostOs      :: CreatorHostOs
	, footerOriginalSize       :: Size
	, footerCurrentSize        :: Size
	, footerDiskGeometry       :: DiskGeometry
	, footerDiskType           :: DiskType
	, footerChecksum           :: Checksum
	, footerUniqueId           :: UniqueId
	, footerIsSavedState       :: Bool
	} deriving (Show, Eq)

data BatmapHeader = BatmapHeader
	{ batmapHeaderCookie       :: Cookie
	, batmapHeaderOffset       :: Offset
	, batmapHeaderSize         :: Word32
	, batmapHeaderVersion      :: Version
	, batmapHeaderChecksum     :: Checksum
	} deriving (Show, Eq)

type BlockSize                   = Word32
type DiskGeometryCylinders       = Word16
type DiskGeometryHeads           = Word8
type DiskGeometrySectorsPerTrack = Word8
type Checksum                    = Word32
type EntryCount                  = Word32
type Offset                      = Word64
type Size                        = Word64
type TimeStamp                   = Word32

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

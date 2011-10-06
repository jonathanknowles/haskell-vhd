module Data.VHD.Types where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.Word
import System.Random

data Header = Header
	{ headerCookie               :: Cookie
	, headerDataOffset           :: Offset
	, headerTableOffset          :: Offset
	, headerVersion              :: Version
	, headerMaxTableEntries      :: EntryCount
	, headerBlockSize            :: BlockSize
	, headerCheckSum             :: CheckSum
	, headerParentUniqueId       :: UniqueId
	, headerParentTimeStamp      :: TimeStamp
	, headerParentUnicodeName    :: ParentUnicodeName
	, headerParentLocatorEntries :: ParentLocatorEntries
	} deriving (Show,Eq)

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
	, footerCheckSum           :: CheckSum
	, footerUniqueId           :: UniqueId
	, footerIsSavedState       :: Bool
	} deriving (Show,Eq)

data BatmapHeader = BatmapHeader
	{ batmapHeaderCookie       :: Cookie
	, batmapHeaderOffset       :: Offset
	, batmapHeaderSize         :: Word32
	, batmapHeaderVersion      :: Version
	, batmapHeaderCheckSum     :: CheckSum
	} deriving (Show,Eq)

type BlockSize                   = Word32
type DiskGeometryCylinders       = Word16
type DiskGeometryHeads           = Word8
type DiskGeometrySectorsPerTrack = Word8
type CheckSum                    = Word32
type EntryCount                  = Word32
type Offset                      = Word64
type Size                        = Word64
type TimeStamp                   = Word32

data Version      = Version VersionMajor VersionMinor deriving (Show,Eq)
type VersionMajor = Word16
type VersionMinor = Word16

data CreatorHostOs
	= CreatorHostOsUnknown
	| CreatorHostOsWindows
	| CreatorHostOsMacintosh deriving (Show,Eq)

data DiskGeometry = DiskGeometry
	DiskGeometryCylinders
	DiskGeometryHeads
	DiskGeometrySectorsPerTrack deriving (Show,Eq)

data DiskType
	= DiskTypeFixed
	| DiskTypeDynamic
	| DiskTypeDifferencing deriving (Show,Eq)

newtype Cookie               = Cookie             B.ByteString deriving (Show,Eq)
newtype CreatorApplication   = CreatorApplication B.ByteString deriving (Show,Eq)
newtype ParentLocatorEntry   = ParentLocatorEntry B.ByteString deriving (Show,Eq)
newtype ParentUnicodeName    = ParentUnicodeName  B.ByteString deriving (Show,Eq)
newtype UniqueId             = UniqueId           B.ByteString deriving (Show,Eq)

newtype ParentLocatorEntries = ParentLocatorEntries [ParentLocatorEntry] deriving (Show,Eq)

cookie               c = assert (B.length c ==   8) $ Cookie               c
creatorApplication   a = assert (B.length a ==   4) $ CreatorApplication   a
parentLocatorEntries e = assert (  length e ==   8) $ ParentLocatorEntries e
parentLocatorEntry   e = assert (B.length e ==  24) $ ParentLocatorEntry   e
parentUnicodeName    n = assert (B.length n == 512) $ ParentUnicodeName    n
uniqueId             i = assert (B.length i ==  16) $ UniqueId             i

randomUniqueId :: IO UniqueId
randomUniqueId
	= liftM (uniqueId . B.pack)
	$ replicateM 16
	$ liftM fromIntegral
	$ randomRIO (0 :: Int, 255)


{-# LANGUAGE OverloadedStrings #-}
module Data.VHD where

import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.ByteString.Char8 ()
import Data.Serialize
import Data.VHD.Serialize
import Data.VHD.Types
import Data.VHD.Bat
import Data.VHD.Block
import Data.VHD.Utils
import Data.Bits
import Data.Word

import System.IO

data DynamicDiskInfo = DynamicDiskInfo
	{ footer :: ! Footer
	, header :: ! Header
	} deriving (Show,Eq)

instance Serialize DynamicDiskInfo where
	get = DynamicDiskInfo
		<$> get
		<*> get
	put d = do
		put $ footer d
		put $ header d

create :: FilePath -> BlockSize -> Size -> IO ()
create filePath bs virtualSize =
	withFile filePath WriteMode $ \handle -> do
		B.hPut handle $ encode (DynamicDiskInfo footer header)
		hAlign handle (fromIntegral sectorLength)
		-- create a BAT with every entries initialized at 0xffffffff.
		B.hPut handle $ B.replicate (fromIntegral batSize) 0xff

		hAlign handle (fromIntegral sectorLength)
		B.hPut handle $ encode footer
	where
		maxTableEntries = fromIntegral (virtualSize `divRoundUp` fromIntegral bs)
		batSize         = (maxTableEntries * 4) `roundUpToModulo` sectorLength
		batPadSize      = batSize - maxTableEntries * 4
		footerSize      = 512
		headerSize      = 1024 -- actually 1020

		footer = Footer
			{ footerCookie             = cookie "conectix"
			, footerIsTemporaryDisk    = False
			, footerFormatVersion      = Version 1 0
			, footerDataOffset         = footerSize -- wrong
			, footerTimeStamp          = 0xffffffff -- wrong
			, footerCreatorApplication = creatorApplication "ptap"
			, footerCreatorVersion     = Version 1 0
			, footerCreatorHostOs      = CreatorHostOsWindows
			, footerOriginalSize       = virtualSize
			, footerCurrentSize        = virtualSize
			, footerDiskGeometry       = DiskGeometry 1 1 1 -- c h s wrong
			, footerDiskType           = DiskTypeDynamic
			, footerCheckSum           = 0 -- wrong
			, footerUniqueId           = randomUniqueId
			, footerIsSavedState       = False
			}
		header = Header
			{ headerCookie               = cookie "cxsparse"
			, headerDataOffset           = 0xffffffffffffffff
			, headerTableOffset          = footerSize + headerSize
			, headerVersion              = Version 1 0
			, headerMaxTableEntries      = maxTableEntries
			, headerBlockSize            = bs
			, headerCheckSum             = 0 -- wrong
			, headerParentUniqueId       = randomUniqueId
			, headerParentTimeStamp      = 0 -- wrong
			, headerParentUnicodeName    = parentUnicodeName $ B.replicate 512 0
			, headerParentLocatorEntries = parentLocatorEntries $ replicate 8 (ParentLocatorEntry $ B.replicate 24 0)
			}
		randomUniqueId = uniqueId $ B.replicate 16 0 -- wrong

readDynamicDiskInfoFromFile :: FilePath -> IO (Either String DynamicDiskInfo)
readDynamicDiskInfoFromFile f = return . decodeLazy =<< BL.readFile f

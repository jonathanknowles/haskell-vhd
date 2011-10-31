{-# LANGUAGE OverloadedStrings #-}
module Data.VHD
	( create
	, CreateParameters (..)
	, defaultCreateParameters
	, getInfo
	-- * block related operations
	, Block
	, readDataRange
	, writeDataRange
	, withBlock
	-- * ctx related operations
	, Context (..)
	, withVhdContext
	, appendEmptyBlock
	-- * exported Types
	, module Data.VHD.Types
	) where

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
import Data.VHD.Context
import Data.VHD.Utils
import Data.VHD.Geometry
import Data.VHD.Checksum
import Data.Bits
import Data.Maybe
import Data.Word
import Data.Time.Clock.POSIX

import System.IO

data CreateParameters = CreateParameters
	{ blockSize :: BlockSize
	, size      :: Size
	, timeStamp :: Maybe TimeStamp
	, uuid      :: Maybe UniqueId
	, useBatmap :: Bool
	} deriving (Show, Eq)

defaultCreateParameters = CreateParameters
	{ blockSize = 2 * 1024 * 1024
	, size      = 0
	, timeStamp = Nothing
	, uuid      = Nothing
	, useBatmap = False
	}

-- | grab the header and footer from a vhd file.
getInfo :: FilePath -> IO (Either String (Header, Footer))
getInfo filePath = withFile filePath ReadMode $ \handle -> do
	footer <- decode <$> B.hGet handle 512
	header <- decode <$> B.hGet handle 1024
	case (footer, header) of
		(Left err, _)      -> return $ Left err
		(_, Left err)      -> return $ Left err
		(Right f, Right h) -> return $ Right (h, f)

-- | create an empty vhd with the specified parameters
create :: FilePath -> CreateParameters -> IO ()
create filePath createParams
	| size createParams == 0 = error "cannot create a 0-sized VHD"
	| otherwise              = do
		nowUnixEpoch <- fromIntegral . fromEnum <$> getPOSIXTime
		let nowVhdEpoch = fromIntegral (nowUnixEpoch - y2k)
		uniqueid <- randomUniqueId
		create' filePath $ createParams
			{ uuid      = Just $ maybe uniqueid id $ uuid createParams
			, timeStamp = Just $ maybe nowVhdEpoch id $ timeStamp createParams
			}
	where
		y2k :: Word64
		y2k = 946684800 -- seconds from the unix epoch to the vhd epoch

create' :: FilePath -> CreateParameters -> IO ()
create' filePath createParams =
	withFile filePath WriteMode $ \handle -> do
		B.hPut handle $ encode footer
		B.hPut handle $ encode header
		hAlign handle (fromIntegral sectorLength)
		-- create a BAT with every entry initialized to 0xffffffff.
		B.hPut handle $ B.replicate (fromIntegral batSize) 0xff

		-- maybe create a batmap
		when (useBatmap createParams) $ do
			hAlign handle (fromIntegral sectorLength)
			headerPos <- hTell handle
			B.hPut handle $ encode $ BatmapHeader
				{ batmapHeaderCookie   = cookie "tdbatmap"
				, batmapHeaderOffset   = fromIntegral (headerPos + fromIntegral sectorLength)
				, batmapHeaderSize     = (maxTableEntries `div` 8) `divRoundUp` sectorLength
				, batmapHeaderVersion  = Version 1 2
				, batmapHeaderChecksum = 0xffffffff
				}
			hAlign handle (fromIntegral sectorLength)
			B.hPut handle $ B.replicate (fromIntegral (maxTableEntries `div` 8)) 0x0

		hAlign handle (fromIntegral sectorLength)
		B.hPut handle $ encode footer
	where
		virtualSize     = size createParams
		maxTableEntries = fromIntegral (virtualSize `divRoundUp` fromIntegral (blockSize createParams))
		batSize         = (maxTableEntries * 4) `roundUpToModulo` sectorLength
		batPadSize      = batSize - maxTableEntries * 4
		footerSize      = 512
		headerSize      = 1024

		footer = adjustFooterChecksum $ Footer
			{ footerCookie             = cookie "conectix"
			, footerIsTemporaryDisk    = False
			, footerFormatVersion      = Version 1 0
			, footerDataOffset         = footerSize
			, footerTimeStamp          = fromJust $ timeStamp createParams
			, footerCreatorApplication = creatorApplication "tap\0"
			, footerCreatorVersion     = if useBatmap createParams then Version 1 3 else Version 1 0
			, footerCreatorHostOs      = CreatorHostOsWindows
			, footerOriginalSize       = virtualSize
			, footerCurrentSize        = virtualSize
			, footerDiskGeometry       = diskGeometry (virtualSize `div` fromIntegral sectorLength)
			, footerDiskType           = DiskTypeDynamic
			, footerChecksum           = 0
			, footerUniqueId           = fromJust $ uuid createParams
			, footerIsSavedState       = False
			}
		header = adjustHeaderChecksum $ Header
			{ headerCookie               = cookie "cxsparse"
			, headerDataOffset           = 0xffffffffffffffff
			, headerTableOffset          = footerSize + headerSize
			, headerVersion              = Version 1 0
			, headerMaxTableEntries      = maxTableEntries
			, headerBlockSize            = blockSize createParams
			, headerChecksum             = 0
			, headerParentUniqueId       = uniqueId $ B.replicate 16 0
			, headerParentTimeStamp      = 0
			, headerReserved1            = B.replicate 4 0
			, headerParentUnicodeName    = parentUnicodeName ""
			, headerParentLocatorEntries = parentLocatorEntries $ replicate 8 (ParentLocatorEntry $ B.replicate 24 0)
			}

-- | Reads raw data from a VHD.
read :: Context -> Word64 -> Int -> IO B.ByteString
read context byteOffset length = undefined

-- | Writes raw data to a VHD.
write :: Context -> Word64 -> B.ByteString -> IO ()
write context byteOffset rawData = undefined

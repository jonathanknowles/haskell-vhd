import Control.Applicative
import Data.Attoparsec
import Data.Attoparsec.Binary
import Data.Bits
import qualified Data.ByteString as BS
import Prelude hiding (take)

import qualified Footer as F
import qualified FooterTypes as FT
import qualified SharedTypes as ST

footer :: Parser F.Footer
footer = F.Footer
	<$> cookie
	<*> isTemporaryDisk
	<*> formatVersion
	<*> dataOffset
	<*> timeStamp
	<*> creatorApplication
	<*> creatorVersion
	<*> creatorHostOs
	<*> originalSize
	<*> currentSize
	<*> diskGeometry
	<*> diskType
	<*> checkSum
	<*> uniqueId
	<*> isSavedState
	<*  ignore

cookie             = ST.Cookie <$> take 8
isTemporaryDisk    = (\n -> n .&. 1 == 1) <$> anyWord32be
version            = ST.Version <$> anyWord16be <*> anyWord16be
formatVersion      = version
dataOffset         = anyWord64be
timeStamp          = anyWord32be
creatorApplication = FT.creatorApplication <$> take 4
creatorVersion     = ST.Version <$> anyWord16be <*> anyWord16be
creatorHostOs      = anyWord32be
originalSize       = anyWord64be
currentSize        = anyWord64be
diskGeometry       = FT.DiskGeometry <$> anyWord16be <*> anyWord8 <*> anyWord8
diskType           = diskTypeFromInteger <$> anyWord32be
checkSum           = anyWord32be
uniqueId           = ST.UniqueId <$> take 16
isSavedState       = isSavedStateFromInteger <$> anyWord8
ignore             = take 427

diskTypeFromInteger n = case n of
	2 -> FT.DiskTypeFixed
	3 -> FT.DiskTypeDynamic
	4 -> FT.DiskTypeDifferencing
	_ -> error "invalid disk type"

isSavedStateFromInteger n = case n of
	0 -> False
	1 -> True
	_ -> error "invalid saved state flag"

loadFromFile :: FilePath -> IO (Either String F.Footer)
loadFromFile filePath = parseOnly footer <$> BS.readFile filePath

main = do
	result <- loadFromFile "/tmp/test.vhd"
	case result of
		Left error -> putStrLn error
		Right result ->
			putStrLn $ show result

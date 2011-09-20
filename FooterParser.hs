import Control.Applicative
import Data.Attoparsec
import Data.Attoparsec.Binary
import Data.Bits
import qualified Data.ByteString as BS
import Prelude hiding (take)

import qualified Footer as F
import qualified FooterTypes as FT
import FooterTypes hiding (diskType, isSavedState, isTemporaryDisk)
import SharedTypes

footer :: Parser F.Footer
footer = F.Footer
	<$> (cookie              <$> take 8                               )
	<*> (isTemporaryDisk     <$> anyWord32be                          )
	<*> (formatVersion       <$> anyWord16be <*> anyWord16be          )
	<*> (dataOffset          <$> anyWord64be                          )
	<*> (timeStamp           <$> anyWord32be                          )
	<*> (creatorApplication  <$> take 4                               )
	<*> (creatorVersion      <$> anyWord16be <*> anyWord16be          )
	<*> (creatorHostOs       <$> anyWord32be                          )
	<*> (originalSize        <$> anyWord64be                          )
	<*> (currentSize         <$> anyWord64be                          )
	<*> (diskGeometry        <$> anyWord16be <*> anyWord8 <*> anyWord8)
	<*> (diskType            <$> anyWord32be                          )
	<*> (checkSum            <$> anyWord32be                          )
	<*> (uniqueId            <$> take 16                              )
	<*> (isSavedState        <$> anyWord8                             )
	<*
	paddingBytes

paddingBytes = take 427

isTemporaryDisk = FT.isTemporaryDisk . isTemporaryDiskFromWord
isSavedState    = FT.isSavedState    . isSavedStateFromWord
diskType        = FT.diskType        . diskTypeFromWord

isTemporaryDiskFromWord n = n .&. 1 == 1

diskTypeFromWord n = case n of
	2 -> DiskTypeFixed
	3 -> DiskTypeDynamic
	4 -> DiskTypeDifferencing
	_ -> error "invalid disk type"

isSavedStateFromWord n = case n of
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

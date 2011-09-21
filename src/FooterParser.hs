module FooterParser where

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

isTemporaryDisk = FT.isTemporaryDisk . readWord where
	readWord n = n .&. 1 == 1

isSavedState = FT.isSavedState . readWord where
	readWord 0 = False
	readWord 1 = True
	readWord _ = error "invalid saved state flag"

diskType = FT.diskType . readWord where
	readWord 2 = DiskTypeFixed
	readWord 3 = DiskTypeDynamic
	readWord 4 = DiskTypeDifferencing
	readWord _ = error "invalid disk type"

loadFromFile :: FilePath -> IO (Either String F.Footer)
loadFromFile filePath = parseOnly footer <$> BS.readFile filePath

main = do
	result <- loadFromFile "/tmp/test.vhd"
	case result of
		Left error -> putStrLn error
		Right result ->
			putStrLn $ show result

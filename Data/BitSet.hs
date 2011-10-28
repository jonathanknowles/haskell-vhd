module Data.BitSet
	( BitSet
	, generateFromRange
	, intersection
	, union
	, toList
	, fromByteString
	)
	where

import Control.Exception
import Data.Bits
import qualified Data.ByteString as B
import Data.Word

data BitSet = BitSet B.ByteString

fromByteString :: B.ByteString -> BitSet
fromByteString = BitSet

empty :: BitSet
empty = BitSet B.empty

fillByte :: Word8 -> Int -> Int -> Word8
fillByte byte loBit hiBit =
	if loBit < hiBit
		then fillByte (setBit byte loBit) (loBit + 1) hiBit
		else byte

generateFromRange :: Int -> Int -> BitSet
generateFromRange lo hi

	| lo <  0  = error "lower bound cannot be less than zero."
	| lo >  hi = error "lower bound cannot be greater than upper bound."
	| lo == hi = BitSet B.empty

	| lo     == 0 && hiBit  == 0 = BitSet setBits
	| loBit  == 0 && hiBit  == 0 = BitSet $ B.concat [clearBits, setBits]
	| loBit  == 0 && hiBit  /= 0 = BitSet $ B.concat [clearBits, setBits, fallByte]
	| loBit  /= 0 && hiBit  == 0 = BitSet $ B.concat [clearBits, riseByte, setBits]

	| loByteFloor   == hiByteFloor = BitSet $ B.concat [clearBits, humpByte]
	| loByteCeiling == hiByteFloor = BitSet $ B.concat [clearBits, riseByte, fallByte]
	| loByteCeiling  < hiByteFloor = BitSet $ B.concat [clearBits, riseByte, setBits, fallByte]

	where

		(loBit, loByteFloor, loByteCeiling) = (lo `mod` 8, lo `div` 8, (lo + 7) `div` 8)
		(hiBit, hiByteFloor, hiByteCeiling) = (hi `mod` 8, hi `div` 8, (hi + 7) `div` 8)

		clearBits = B.replicate (loByteFloor                ) 0x00
		setBits   = B.replicate (hiByteFloor - loByteCeiling) 0xff

		riseByte = B.singleton $ fillByte 0 loBit     8
		fallByte = B.singleton $ fillByte 0     0 hiBit
		humpByte = B.singleton $ fillByte 0 loBit hiBit

byteStringPackZipWith :: (Word8 -> Word8 -> Word8) -> B.ByteString -> B.ByteString -> B.ByteString
byteStringPackZipWith = ((B.pack .) .) . B.zipWith

byteStringsPad :: B.ByteString -> B.ByteString -> (B.ByteString, B.ByteString)
byteStringsPad b1 b2 =
	if length1 < length2
		then (B.append b1 (B.replicate (length2 - length1) 0), b2)
		else (b1, B.append b2 (B.replicate (length1 - length2) 0))
	where
		length1 = B.length b1
		length2 = B.length b2

intersection = binaryOp (.&.)
union        = binaryOp (.|.)

binaryOp f (BitSet b1) (BitSet b2) =
	BitSet $ byteStringPackZipWith f b1' b2'
	where (b1', b2') = byteStringsPad b1 b2

word8Bits :: Word8 -> [Bool]
word8Bits w = map (testBit w) [0 .. 7]

byteStringBits byteString = do
	word <- B.unpack byteString
	bit <- word8Bits word
	return bit

toList :: BitSet -> [Int]
toList (BitSet b) = map snd $ filter fst $ zip (byteStringBits b) [0 ..]

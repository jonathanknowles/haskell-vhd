module Data.BitSet
	( BitSet
	, empty
	, fromByteString
	, fromRange
	, toList
	, intersection
	, union
	, subtract
	)
	where

import Control.Exception
import Data.Bits
import qualified Data.ByteString as B
import Data.Word
import Prelude hiding (subtract)

data BitSet = BitSet B.ByteString

instance Show BitSet where show = show . toList

empty :: BitSet
empty = BitSet B.empty

fromByteString :: B.ByteString -> BitSet
fromByteString = BitSet

fromRange :: Int -> Int -> BitSet
fromRange lo hi = BitSet generate where

	generate
		| lo <  0  = error "lower bound cannot be less than zero."
		| lo >  hi = error "lower bound cannot be greater than upper bound."
		| lo == hi = B.empty
		| lo    == 0 && hiBit == 0 = setBits
		| loBit == 0 && hiBit == 0 = B.concat [clearBits, setBits]
		| loBit == 0 && hiBit /= 0 = B.concat [clearBits, setBits, fallByte]
		| loBit /= 0 && hiBit == 0 = B.concat [clearBits, riseByte, setBits]
		| loByteFloor   == hiByteFloor = B.concat [clearBits, humpByte]
		| loByteCeiling == hiByteFloor = B.concat [clearBits, riseByte, fallByte]
		| loByteCeiling <  hiByteFloor = B.concat [clearBits, riseByte, setBits, fallByte]

	(loBit, loByteFloor, loByteCeiling) = (lo `mod` 8, lo `div` 8, (lo + 7) `div` 8)
	(hiBit, hiByteFloor, hiByteCeiling) = (hi `mod` 8, hi `div` 8, (hi + 7) `div` 8)

	clearBits = B.replicate (loByteFloor                ) 0x00
	setBits   = B.replicate (hiByteFloor - loByteCeiling) 0xff

	riseByte = B.singleton $ fillByte 0 loBit     8
	fallByte = B.singleton $ fillByte 0     0 hiBit
	humpByte = B.singleton $ fillByte 0 loBit hiBit

toList :: BitSet -> [Int]
toList (BitSet b) = map snd $ filter fst $ zip (byteStringBits b) [0 ..]

intersection :: BitSet -> BitSet -> BitSet
union        :: BitSet -> BitSet -> BitSet
subtract     :: BitSet -> BitSet -> BitSet

intersection = binaryOp (.&.)
union        = binaryOp (.|.)
subtract     = binaryOp (\x y -> x .&. complement y)

binaryOp f (BitSet b1) (BitSet b2) =
	BitSet $ byteStringPackZipWith f b1' b2'
	where (b1', b2') = byteStringsPad b1 b2

byteStringBits byteString = do
	word <- B.unpack byteString
	bit <- word8Bits word
	return bit

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

fillByte :: Word8 -> Int -> Int -> Word8
fillByte byte loBit hiBit = if loBit < hiBit
	then fillByte (setBit byte loBit) (loBit + 1) hiBit
	else byte

word8Bits :: Word8 -> [Bool]
word8Bits w = map (testBit w) [0 .. 7]


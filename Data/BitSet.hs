module Data.BitSet
	( BitSet
	, empty
	, fromByteString
	, fromRange
	, toList
	, isEmpty
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

instance Eq BitSet
	where (BitSet b1) == (BitSet b2) = b1' == b2'
		where (b1', b2') = byteStringsPad b1 b2

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
		| lo    == 0 && hiBit == 0 = setBytes
		| loBit == 0 && hiBit == 0 = B.concat [clearBytes, setBytes]
		| loBit == 0 && hiBit /= 0 = B.concat [clearBytes, setBytes, fallByte]
		| loBit /= 0 && hiBit == 0 = B.concat [clearBytes, riseByte, setBytes]
		| loByteFloor   == hiByteFloor = B.concat [clearBytes, humpByte]
		| loByteCeiling == hiByteFloor = B.concat [clearBytes, riseByte, fallByte]
		| loByteCeiling <  hiByteFloor = B.concat [clearBytes, riseByte, setBytes, fallByte]

	(loBit, loByteFloor, loByteCeiling) = (lo `mod` 8, lo `div` 8, (lo + 7) `div` 8)
	(hiBit, hiByteFloor, hiByteCeiling) = (hi `mod` 8, hi `div` 8, (hi + 7) `div` 8)

	clearBytes = B.replicate (loByteFloor                ) 0x00
	setBytes   = B.replicate (hiByteFloor - loByteCeiling) 0xff

	riseByte = B.singleton $ setBits 0 loBit     8
	fallByte = B.singleton $ setBits 0     0 hiBit
	humpByte = B.singleton $ setBits 0 loBit hiBit

toList :: BitSet -> [Int]
toList (BitSet b) = map snd $ filter fst $ zip (byteStringBits b) [0 ..]

isEmpty :: BitSet -> Bool
isEmpty (BitSet b) = B.all (== 0) b

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

setBits :: Bits a => a -> Int -> Int -> a
setBits value loBit hiBit = if loBit < hiBit
	then setBits (setBit value loBit) (loBit + 1) hiBit
	else value

word8Bits :: Word8 -> [Bool]
word8Bits w = map (testBit w) [0 .. 7]


module Data.BitSet where

import Data.Bits
import qualified Data.ByteString as B
import Data.Word

data BitSet = BitSet B.ByteString

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

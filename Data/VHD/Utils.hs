module Data.VHD.Utils
	( divRoundUp
	, roundUpToModulo
	, hAlign
	) where

import Control.Monad (unless)
import System.IO
import qualified Data.ByteString as B

divRoundUp a b = let (d, m) = a `divMod` b in d + if m > 0 then 1 else 0

roundUpToModulo n m
	| n `mod` m == 0 = n
	| otherwise      = n + m - (n `mod` m)

-- | align an handle to the next modulo
hAlign :: Handle -> Int -> IO ()
hAlign h n = hTell h >>= \i -> unless ((i `mod` fromIntegral n) == 0) (realign i)
	where realign i = B.hPut h $ B.replicate (n - fromIntegral (i `mod` fromIntegral n)) 0

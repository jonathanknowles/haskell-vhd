module Data.Vhd.Utils
	( divRoundUp
	, resolveColocatedFilePath
	, roundUpToModulo
	, hAlign
	, unlessM
	) where

import Control.Monad (unless)
import qualified Data.ByteString as B
import System.FilePath.Posix
import System.IO

divRoundUp a b = let (d, m) = a `divMod` b in d + if m > 0 then 1 else 0

resolveColocatedFilePath :: FilePath -> FilePath -> FilePath
resolveColocatedFilePath baseFilePath colocatedFilePath =
	if isAbsolute colocatedFilePath
		then colocatedFilePath
		else takeDirectory baseFilePath </> colocatedFilePath

roundUpToModulo n m
	| n `mod` m == 0 = n
	| otherwise      = n + m - (n `mod` m)

-- | align an handle to the next modulo
hAlign :: Handle -> Int -> IO ()
hAlign h n = hTell h >>= \i -> unless ((i `mod` fromIntegral n) == 0) (realign i)
	where realign i = B.hPut h $ B.replicate (n - fromIntegral (i `mod` fromIntegral n)) 0

unlessM condition elseBranch = do
	c <- condition
	unless c elseBranch


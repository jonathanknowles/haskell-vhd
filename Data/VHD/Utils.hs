module Data.VHD.Utils
	( divRoundUp
	, roundUpToModulo
	) where

divRoundUp a b = let (d,m) = a `divMod` b in d + if m > 0 then 1 else 0

roundUpToModulo n m
	| n `mod` m == 0 = n
	| otherwise      = n + m - (n `mod` m)

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Data.Range
	( Range
	, RangeLength
	, range
	, length
	, minimum
	, maximum
	) where

import Control.Exception
import Data.Word
import Prelude hiding (length, minimum, maximum)

data Range a = Range a a

class (Integral a, Integral b) => RangeLength a b | a -> b

range :: RangeLength a b => a -> a -> Range a
range a b = assert (a <= b) $ Range a b

length :: RangeLength a b => Range a -> b
length (Range a b) = fromIntegral $ b - a

minimum :: Range a -> a
minimum (Range a _) = a

maximum :: Range a -> a
maximum (Range _ a) = a

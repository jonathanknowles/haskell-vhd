{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Data.Range
	( Range
	, RangeLength
	, range
	, rangeLength
	) where

import Control.Exception
import Data.Word

data Range a = Range a a

class (Integral a, Integral b) => RangeLength a b | a -> b

range :: RangeLength a b => a -> a -> Range a
range a b = assert (a <= b) $ Range a b

rangeLength :: RangeLength a b => Range a -> b
rangeLength (Range a b) = fromIntegral $ b - a

rangeMinimum :: Range a -> a
rangeMinimum (Range a _) = a

rangeMaximum :: Range a -> a
rangeMaximum (Range _ a) = a

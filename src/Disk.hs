module Disk where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Serialize
import qualified Data.ByteString.Lazy as BL

import Header
import Footer

data DynamicDisk = DynamicDisk
	{ footer :: ! Footer
	, header :: ! Header
	} deriving Show

instance Serialize DynamicDisk where
	get = DynamicDisk
		<$> get
		<*> get
	put d = do
		put $ footer d
		put $ header d

readFromFile :: FilePath -> IO (Either String DynamicDisk)
readFromFile f = return . decodeLazy =<< BL.readFile f


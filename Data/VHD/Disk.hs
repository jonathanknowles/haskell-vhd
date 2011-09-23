module Data.VHD.Disk where

import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import Data.Serialize
import Data.VHD.Header
import Data.VHD.Footer

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


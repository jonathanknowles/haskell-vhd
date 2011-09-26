module Data.VHD where

import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import Data.Serialize
import Data.VHD.Serialize
import Data.VHD.Types

data DynamicDiskInfo = DynamicDiskInfo
	{ footer :: ! Footer
	, header :: ! Header
	} deriving (Show,Eq)

instance Serialize DynamicDiskInfo where
	get = DynamicDiskInfo
		<$> get
		<*> get
	put d = do
		put $ footer d
		put $ header d

readDynamicDiskInfoFromFile :: FilePath -> IO (Either String DynamicDiskInfo)
readDynamicDiskInfoFromFile f = return . decodeLazy =<< BL.readFile f


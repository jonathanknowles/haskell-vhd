module Data.VHD.HeaderTypes where

import Control.Exception
import qualified Data.ByteString.Char8 as B
import Data.VHD.SharedTypes
import Data.Word

type BlockSize  = Word32
type EntryCount = Word32

newtype ParentLocatorEntry  = ParentLocatorEntry B.ByteString deriving Show
newtype ParentUnicodeName   = ParentUnicodeName  B.ByteString deriving Show

data ParentLocatorEntries = ParentLocatorEntries [ParentLocatorEntry] deriving Show

parentLocatorEntries e = assert (  length e ==   8) $ ParentLocatorEntries e
parentLocatorEntry   e = assert (B.length e ==  24) $ ParentLocatorEntry   e
parentUnicodeName    n = assert (B.length n <= 512) $ ParentUnicodeName    n

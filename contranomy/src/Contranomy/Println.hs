module Contranomy.Println ( hookPrint ) where

import Clash.Prelude

import qualified Data.ByteString as BS
import Data.Foldable (traverse_)
import Data.Maybe (catMaybes)

hookPrint
  :: Unsigned 32 -- ^ Address
  -> [Maybe (Unsigned 32, Signed 32)] -- ^ Data
  -> IO ()
hookPrint addr =
    pChars
  . fmap snd
  . filter (\(addr', _) -> addr' == addr)
  . catMaybes

pChars :: Foldable t => t (Signed 32) -> IO ()
pChars = traverse_ pChar

pChar :: Signed 32 -> IO ()
pChar = BS.putStr . BS.pack . (:[]) . addrByte

-- take one byte
addrByte :: (BitPack a, BitSize a ~ 8) => Signed 32 -> a
addrByte = bitCoerce . slice d7 d0

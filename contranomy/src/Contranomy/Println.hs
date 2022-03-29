module Contranomy.Println ( hookPrint ) where

import Clash.Prelude

import qualified Data.ByteString as BS
import Data.Foldable (traverse_)
import Data.Maybe (catMaybes)
import Data.Word (Word8)

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
addrByte :: Signed 32 -> Word8
addrByte = bitCoerce . slice d7 d0

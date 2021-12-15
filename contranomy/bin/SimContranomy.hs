{-# LANGUAGE ViewPatterns #-}

import Clash.Prelude

import Contranomy
import qualified Data.ByteString as BS
import Data.Elf
import qualified Data.IntMap.Strict as I
import qualified Data.List as L
import System.Environment (getArgs)

type BinaryData = I.IntMap (BitVector 8)
type Address = BitVector 32

-- | readElf :: elf file -> (initial PC, instructions, data)
--
-- TODO Check the ELF header is valid: is this RISCV? Is it RV32IMC?
-- TODO Binaries output now are SYS V ABI, are others compatible?
readElf :: Elf -> (Address, BinaryData, BinaryData)
readElf elf =
  let (iMem, dMem) = L.foldr go (mempty, mempty) (elfSections elf)
   in (fromIntegral (elfEntry elf), iMem, dMem)
 where
  go sec acc@(is, ds)
    -- Address is 0: Not mapped to virtual memory
    | elfSectionAddr sec == 0
    = acc

    -- Section contains instruction memory
    | SHF_EXECINSTR `elem` elfSectionFlags sec
    , SHF_WRITE `notElem` elfSectionFlags sec
    = (addData (elfSectionAddr sec) (elfSectionData sec) is, ds)

    -- Section contains data memory
    | SHF_WRITE `elem` elfSectionFlags sec
        || SHF_ALLOC `elem` elfSectionFlags sec
    , SHF_EXECINSTR `notElem` elfSectionFlags sec
    = (is, addData (elfSectionAddr sec) (elfSectionData sec) ds)

    | otherwise
    = error ("Section is not executable XOR data:\n" <> show sec)

  addData (fromIntegral -> startAddr) str mem =
    let bytes = pack <$> BS.unpack str
     in I.fromList (L.zip [startAddr..] bytes) <> mem

main :: IO ()
main = do
  args <- getArgs

  case args of
    ["-h"]     -> showHelp
    ["--help"] -> showHelp
    [elfFile]  -> simulateFile elfFile
    _          -> showHelp

showHelp :: IO ()
showHelp = putStrLn $ unlines
  [ "simcontranomy - Simulate an ELF file in contranomy"
  , ""
  , "USAGE"
  , "  simcontranomy FILE"
  ]

simulateFile :: FilePath -> IO ()
simulateFile file = do
  elfBytes <- BS.readFile file
  let elf = parseElf elfBytes
  let (entry, iMem, dMem) = readElf elf

  print
    . sampleN 10000 -- TODO Perhaps this should be some sampleUntil ?
    . contranomy' hasClock hasReset entry iMem dMem
    $ pure (False, False, 0b0)

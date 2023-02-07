module Bittide.Instances.ProcessingElement where

import Clash.Prelude
import Bittide.ProcessingElement

import Bittide.ProcessingElement.Util
import Contranomy.Core
import Bittide.DoubleBufferedRam
import Paths_bittide_instances
import Language.Haskell.TH

-- TODO: Replace with VexRiscV
-- | For this example, make sure contranomy does not try to instantiate operators for the M extension.
contranomy :: Clock System -> Reset System -> Signal System (BitVector 8)
contranomy clk rst = out
 where
  (Nil, sinkOut :: Signal System (Maybe (BitVector 4, BitVector 32))) =
    withClockResetEnable clk rst enableGen $ processingElement peConfig Nil

  ( pc
   , (iStart, iSize, iMem)
   , (dStart, dSize, dMem)) = $(do
      elfPath <- runIO $ getDataFileName "../target/riscv32imc-unknown-none-elf/release/hello"
      fdtPath <- runIO $ getDataFileName "/device-trees/hello.dts"
      memBlobsFromElf elfPath fdtPath (0x40002EE8 :: Int))

  peConfig = PeConfig (0 :> 1 :> 2 :> Nil) (Reloadable $ Blob iMem) (Reloadable $ Blob dMem) pc
  out = withClockResetEnable clk rst enableGen $ resize . snd <$> regMaybe (unpack 0) sinkOut

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
contranomy :: Clock System -> Reset System -> Signal System (Unsigned 8)
contranomy clk rst = out
 where
  (wbM2S :> Nil) =
    withClockResetEnable clk rst enableGen $ processingElement peConfig (wbS2M :> Nil)

  ( pc
   , (iStart, iSize, iMem)
   , (dStart, dSize, dMem)) = $(do
      elfPath <- runIO $ getDataFileName "/demo-files/binaries/hello"
      fdtPath <- runIO $ getDataFileName "/device-trees/hello.dts"
      memBlobsFromElf elfPath (Just fdtPath) (0x40003EE8 :: Int))

  peConfig = PeConfig (0 :> 1 :> 2 :> Nil) (Reloadable $ Blob iMem) (Reloadable $ Blob dMem) pc
  (out, wbS2M) = withClockResetEnable clk rst enableGen $ registerWb WishbonePriority 0 wbM2S (pure Nothing)

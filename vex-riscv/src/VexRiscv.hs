-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module VexRiscv where

import Clash.Prelude
import Clash.Signal.Internal
import Foreign.Marshal (alloca)
import Foreign.Storable
import GHC.IO (unsafePerformIO)
import GHC.Stack (HasCallStack)
import Protocols.Wishbone
import VexRiscv.FFI

data Input = Input
  { timerInterrupt :: Bit,
    externalInterrupt :: Bit,
    softwareInterrupt :: Bit,
    iBusWbS2M :: WishboneS2M (BitVector 32),
    dBusWbS2M :: WishboneS2M (BitVector 32)
  }
  deriving (Generic, NFDataX, ShowX, Eq)

data Output = Output
  { iBusWbM2S    :: WishboneM2S 32 4 (BitVector 32),
    dBusWbM2S    :: WishboneM2S 32 4 (BitVector 32)
  }
  deriving (Generic, NFDataX, ShowX, Eq)

inputToFFI :: Bool -> Input -> INPUT
inputToFFI reset Input {..} =
  INPUT
    { reset = boolToBit reset,
      timerInterrupt,
      externalInterrupt,
      softwareInterrupt,
      iBusWishbone_ACK = boolToBit $ acknowledge iBusWbS2M,
      iBusWishbone_DAT_MISO = bitCoerce $ readData iBusWbS2M,
      iBusWishbone_ERR = boolToBit $ err iBusWbS2M,
      dBusWishbone_ACK = boolToBit $ acknowledge dBusWbS2M,
      dBusWishbone_DAT_MISO = bitCoerce $ readData dBusWbS2M,
      dBusWishbone_ERR = boolToBit $ err dBusWbS2M
    }

outputFromFFI :: OUTPUT -> Output
outputFromFFI OUTPUT {..} =
  Output
    { iBusWbM2S =
        (emptyWishboneM2S @32 @(BitVector 32))
          { busCycle = bitToBool iBusWishbone_CYC,
            strobe = bitToBool iBusWishbone_STB,
            writeEnable = bitToBool iBusWishbone_WE,
            addr = bitCoerce iBusWishbone_ADR,
            writeData = bitCoerce iBusWishbone_DAT_MOSI,
            busSelect = resize $ bitCoerce iBusWishbone_SEL,
            cycleTypeIdentifier = bitCoerce $ resize $ bitCoerce @_ @(BitVector 8) iBusWishbone_CTI,
            burstTypeExtension = bitCoerce $ resize $ bitCoerce @_ @(BitVector 8) iBusWishbone_BTE
          },
      dBusWbM2S =
        (emptyWishboneM2S @32 @(BitVector 32))
          { busCycle = bitToBool dBusWishbone_CYC,
            strobe = bitToBool dBusWishbone_STB,
            writeEnable = bitToBool dBusWishbone_WE,
            addr = bitCoerce dBusWishbone_ADR,
            writeData = bitCoerce dBusWishbone_DAT_MOSI,
            busSelect = resize $ bitCoerce dBusWishbone_SEL,
            cycleTypeIdentifier = bitCoerce $ resize $ bitCoerce @_ @(BitVector 8) dBusWishbone_CTI,
            burstTypeExtension = bitCoerce $ resize $ bitCoerce @_ @(BitVector 8) dBusWishbone_BTE
          }
    }

makeDefined :: WishboneS2M (BitVector 32) -> WishboneS2M (BitVector 32)
makeDefined wb = wb { readData = readData' }
  where
    readData' =
      if hasUndefined (readData wb) then
        0
      else
        readData wb

{-# NOINLINE vexRiscv #-}
vexRiscv :: (HasCallStack, HiddenReset dom) => Signal dom Input -> Signal dom Output
vexRiscv = unsafePerformIO $ do
  (stepFirst, stepSecond, _) <- vexCPU
  let
    {-# NOINLINE stepFirst' #-}
    stepFirst' reset input = unsafePerformIO $ do
      -- putStrLn "step first"
      stepFirst reset input
    {-# NOINLINE stepSecond' #-}
    stepSecond' reset input = unsafePerformIO $ do
      -- putStrLn "step second"
      stepSecond reset input
  pure $ go stepFirst' stepSecond' (unsafeFromReset hasReset)
  where
    {-# NOINLINE go #-}
    go stepFirst stepSecond (rst :- rsts) ~(input :- inputs) =
      let !output = stepFirst rst (firstInput input)
      in
          output :-
            ( stepSecond rst (secondInput input)
            `seq` go stepFirst stepSecond rsts inputs )

    firstInput i =
      i { iBusWbS2M = makeDefined emptyWishboneS2M
        , dBusWbS2M = makeDefined emptyWishboneS2M }
    secondInput i@Input{..} =
      i { iBusWbS2M = makeDefined iBusWbS2M
        , dBusWbS2M = makeDefined dBusWbS2M }

type StepFn = Bool -> Input -> IO Output

-- | Return a function that performs an execution step and a function to free
-- the internal CPU state
vexCPU :: IO (StepFn, StepFn, IO ())
vexCPU = do
  v <- vexrInit
  let
    stepFirst reset input = alloca $ \inputFFI -> alloca $ \outputFFI -> do
      poke inputFFI (inputToFFI reset input)
      vexrStepFirst v inputFFI outputFFI
      outVal <- peek outputFFI
      pure $ outputFromFFI outVal

    stepSecond reset input = alloca $ \inputFFI -> alloca $ \outputFFI -> do
      poke inputFFI (inputToFFI reset input)
      vexrStepSecond v inputFFI outputFFI
      outVal <- peek outputFFI
      pure $ outputFromFFI outVal

    shutDown = vexrShutdown v
  pure (stepFirst, stepSecond, shutDown)

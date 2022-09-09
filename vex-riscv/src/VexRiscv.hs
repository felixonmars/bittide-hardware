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
  { iBusWbM2S :: WishboneM2S 32 4 (BitVector 32),
    dBusWbM2S :: WishboneM2S 32 4 (BitVector 32)
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

{-
vexRiscv :: (HasCallStack, HiddenReset dom) => Maybe Integer -> Signal dom Input -> Signal dom Output
vexRiscv simSteps inSignal = unsafePerformIO $ do
  v <- vexrInit
  alloca $ \input -> alloca $ \output ->
    go simSteps v input output (unsafeFromReset hasReset) inSignal
  where
    go (Just 0) v _ _ _ _ = do
      vexrShutdown v
      pure . pure $
        Output
          { iBusWbM2S = emptyWishboneM2S,
            dBusWbM2S = emptyWishboneM2S
          }
    go Nothing v input output (rst :- resets) (inVal :- inputs) = do
      poke input (inputToFFI rst inVal)
      vexrStep v input output
      outVal <- peek output
      (outputFromFFI outVal :-) <$> go Nothing v input output resets inputs
    go (Just n) v input output (rst :- resets) (inVal :- inputs) = do
      poke input (inputToFFI rst inVal)
      vexrStep v input output
      outVal <- peek output
      (outputFromFFI outVal :-) <$> go (Just $ n - 1) v input output resets inputs
-}


{-# NOINLINE vexRiscv #-}
vexRiscv :: (HasCallStack, HiddenReset dom) => Signal dom Input -> Signal dom Output
vexRiscv = unsafePerformIO $ do
  (step, _) <- vexCPU
  pure $ go step (unsafeFromReset hasReset)
  where
    {-# NOINLINE go #-}
    go step (rst :- rsts) (input :- inputs) = unsafePerformIO $ do
      out <- step rst input
      pure $ out :- go step rsts inputs


-- | Return a function that performs an execution step and a function to free
-- the internal CPU state
vexCPU :: IO (Bool -> Input -> IO Output, IO ())
vexCPU = do
  v <- vexrInit
  alloca $ \inputFFI -> alloca $ \outputFFI ->
    let
      step reset input = do
        poke inputFFI (inputToFFI reset input)
        vexrStep v inputFFI outputFFI
        outVal <- peek outputFFI
        pure $ outputFromFFI outVal
      shutDown = vexrShutdown v
    in pure (step, shutDown)



{-

runVexRiscv :: Maybe Integer -> (Output -> IO Input) -> IO ()
runVexRiscv simSteps stepFn = do
  v <- vexrInit

  alloca $ \input -> alloca $ \output ->
    go simSteps v input output

  vexrShutdown v
  where
    go Nothing v inp out = do
      outVal <- peek out
      inVal <- stepFn (outputFromFFI outVal)
      poke inp (inputToFFI False inVal)
      vexrStep v inp out
      go Nothing v inp out
    go (Just 0) _ _ _ = pure ()
    go (Just n) v inp out = do
      outVal <- peek out
      inVal <- stepFn (outputFromFFI outVal)
      poke inp (inputToFFI False inVal)
      vexrStep v inp out
      go (Just $ n - 1) v inp out

-}

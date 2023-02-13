module Bittide.Extra.DeviceId where

import Clash.Explicit.Prelude
import Clash.Prelude (HiddenClockResetEnable, hasClock, hasReset, hasEnable)

import Clash.Annotations.Primitive
import Clash.Signal (withClockResetEnable)
import Data.String.Interpolate      (i)
import Data.String.Interpolate.Util (unindent)

data State
  = LOAD
  | SHIFT
  | DONE
  deriving (Generic, NFDataX)

type DIN = BitVector 1
type DOUT = BitVector 1
type READ = Bool
type SHIFT = Bool

deviceId :: HiddenClockResetEnable dom => Signal dom (Maybe (BitVector 96))
deviceId = mux done (Just <$> dna) (pure Nothing)
 where
  (read, shift, dna, done) = unbundle $ dnaControl hasClock hasReset hasEnable dout
  dout = dnaPorte hasClock dout read shift

-- | State machine to extract the DNA from Kintex Ultrascale FPGAs.
-- To be connected to the Device DNA Access Port @DNA_PORTE2@
-- Instantiation template:
--  DNA_PORTE2 #(
--     .SIM_DNA_VALUE(96'h000000000000000000000000)  // Specifies a sample 96-bit DNA value for simulation.
--  )
--  DNA_PORTE2_inst (
--     .DOUT(DOUT),   // 1-bit output: DNA output data.
--     .CLK(CLK),     // 1-bit input: Clock input.
--     .DIN(DIN),     // 1-bit input: User data input pin.
--     .READ(READ),   // 1-bit input: Active-High load DNA, active-Low read input.
--     .SHIFT(SHIFT)  // 1-bit input: Active-High shift enable input.
--  );
dnaControl ::
  KnownDomain dom =>
  "clk" ::: Clock dom ->
  "rst" ::: Reset dom ->
  "ena" ::: Enable dom ->
  "dout" ::: Signal dom DOUT ->
  "" ::: Signal dom
  ( "read" ::: READ
  , "shift" ::: SHIFT
  , "dna" ::: BitVector 96
  , "done" ::: Bool)

dnaControl clk rst ena = mealy clk rst ena go (LOAD, 0)
 where
  go :: (State, BitVector 96) -> DOUT -> ((State, BitVector 96), (READ, SHIFT, BitVector 96, Bool))
  go (state,reg) input = (newState, (r, s, reg, dnaDetected))
   where
    (newState, r, s) = case state of
      LOAD  -> ((SHIFT, newShift), True, False)
      SHIFT
        | dnaDetected -> ((DONE, reg), False, False)
        | otherwise   -> ((SHIFT, newShift), False, True)
      DONE  -> ((DONE, reg), False, False)
    newShift = input ++# slice d95 d1 reg
    dnaDetected = slice d95 d94 reg == 1 && slice d1 d0 reg == 1

{-# ANN dnaPorte (InlineYamlPrimitive [Verilog] $ unindent [i|
  BlackBox:
    name: Bittide.Extra.DeviceId.dnaPorte
    kind: Declaration
    outputUsage: NonBlocking
    type: |-
      dnaPorte ::
        KnownDomain dom =>    -- ARG[0]
        Clock dom ->          -- ARG[1]
        Signal dom DIN ->     -- ARG[2]
        Signal dom READ ->    -- ARG[3]
        Signal dom SHIFT ->   -- ARG[4]
        Signal dom DOUT       -- RESULT
    template: |-
      // Start DNA-PORTE2 instantiation
      DNA_PORTE2 #(
          .SIM_DNA_VALUE(96'h000000000000000000000000)  // Specifies a sample 96-bit DNA value for simulation.
      )
      DNA_PORTE2_inst (
          .DOUT(~RESULT),   // 1-bit output: DNA output data.
          .CLK(~ARG[1]),     // 1-bit input: Clock input.
          .DIN(~ARG[2]),     // 1-bit input: User data input pin.
          .READ(~ARG[3]),   // 1-bit input: Active-High load DNA, active-Low read input.
          .SHIFT(~ARG[4])  // 1-bit input: Active-High shift enable input.
      );
      // Stop DNA-PORTE2 instantiation
|]) #-}

-- | A component that can be found on Xilinx Ultrascale FPGAs that can be used
-- to extract a unique device identifier. It offers an API that can be used to
-- load the identifier into a shift register and control signals to control this
-- shift register.
dnaPorte ::
  KnownDomain dom =>
  -- | Incoming clock signal.
  Clock dom ->
  -- | Shift register input pin.
  Signal dom DIN ->
  -- | Active high load DNA, active low read input.
  Signal dom READ ->
  -- | Active high shift enable.
  Signal dom SHIFT ->
  -- | DNA output pin.
  Signal dom DOUT
dnaPorte clk din read shift = resize <$> regOut
 where
  regOut = register clk resetGen enableGen 0 $
    mux read
    (pure 0x5555555555555555)
    (mux shift
      ((\a b -> slice d96 d1 $ pack (a, b)) <$> din <*> regOut)
      regOut
    )
{-# NOINLINE dnaPorte #-}
{-# ANN dnaPorte hasBlackBox #-}

topEntity :: Clock System -> Reset System -> Signal System (Maybe (BitVector 96))
topEntity  clk rst = withClockResetEnable clk rst enableGen $ deviceId @System

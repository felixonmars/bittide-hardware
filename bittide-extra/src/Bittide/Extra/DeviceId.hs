module Bittide.Extra.DeviceId where

import Clash.Explicit.Prelude
import Clash.Annotations.TH (makeTopEntity)

data State
  = LOAD
  | SHIFT
  | DONE
  deriving (Generic, NFDataX)

type DOUT = BitVector 1
type READ = BitVector 1
type SHIFT = BitVector 1

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
  "clk" ::: Clock System ->
  "rst" ::: Reset System ->
  "ena" ::: Enable System ->
  "dout" ::: Signal System DOUT ->
  "" ::: Signal System
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
      LOAD  -> ((SHIFT, newShift), 1, 0)
      SHIFT
        | dnaDetected -> ((DONE, reg), 0, 0)
        | otherwise   -> ((SHIFT, newShift), 0, 1)
      DONE  -> ((DONE, reg), 0,0)
    newShift = input ++# slice d95 d1 reg
    dnaDetected = slice d95 d94 reg == 1 && slice d1 d0 reg == 1

makeTopEntity 'dnaControl

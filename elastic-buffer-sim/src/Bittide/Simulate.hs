{-|

Provides a rudimentary simulation of elastic buffers.

TODO:

  * Define static topologies
  * Ability to extract statistics
  * Ability to generate topologies

-}

{-# LANGUAGE OverloadedStrings #-}

module Bittide.Simulate where

import Clash.Prelude
import Clash.Signal.Internal
import Data.Bifunctor (second)
import Numeric.Natural

import Data.Csv

import Bittide.Simulate.Arithmetic

-- 200kHz instead of 200MHz; otherwise the periods are so small that deviations
-- can't be expressed as 'Natural's
createDomain vSystem{vName="Bittide", vPeriod=hzToPeriod 200e3}

-- Number of type aliases for documentation purposes in various functions defined
-- down below.
type StepSize = Natural
type InitialPeriod = Natural
type DataCount = Natural
type ElasticBufferSize = Natural
type Offset = Integer
type DynamicRange = Natural

-- | Calculate target data count given a FIFO size. Currently returns a target
-- data count of half the FIFO size.
targetDataCount :: ElasticBufferSize -> Natural
targetDataCount size = size `div` 2

-- | Safer version of FINC/FDEC signals present on the Si5395/Si5391 clock multipliers.
data SpeedChange
  = SpeedUp
  | SlowDown
  | NoChange
  deriving (Eq, Show, Generic, NFDataX)

instance ToField SpeedChange where
  toField SpeedUp = "speedUp"
  toField SlowDown = "slowDown"
  toField NoChange = "noChange"

-- | Simple model of the Si5395/Si5391 clock multipliers. In real hardware, these
-- are connected to some oscillator (i.e., incoming Clock) but for simulation purposes
-- we pretend it generates the clock too. In the future we could make it accept a clock
-- too, which would enable us to inject clock jitter in the source clock.
--
-- TODO:
--
--   * Reset adjustment to zero after reset assertion
--   * Ignore speed change requests if requested at a rate >1 MHz
--
clockTuner ::
  forall dom.
  KnownDomain dom =>
  -- | Offset from the ideal period (encoded in the domain) of this clock. For
  -- the Si5395/Si5391 oscillators, this value lies between ±100 ppm.
  Offset ->
  -- | The size of the clock frequency should "jump" on a speed change request.
  StepSize ->
  -- | When asserted, clock multiplier resets the outgoing clock to its original
  -- frequency. TODO: Implement.
  Reset dom ->
  -- | Speed change request. After submitting 'SpeedUp'/'SpeedDown', caller shouldn't
  -- submit another request for 1 microsecond (i.e., the clock tuner effectively operates
  -- at 1 MHz).
  Signal dom SpeedChange ->
  -- | Clock with a dynamic frequency. At the time of writing, Clash primitives don't
  -- account for this yet, so be careful when using them. Note that dynamic frequencies
  -- are only relevant for components handling multiple domains.
  --
  -- We also export the clock's period as a 'Signal' so that it can be easily
  -- observed during simulation.
  (Signal dom PeriodPs, Clock dom)
clockTuner periodOffset stepSize _reset speedChange =
  case knownDomain @dom of
    SDomainConfiguration _ (snatToNum -> period) _ _ _ _ ->
      let initPeriod = fromIntegral (period + periodOffset)
          clockSignal = initPeriod :- go 0 initPeriod speedChange in
      (clockSignal, DClock SSymbol (Just clockSignal))
 where
  -- only process a speed change every 200 cycles
  go ::
    Natural ->
    StepSize ->
    Signal dom SpeedChange ->
    Signal dom PeriodPs
  go 0 !period (sc :- scs) =
    let
      newPeriod = case sc of
        SpeedUp -> period - stepSize
        SlowDown -> period + stepSize
        NoChange -> period
    in
      newPeriod :- go 200 newPeriod scs

  go counter period (_ :- scs) =
    period :- go (pred counter) period scs

-- | Simple model of a FIFO that only models the interesting part for conversion: data
-- counts.
--
-- TODO:
--
--   * Error on buffer overflow/underflow
--
--   * Real FIFOs take a few clock cycles to propagate elements from the write side to
--     the read side. This causes frequency changes to effectively take effect "later
--     than instantly". Model this?
--
elasticBuffer ::
  forall readDom writeDom.
  (KnownDomain readDom, KnownDomain writeDom) =>
  -- | Size of FIFO. To reflect our target platforms, this should be a power of two
  -- where typical sizes would probably be: 16, 32, 64, 128.
  ElasticBufferSize ->
  Clock readDom ->
  Clock writeDom ->
  Signal readDom DataCount
elasticBuffer size (DClock _ (Just readPeriods)) (DClock _ (Just writePeriods)) =
  targetDataCount size :- go 0 (targetDataCount size) readPeriods writePeriods
 where
  go !relativeTime !fillLevel rps wps@(writePeriod :- _) =
    if relativeTime < toInteger writePeriod
    then goRead relativeTime fillLevel rps wps
    else goWrite relativeTime fillLevel rps wps

  goWrite !relativeTime !fillLevel rps (writePeriod :- wps) =
    newFillLevel :- go (relativeTime - toInteger writePeriod) newFillLevel rps wps
   where
    newFillLevel
      | fillLevel == size = size
      | otherwise         = fillLevel + 1

  goRead relativeTime fillLevel (readPeriod :- rps) wps =
    newFillLevel :- go (relativeTime + toInteger readPeriod) newFillLevel rps wps
   where
    newFillLevel
      | fillLevel == 0 = 0
      | otherwise      = fillLevel - 1

elasticBuffer size (DClock ss Nothing) clock1 =
  -- Convert read clock to a "dynamic" clock if it isn't one
  case knownDomain @readDom of
    SDomainConfiguration _ (snatToNum -> period) _ _ _ _ ->
      elasticBuffer size (DClock ss (Just (pure period))) clock1

elasticBuffer size clock0 (DClock ss Nothing) =
  -- Convert write clock to a "dynamic" clock if it isn't one
  case knownDomain @writeDom of
    SDomainConfiguration _ (snatToNum -> period) _ _ _ _ ->
      elasticBuffer size clock0 (DClock ss (Just (pure period)))

minTOffset, maxTOffset :: Ppm -> PeriodPs -> Integer
minTOffset ppm period = toInteger (fastPeriod ppm period) - toInteger period
maxTOffset ppm period = toInteger (slowPeriod ppm period - period)

-- | Determines how to influence clock frequency given statistics provided by
-- all elastic buffers.
--
-- TODO:
--
--   * Generalize to make it easy to "swap" strategies?
--
clockControl ::
  forall n dom.
  (KnownNat n, KnownDomain dom, 1 <= n) =>
  -- | The size of the clock frequency should "jump" on a speed change request.
  StepSize ->
  -- | Maximum divergence from initial frequency. Used to prevent frequency
  -- runoff.
  Ppm ->
  -- | Size of elastic buffers. Used to observe bounds and 'targetDataCount'.
  ElasticBufferSize ->
  -- | Statistics provided by elastic buffers.
  Vec n (Signal dom DataCount) ->
  -- | Whether to adjust node clock frequency
  Signal dom SpeedChange
clockControl step ppm elasticBufferSize ebs = res
 where
  (_, res) = go 0 (bundle ebs) 0

  -- we only need to adjust offset every 200 cycles, because the clock tuner
  -- only processes that often
  go ::
    Natural ->
    Signal dom (Vec n DataCount) ->
    Integer ->
    (Integer, Signal dom SpeedChange)
  go 0 (currentSizes :- dataCounts) offs =
    nextOffs `seq` second (speedChange :-) (go 200 dataCounts nextOffs)
   where
    (speedChange, nextOffs) =
      let
        average = sum currentSizes `div` fromIntegral (length currentSizes)
      in case compare average (targetDataCount elasticBufferSize) of
        LT | offs + toInteger step <= ma -> (SlowDown, offs + toInteger step)
        GT | offs - toInteger step >= mi -> (SpeedUp, offs - toInteger step)
        _ -> (NoChange, offs)
  go counter (_ :- dataCounts) offs =
    second (NoChange :-) (go (pred counter) dataCounts offs)

  mi = minTOffset ppm domT
  ma = maxTOffset ppm domT

  domT = snatToNum @PeriodPs (clockPeriod @dom)

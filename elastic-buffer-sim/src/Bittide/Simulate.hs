{-|

Provides a rudimentary simulation of elastic buffers.

TODO:

  * Define static topologies
  * Ability to extract statistics
  * Ability to generate topologies

-}

module Bittide.Simulate where

import Clash.Prelude
import Clash.Signal.Internal
import Numeric.Natural

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
  Clock dom
clockTuner periodOffset stepSize _reset =
  case knownDomain @dom of
    SDomainConfiguration _ (snatToNum -> period) _ _ _ _ ->
      DClock SSymbol . Just . go (fromIntegral (period + periodOffset))
 where
  go :: StepSize -> Signal dom SpeedChange -> Signal dom StepSize
  go !period (sc :- scs) =
    let
      newPeriod = case sc of
        SpeedUp -> period - stepSize
        SlowDown -> period + stepSize
        NoChange -> period
    in
      newPeriod :- go newPeriod scs

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
  go 0 (targetDataCount size) readPeriods writePeriods
 where
  go !relativeTime !fillLevel rps wps@(writePeriod :- _) =
    if relativeTime < toInteger writePeriod
    then goRead relativeTime fillLevel rps wps
    else goWrite relativeTime fillLevel rps wps

  goWrite !relativeTime !fillLevel rps (writePeriod :- wps) =
    (fillLevel + 1) :- go (relativeTime - toInteger writePeriod) (fillLevel + 1) rps wps

  goRead relativeTime fillLevel (readPeriod :- rps) wps =
    (fillLevel - 1) :- go (relativeTime + toInteger readPeriod) (fillLevel - 1) rps wps

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

-- | Determines how to influence clock frequency given statistics provided by
-- all elastic buffers.
--
-- TODO:
--
--   * Account for dynamic ranges: although our clock adjusters / multipliers can
--     arbitrarily adjust clocks, we should stay within bounds to prevent frequency
--     runoff.
--
--   * Generalize to make it easy to "swap" strategies?
--
clockControl ::
  forall n dom.
  (KnownNat n, KnownDomain dom, 1 <= n) =>
  -- | Maximum divergence from initial clock frequency. Used to prevent frequency
  -- runoff.
  DynamicRange ->
  -- | The size of the clock frequency should "jump" on a speed change request.
  StepSize ->
  -- | Size of elastic buffers. Used to observe bounds and 'targetDataCount'.
  ElasticBufferSize ->
  -- | Statistics provided by elastic buffers.
  Vec n (Signal dom DataCount) ->
  -- | Whether to adjust node clock frequency
  Signal dom SpeedChange
clockControl _dynamicRange _stepSize elasticBufferSize = go 0 . bundle
 where
  go :: Word -> Signal dom (Vec n DataCount) -> Signal dom SpeedChange
  go 0 (currentSizes :- dataCounts) = speedChange :- go 200 dataCounts
   where
    speedChange =
      let
        average = sum currentSizes `div` fromIntegral (length currentSizes)
      in case compare average (targetDataCount elasticBufferSize) of
        LT -> SlowDown
        EQ -> NoChange
        GT -> SpeedUp

  go counter (_ :- dataCounts) = NoChange :- go (pred counter) dataCounts

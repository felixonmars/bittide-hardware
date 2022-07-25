-- | This module contains static topologies.
module Bittide.Layout ( genOffs, twoNodes, threeNodes ) where

import Clash.Explicit.Prelude
import Numeric.Natural

import qualified Data.ByteString.Lazy as BSL
import Data.Csv
import System.Random (randomRIO)

import Arithmetic
import Bittide.Simulate

-- | This can be used inside a REPL and fed to @script.py@
dumpCsv :: IO ()
dumpCsv = do
  o1 <- genOffs
  o2 <- genOffs
  o3 <- genOffs
  writeFile "clocks.csv" "clk0,eb01,eb02,,clk1,eb10,eb12,,clk2,eb20,eb21,,\n"
  BSL.appendFile "clocks.csv"
    $ encode
    $ sampleN 10000000
    $ bundle
    $ threeNodes @Bittide @Bittide @Bittide o1 o2 o3

genOffs :: IO Offset
genOffs =
  (`subtract` toInteger specPeriod)
    <$> randomRIO (toInteger minT, toInteger maxT)
 where
  (maxT, minT) = slowFastPeriod 100 specPeriod

-- | Two bittide nodes connected to one another.
twoNodes ::
  ( KnownDomain dom1
  , KnownDomain dom2
  ) =>
  Offset ->
  Offset ->
  ( Signal dom1 Natural
  , Signal dom1 Natural
  , Signal dom1 SpeedChange
  , Signal dom2 Natural
  , Signal dom2 Natural
  , Signal dom2 SpeedChange
  )
twoNodes offs0 offs1 =
  (clk0Signal, eb01, clockControl0, clk1Signal, eb10, clockControl1)
 where

  (clk0Signal, clock0) = clockTuner offs0 step resetGen clockControl0
  eb01 = elasticBuffer ebSz clock0 clock1
  clockControl0 = clockControl step mi ma ebSz (eb01 :> Nil)

  (clk1Signal, clock1) = clockTuner offs1 step resetGen clockControl1
  eb10 = elasticBuffer ebSz clock1 clock0
  clockControl1 = clockControl step mi ma ebSz (eb10 :> Nil)

  ebSz = 128
  step = 1

  -- +/- 150ppm
  mi = minTOffset specPeriod
  ma = maxTOffset specPeriod
  -- TODO: mi/ma depend on offs0, offs1

-- we use 200kHz in simulation
specPeriod :: PeriodPs
specPeriod = hzToPeriod 200e3

minTOffset, maxTOffset :: PeriodPs -> Integer
minTOffset ᴛ = toInteger (fastPeriod 150 ᴛ) - toInteger ᴛ
maxTOffset ᴛ = toInteger (slowPeriod 150 ᴛ - ᴛ)

-- | Three nodes, all connected to one another
threeNodes ::
  ( KnownDomain dom1
  , KnownDomain dom2
  , KnownDomain dom3
  ) =>
  Offset ->
  Offset ->
  Offset ->
  ( Signal dom1 Natural
  , Signal dom1 Natural
  , Signal dom1 Natural
  , Signal dom1 SpeedChange
  , Signal dom2 Natural
  , Signal dom2 Natural
  , Signal dom2 Natural
  , Signal dom2 SpeedChange
  , Signal dom3 Natural
  , Signal dom3 Natural
  , Signal dom3 Natural
  , Signal dom3 SpeedChange
  )
threeNodes offs0 offs1 offs2 =
  ( clk0Signal
  , eb01
  , eb02
  , clockControl0
  , clk1Signal
  , eb10
  , eb12
  , clockControl1
  , clk2Signal
  , eb20
  , eb21
  , clockControl2
  )
 where

  (clk0Signal, clock0) = clockTuner offs0 step resetGen clockControl0
  eb01 = elasticBuffer ebSz clock0 clock1
  eb02 = elasticBuffer ebSz clock0 clock2
  clockControl0 = clockControl step mi ma ebSz (eb01 :> eb02 :> Nil)

  (clk1Signal, clock1) = clockTuner offs1 step resetGen clockControl1
  eb10 = elasticBuffer ebSz clock1 clock0
  eb12 = elasticBuffer ebSz clock1 clock2
  clockControl1 = clockControl step mi ma ebSz (eb10 :> eb12 :> Nil)

  (clk2Signal, clock2) = clockTuner offs2 step resetGen clockControl2
  eb20 = elasticBuffer ebSz clock2 clock0
  eb21 = elasticBuffer ebSz clock2 clock1
  clockControl2 = clockControl step mi ma ebSz (eb20 :> eb21 :> Nil)

  ebSz = 128
  step = 1

  -- +/- 150ppm
  mi = minTOffset specPeriod
  ma = maxTOffset specPeriod

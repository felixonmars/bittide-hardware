-- | This module contains static topologies.
module Bittide.Layout ( dumpCsv, genOffs, twoNodes, threeNodes ) where

import Clash.Explicit.Prelude
import Numeric.Natural
import qualified Prelude as P

import Clash.Signal.Internal (Signal (..))

import qualified Data.ByteString.Lazy as BSL
import Data.Csv
import System.Random (randomRIO)

import Arithmetic
import Bittide.Simulate

type Ps = Natural

timeClock :: Signal dom (Natural, a, b) -> [(Ps, Natural, a, b)]
timeClock = go 0
 where
  go t ((ᴛ, x, y) :- xs) = (t, ᴛ, x, y) : go (t+ᴛ) xs

-- | This can be used inside a REPL and fed to @script.py@
dumpCsv :: Int -> IO ()
dumpCsv n = do
  o1 <- genOffs
  o2 <- genOffs
  o3 <- genOffs
  writeFile "clocks0.csv" "t,clk0,eb01,eb02,,\n"
  writeFile "clocks1.csv" "t,clk1,eb10,eb12,,\n"
  writeFile "clocks2.csv" "t,clk2,eb20,eb21,,\n"
  let (d0,d1,d2) =
          on3 (encode . P.take n)
        $ (\(c0, e00, e01, c1, e10, e11, c2, e20, e21) -> (timeClock (bundle (c0, e00, e01)), timeClock (bundle (c1, e10, e11)), timeClock (bundle (c2, e20, e21))))
        $ threeNodes @Bittide @Bittide @Bittide o1 o2 o3
  BSL.appendFile "clocks0.csv" d0
  BSL.appendFile "clocks1.csv" d1
  BSL.appendFile "clocks2.csv" d2
 where
  on3 f (x, y, z) = (f x, f y, f z)

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
  -- , Signal dom1 SpeedChange
  , Signal dom2 Natural
  , Signal dom2 Natural
  , Signal dom2 Natural
  -- , Signal dom2 SpeedChange
  , Signal dom3 Natural
  , Signal dom3 Natural
  , Signal dom3 Natural
  -- , Signal dom3 SpeedChange
  )
threeNodes offs0 offs1 offs2 =
  ( clk0Signal
  , eb01
  , eb02
  -- , clockControl0
  , clk1Signal
  , eb10
  , eb12
  -- , clockControl1
  , clk2Signal
  , eb20
  , eb21
  -- , clockControl2
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

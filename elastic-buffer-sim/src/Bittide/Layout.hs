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
  writeFile "clocks.csv" "t,clk0,eb01,eb02,,\n" -- clk1,eb10,eb12,,clk2,eb20,eb21,,\n"
  -- FIXME: sample on a ps-basis?
  BSL.appendFile "clocks.csv"
    $ encode
    $ P.take n
    -- $ (\(a, b, c) -> P.zipWith3 (\(t,x0,y0,z0) (_,x1,y1,z1) (_,x2,y2,z2) -> (t,x0,y0,z0)) a b c)

    $ (\(c0, e00, e01, c1, c10, c11, c2, c20, c21) -> timeClock (bundle (c0, e00, e01))) -- , timeClock (bundle (c1, c10, c11)), timeClock (bundle (c2, c20, c21))))
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

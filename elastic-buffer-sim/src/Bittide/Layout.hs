-- | This module contains static topologies.
module Bittide.Layout ( dumpCsv, genOffs, twoNodes, threeNodes, dumpCsv2,dumpCsv2' ) where

import Clash.Explicit.Prelude
import Numeric.Natural
import qualified Prelude as P

import Clash.Signal.Internal (Signal (..), Clock(..))

import qualified Data.ByteString.Lazy as BSL
import Data.Csv
import System.Random (randomRIO)

import Bittide.Simulate.Arithmetic
import Bittide.Simulate

type Ps = Natural

timeClock :: Signal dom (PeriodPs, a, b) -> [(Ps, PeriodPs, a, b)]
timeClock = go 0
 where
  go t ((period, x, y) :- xs) = (t, period, x, y) : go (t+period) xs

-- | This can be used inside a REPL and fed to @script.py@
dumpCsv :: Int -> IO ()
dumpCsv n = do
  o1 <- genOffs
  o2 <- genOffs
  o3 <- genOffs
  writeFile "clocks0.csv" "t,clk0,eb01,eb02,\n"
  writeFile "clocks1.csv" "t,clk1,eb10,eb12,\n"
  writeFile "clocks2.csv" "t,clk2,eb20,eb21,\n"
  let (dat0, dat1, dat2) =
          on3 (encode . P.take n . timeClock)
        $ threeNodes @Bittide @Bittide @Bittide o1 o2 o3
  BSL.appendFile "clocks0.csv" dat0
  BSL.appendFile "clocks1.csv" dat1
  BSL.appendFile "clocks2.csv" dat2
 where
  on3 f (x, y, z) = (f x, f y, f z)

-- | This can be used inside a REPL and fed to @script.py@
dumpCsv2 :: Int -> IO ()
dumpCsv2 n = do
  o1 <- genOffs
  o2 <- genOffs
  writeFile "clocks0.csv" "t,clk0,eb01,\n"
  writeFile "clocks1.csv" "t,clk1,eb10,\n"
  let (dat0, dat1) =
          on2 (encode . P.take n . timeClock)
        $ twoNodes @Bittide @Bittide o1 o2
  BSL.appendFile "clocks0.csv" dat0
  BSL.appendFile "clocks1.csv" dat1
 where
  on2 f (x, y) = (f x, f y)


dumpCsv2' :: Int -> IO ()
dumpCsv2' n = do
  let o1 = 0
  let o2 = 0

  writeFile "clocks0.csv" "t,clk0,eb01,\n"
  writeFile "clocks1.csv" "t,clk1,eb10,\n"
  let (dat0, dat1) =
          on2 (encode . P.take n . timeClock)
        $ twoNodes' @Bittide @Bittide o1 o2
  BSL.appendFile "clocks0.csv" dat0
  BSL.appendFile "clocks1.csv" dat1
 where
  on2 f (x, y) = (f x, f y)



genOffs :: IO Offset
genOffs =
  (`subtract` toInteger specPeriod)
    <$> randomRIO (toInteger minT, toInteger maxT)
 where
  (maxT, minT) = slowFastPeriod specPpm specPeriod
  specPpm = 100

-- we use 200kHz in simulation
specPeriod :: PeriodPs
specPeriod = hzToPeriod 200e3

maxPpm :: Ppm
maxPpm = 150

-- | Two bittide nodes connected to one another.
twoNodes ::
  ( KnownDomain dom1
  , KnownDomain dom2
  ) =>
  Offset ->
  Offset ->
  ( Signal dom1 (PeriodPs, DataCount, SpeedChange)
  , Signal dom2 (PeriodPs, DataCount, SpeedChange)
  )
twoNodes offs0 offs1 =
  ( bundle (clk0Signal, eb01, clockControl0)
  , bundle (clk1Signal, eb10, clockControl1)
  )
 where

  (clk0Signal, clock0) = clockTuner offs0 step resetGen clockControl0
  eb01 = elasticBuffer ebSz clock0 clock1
  clockControl0 = clockControl offs0 step maxPpm ebSz (eb01 :> Nil)

  (clk1Signal, clock1) = clockTuner offs1 step resetGen clockControl1
  eb10 = elasticBuffer ebSz clock1 clock0
  clockControl1 = clockControl offs1 step maxPpm ebSz (eb10 :> Nil)

  ebSz = 128
  step = 1


twoNodes' ::
  forall dom0 dom1.
  ( KnownDomain dom0
  , KnownDomain dom1
  ) =>
  Offset ->
  Offset ->
  ( Signal dom0 (PeriodPs, DataCount, SpeedChange)
  , Signal dom1 (PeriodPs, DataCount, SpeedChange)
  )
twoNodes' offs0 offs1 =
  ( bundle (clk0Signal, eb01, clockControl0)
  , bundle (clk1Signal, eb10, clockControl1)
  )
 where

  change = 100
  period0 = (snatToNatural $ clockPeriod @dom0) -- - 1000000
  -- (clk0Signal, clock0) = clockTuner offs0 step resetGen clockControl0
  -- clk0Signal = pure period0
  clk0Signal = foo period0 [(5*k,period0),(20*k,period0-change),(30*k,period0),(20*k,period0+change)]

  -- clock0 = clockGen @dom0
  clock0 = DClock SSymbol (Just clk0Signal)

  eb01 = elasticBuffer ebSz clock0 clock1
  -- clockControl0 = clockControl offs0 step maxPpm ebSz (eb01 :> Nil)
  clockControl0 = pure NoChange

  -- (clk1Signal, clock1) = clockTuner offs1 step resetGen clockControl1
  clock1 = DClock SSymbol (Just clk1Signal)
  eb10 = elasticBuffer ebSz clock1 clock0
  period1 = snatToNatural $ clockPeriod @dom1
  -- clk1Signal = foo period1 [(5*k,period1),(20*k,period1+change),(20*k,period1),(20*k,period1-change)]
  clk1Signal = pure period1

  -- clockControl1 = clockControl offs1 step maxPpm ebSz (eb10 :> Nil)
  -- clockControl1 = foo NoChange [(20*k,NoChange),(20*k,SpeedUp),(20*k,NoChange),(20*k,SlowDown)]
  clockControl1 = pure NoChange

  foo end xs = fromList $ P.concatMap (\(n,v) -> P.replicate n v) xs <> P.repeat end
  k = 10000
  ebSz = 128
  step = 1


-- | Three nodes, all connected to one another
threeNodes ::
  ( KnownDomain dom1
  , KnownDomain dom2
  , KnownDomain dom3
  ) =>
  Offset ->
  Offset ->
  Offset ->
  ( Signal dom1 (PeriodPs, DataCount, DataCount)
  , Signal dom2 (PeriodPs, DataCount, DataCount)
  , Signal dom3 (PeriodPs, DataCount, DataCount)
  )
threeNodes offs0 offs1 offs2 =
  ( bundle (clk0Signal, eb01, eb02)
  , bundle (clk1Signal, eb10, eb12)
  , bundle (clk2Signal, eb20, eb21)
  )
 where

  (clk0Signal, clock0) = clockTuner offs0 step resetGen clockControl0
  eb01 = elasticBuffer ebSz clock0 clock1
  eb02 = elasticBuffer ebSz clock0 clock2
  clockControl0 = clockControl offs0 step maxPpm ebSz (eb01 :> eb02 :> Nil)

  (clk1Signal, clock1) = clockTuner offs1 step resetGen clockControl1
  eb10 = elasticBuffer ebSz clock1 clock0
  eb12 = elasticBuffer ebSz clock1 clock2
  clockControl1 = clockControl offs1 step maxPpm ebSz (eb10 :> eb12 :> Nil)

  (clk2Signal, clock2) = clockTuner offs2 step resetGen clockControl2
  eb20 = elasticBuffer ebSz clock2 clock0
  eb21 = elasticBuffer ebSz clock2 clock1
  clockControl2 = clockControl offs2 step maxPpm ebSz (eb20 :> eb21 :> Nil)

  ebSz = 128
  step = 1

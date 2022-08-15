-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

-- | This module generates a static topology using template haskell and then
-- dumps clock periods and elastic buffer occupancy to csv.
module Bittide.Topology ( dumpCsv, plotEbs ) where

import Clash.Explicit.Prelude
import Control.Monad (replicateM, forM_, zipWithM_)
import Data.Functor (void)
import Numeric.Natural
import Prelude qualified as P
import Data.List qualified as L

import Data.Array qualified as A
import Data.ByteString.Lazy qualified as BSL
import Data.Csv
import Graphics.Matplotlib (Matplotlib, (%), mp, plot, file, xlabel, ylabel)
import System.Random (randomRIO)

import Bittide.Simulate
import Bittide.Simulate.Ppm
import Bittide.Topology.Graph
import Bittide.Topology.TH

plotDat :: [(Ps, PeriodPs, DataCount, DataCount, DataCount, DataCount, DataCount)] -> Matplotlib
plotDat = uncurry plot . P.unzip . fmap (fst . $(extrClocks 5))

-- | This samples @n@ steps and plots clock speeds, saved in @clock.pdf@.
plotEbs :: Int -> IO ()
plotEbs m = do
  offs <- replicateM (n+1) genOffs
  let dats =
          onN (plotDat . P.take m)
        $ $(simNodesFromGraph (kn 6)) offs
      -- TODO: auto-ebs
  void $ file "clocks.pdf" (xlabel "Time (ps)" % L.foldl' (%) mp dats)
 where
  onN = $(onTup 6)
  (0, n) = A.bounds g
  g = kn 6

-- | This samples @n@ steps and writes results in @.csv@ files.
dumpCsv :: Int -> IO ()
dumpCsv m = do
  offs <- replicateM (n+1) genOffsets
  forM_ [0..n] $ \i ->
    let eb = g A.! i in
    writeFile
      ("clocks" <> show i <> ".csv")
      ("t,clk" <> show i <> P.concatMap (\j -> ",eb" <> show i <> show j) eb <>  "\n")
  let dats =
          onN (encode . P.take m)
        $ $(simNodesFromGraph defClockConfig (complete 6)) offs
  zipWithM_ (\dat i ->
    BSL.appendFile ("clocks" <> show i <> ".csv") dat) dats [(0::Int)..]
 where
  onN = $(onTup 6)
  (0, n) = A.bounds g
  g = complete 6

-- | Randomly generate a 'Offset', how much a real clock's period may differ
-- from its spec.
genOffsets :: IO Offset
genOffsets =
  (`subtract` toInteger specPeriod)
    <$> randomRIO (toInteger minT, toInteger maxT)
 where
  minT = speedUpPeriod specPpm specPeriod
  maxT = slowDownPeriod specPpm specPeriod

-- | Clocks uncertainty is ±100 ppm
specPpm :: Ppm
specPpm = Ppm 100

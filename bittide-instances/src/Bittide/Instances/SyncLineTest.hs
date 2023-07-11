-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- | Checks to verify the connectivity/functionality of the shared synchronisation line
module Bittide.Instances.SyncLineTest where

import Clash.Explicit.Prelude

import Clash.Annotations.TH (makeTopEntity)
import Clash.Cores.Xilinx.VIO
import Clash.Cores.Xilinx.Extra (ibufds)

import Bittide.Instances.Domains

type TestStart = Bool
type Counter = Unsigned 32
data TestState = Idle | Start | WaitForSync Counter | Done Counter | Error Counter deriving (Generic,NFDataX)

toDoneSuccess :: TestState -> (Bool, Bool, Counter)
toDoneSuccess st = case st of
  WaitForSync cntr -> (False,False,cntr)
  Done cntr -> (True, True, cntr)
  Error cntr -> (True, False, cntr)
  _ -> (False,False,0)

timeout :: Counter
timeout = 20000

syncLineTest ::
  "CLK_125MHZ" ::: DiffClock Basic125 ->
  "SYNC_IN" ::: Signal Basic125 Bool ->
  "" ::: Signal Basic125
    ( "done" ::: Bool
    , "success" ::: Bool
    , "SYNC_OUT" ::: Bool
    )
syncLineTest diffClk sync_in = bundle (testDone, testSuccess, sync_out)
 where
  clk = ibufds diffClk
  rst = unsafeFromActiveLow testStart

  (testState,sync_out) = mealyB clk rst enableGen go Idle sync_in
  (testDone, testSuccess, counter_out) = unbundle $ toDoneSuccess <$> testState

  testStart =
    vioProbe
      ("probe_test_done" :> "probe_test_success" :> "counter" :> Nil)
      ("probe_test_start" :> Nil)
      False
      clk
      testDone
      testSuccess
      counter_out

go :: TestState -> Bool -> (TestState,(TestState,Bool))
go st sync_in = (st',(st,sync_out))
 where
  st' = case st of
    Idle  | sync_in -> Error 0  -- error when the sync_in is asserted before we started
          | otherwise -> Start
    Start -> WaitForSync 0
    WaitForSync n | sync_in -> Done n
                  | n > timeout -> Error n
                  | otherwise -> WaitForSync (n+1)
    Done n  -> Done n
    Error n -> Error n
  sync_out = case st of
    Start         -> True
    WaitForSync _ -> True
    _             -> False

makeTopEntity 'syncLineTest

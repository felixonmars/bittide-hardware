-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE FlexibleContexts #-}

module Bittide.Transceiver (transceiverPrbs, transceiverPrbsN) where

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx.GTH

import Data.Proxy

transceiverPrbsN ::
  ( KnownNat chansUsed
  , KnownDomain freeclk
  , KnownDomain tx
  , KnownDomain rx
  ) =>
  Clock refclk ->
  Clock freeclk ->

  Reset freeclk -> -- ^ rst all
  Reset freeclk -> -- ^ rst txStim
  Reset freeclk -> -- ^ rst prbsChk

  Vec chansUsed String ->
  Vec chansUsed String ->

  Vec chansUsed (Signal rxS (BitVector 1)) ->
  Vec chansUsed (Signal rxS (BitVector 1)) ->

  Vec chansUsed
    ( Clock tx
    , Clock rx
    , Signal txS (BitVector 1)
    , Signal txS (BitVector 1)
    , Signal rx Bool  -- link up
    )
transceiverPrbsN refclk freeclk rst_all rst_txStim rst_prbsChk chanNms clkPaths rxns rxps =
  zipWith4
    (transceiverPrbs refclk freeclk rst_all rst_txStim rst_prbsChk)
    chanNms
    clkPaths
    rxns
    rxps

transceiverPrbs ::
  ( KnownDomain freeclk
  , KnownDomain tx
  , KnownDomain rx
  ) =>

  Clock refclk ->
  Clock freeclk ->

  Reset freeclk ->  -- ^ rst all
  Reset freeclk ->  -- ^ rst txStim
  Reset freeclk ->  -- ^ rst prbsChk

  String -> -- ^ channel, example X0Y18
  String -> -- ^ clkPath, example clk0-2

  Signal rxS (BitVector 1) ->
  Signal rxS (BitVector 1) ->

  ( Clock tx
  , Clock rx
  , Signal txS (BitVector 1)
  , Signal txS (BitVector 1)
  , Signal rx Bool  -- link up
  )
transceiverPrbs gtrefclk freeclk rst_all_btn rst_txStim rst_prbsChk chan clkPath rxn rxp
 = (tx_clk, rx_clk, txn, txp, link_up)
 where
  (txn, txp, tx_clk, rx_clk, rx_data, reset_tx_done, reset_rx_done, tx_active)
    = gthCore -- @_ @_ @RxS @Freerun @TxUser2 @RefClk @RefClk @TxS @RxUser2
        chan clkPath
        rxn
        rxp

        freeclk -- gtwiz_reset_clk_freerun_in

        rst_all
        noReset -- gtwiz_reset_tx_pll_and_datapath_in
        noReset -- gtwiz_reset_tx_datapath_in
        noReset -- gtwiz_reset_rx_pll_and_datapath_in
        rst_rx -- gtwiz_reset_rx_datapath_in
        gtwiz_userdata_tx_in
        txctrl2
        freeclk -- drpclk_in
        gtrefclk -- gtrefclk0_in

  (gtwiz_userdata_tx_in,txctrl2) = prbsStimuliGen tx_clk txStimRst
  rstPrbsChk =
      resetGlitchFilter (SNat @125000) rx_clk
    $ unsafeFromActiveHigh
    $ unsafeSynchronizer freeclk rx_clk
    $ unsafeToActiveHigh rst_prbsChk

  prbsErrors = prbsChecker rx_clk rstPrbsChk enableGen prbsConf31w64 rx_data
  anyErrors = fmap (pack . reduceOr) prbsErrors
  link_up = linkStateTracker rx_clk noReset anyErrors

  rst_all_in = resetGlitchFilter (SNat @125000) freeclk rst_all_btn

  (rst_all, rst_rx, _init_done) =
    gthResetManager
      freeclk rst_all_in rx_clk
      (unpack <$> reset_tx_done)
      (unpack <$> reset_rx_done)
      link_up

  txStimRst' =
      resetGlitchFilter (SNat @125000) tx_clk
    $ unsafeFromActiveHigh
    $ unsafeSynchronizer freeclk tx_clk
    $ unsafeToActiveHigh rst_txStim

  txStimRst =
      resetSynchronizer tx_clk
    $ orReset txStimRst' (unsafeFromActiveLow $ fmap bitCoerce tx_active)

orReset :: KnownDomain dom => Reset dom -> Reset dom -> Reset dom
orReset a b = unsafeFromActiveHigh (unsafeToActiveHigh a .||. unsafeToActiveHigh b)

noReset :: KnownDomain dom => Reset dom
noReset = unsafeFromActiveHigh (pure False)

data LinkSt = Down | Up deriving (Eq, Show, Generic, NFDataX)
type LinkStCntr = Index 127

linkStateTracker ::
  (KnownDomain dom, KnownNat w) =>
  Clock dom ->
  Reset dom ->
  Signal dom (BitVector w) ->
  Signal dom Bool
linkStateTracker clk rst =
  fst . mooreB clk rst enableGen update genOutput initSt . (reduceOr <$>)
 where
  initSt = (Down,0)
  update :: (LinkSt, LinkStCntr) -> Bit -> (LinkSt, LinkStCntr)
  update (st,cntr) anyErrs =
    case (st,anyErrs) of
      (Down,1) -> (Down,0)
      (Down,_)
        | cntr < maxBound -> (Down, cntr+1)
        | otherwise       -> (Up, cntr)
      (Up,1)
        | cntr > 33 -> (Up, cntr-34)
        | otherwise -> (Down, 0)
      (Up,_) -> (Up, boundedAdd cntr 1)

  genOutput :: (LinkSt, LinkStCntr) -> (Bool, LinkStCntr)
  genOutput (st, cntr) = (st == Up, cntr)

prbsStimuliGen ::
  forall dom .
  KnownDomain dom =>
  Clock dom ->
  Reset dom ->
  ( Signal dom (BitVector 64)
  , Signal dom (BitVector 8) )
prbsStimuliGen clk rst =
  ( mux sendCommas (pure commas)   prbs
  , mux sendCommas (pure maxBound) (pure 0))
 where
  comma :: BitVector 8
  comma = 0xbc

  commas :: BitVector 64
  commas = pack $ repeat comma

  sendCommas :: Signal dom Bool
  sendCommas =
    moore
      clk rst enableGen
      (\s _ -> satSucc SatBound s)
      (/= maxBound)
      (0::Index 10240)
      (pure ())

  prbs = prbsGen clk rst enableGen prbsConf31w64

type PrbsConfig polyLength polyTap nBits =
  ( SNat polyLength
  , SNat polyTap
  , SNat nBits
  , Bool
  )


prbsConf31w64 :: PrbsConfig 31 28 64
prbsConf31w64 = (d31,d28,d64,True)

prbsGen ::
  forall dom polyLength polyTap nBits _n0 _n1 _n2 _n3 .
  ( KnownDomain dom
  , (_n0 + 1) ~ nBits
  , (polyTap + _n1) ~ polyLength
  , polyTap ~ (_n2 + 1)
  , _n1 ~ (_n3 + 1)
  ) =>
  Clock dom -> Reset dom -> Enable dom ->
  PrbsConfig polyLength polyTap nBits ->
  Signal dom (BitVector nBits)
prbsGen clk rst ena (pLen@SNat, tap'@SNat, SNat, inv) =
  mealy clk rst ena go (maxBound,maxBound) (pure ())
 where
  go ::
    (BitVector polyLength, BitVector nBits) ->
    () ->
    ((BitVector polyLength, BitVector nBits), BitVector nBits)
  go (prbs_reg, prbs_out_prev) _ =
    ( ( last prbs
      , (if inv then complement else id) $ pack (reverse $ map msb prbs))
    , prbs_out_prev
    )
   where
     prbs :: Vec nBits (BitVector polyLength)
     prbs = unfoldrI goPrbs prbs_reg

     goPrbs :: BitVector polyLength -> (BitVector polyLength, BitVector polyLength)
     goPrbs bv = (o,o)
      where
       o = nb +>>. bv
       tap = subSNat pLen tap'
       nb = xor (lsb bv) (unpack $ slice tap tap bv)


prbsChecker ::
  forall dom polyLength polyTap nBits _n0 _n1 _n2 .
  ( KnownDomain dom
  , (_n0 + 1) ~ nBits
  , (polyTap + _n1) ~ polyLength
  , polyTap ~ (_n2 + 1)
  ) =>
  Clock dom -> Reset dom -> Enable dom ->
  PrbsConfig polyLength polyTap nBits ->
  Signal dom (BitVector nBits) ->
  Signal dom (BitVector nBits)
prbsChecker clk rst ena (pLen@SNat, tap'@SNat, SNat, inv) sigPrbsIn =
  mealy
    clk rst ena
    go
    (maxBound, maxBound)
    (fmap (if inv then complement else id) sigPrbsIn)
 where
  go ::
    (BitVector polyLength, BitVector nBits) ->
    BitVector nBits ->
    ((BitVector polyLength, BitVector nBits), BitVector nBits)
  go (prbs_reg, prbs_out_prev) prbsIn =
    ( (prbs_state, pack $ reverse prbs_out)
    , prbs_out_prev
    )
   where
     prbs_out :: Vec nBits Bit
     prbs_state :: BitVector polyLength
     (prbs_state, prbs_out) = mapAccumL goPrbs prbs_reg (reverse $ unpack prbsIn)

     goPrbs :: BitVector polyLength -> Bit -> (BitVector polyLength, Bit)
     goPrbs bv inp = (o, bitErr)
      where
       o = inp +>>. bv
       tap = subSNat pLen tap'
       bitErr = xor inp (xor (lsb bv) (unpack $ slice tap tap bv))

type Counter = Unsigned 25

data GthLinkRstSt
  = Start Bool
  | TxWait Counter
  | RxWait Counter
  | Monitor
  deriving (Generic,NFDataX)

gthResetManager ::
  forall freerun rxUser2 .
  ( KnownDomain freerun
  , KnownDomain rxUser2
  ) =>
  Clock freerun ->
  "reset_all_in" ::: Reset freerun ->
  Clock rxUser2 ->
  "tx_init_done" ::: Signal freerun Bool ->
  "rx_init_done" ::: Signal freerun Bool ->
  "rx_data_good" ::: Signal rxUser2 Bool ->
  ( "reset_all_out" ::: Reset freerun
  , "reset_rx"  ::: Reset freerun
  , "init_done" ::: Signal freerun Bool
  )
gthResetManager free_clk reset_all_in tx_clk tx_init_done rx_init_done rx_data_good' =
  ( unsafeFromActiveHigh reset_all_out_sig
  , unsafeFromActiveHigh reset_rx_sig
  , init_done
  )
 where
  rx_data_good = dualFlipFlopSynchronizer tx_clk free_clk reset_all_in enableGen False rx_data_good'
  (reset_all_out_sig, reset_rx_sig, init_done) =
    mooreB
      free_clk reset_all_in enableGen
      update
      extractOutput
      initSt
      (tx_init_done, rx_init_done, rx_data_good)

  initSt :: GthLinkRstSt
  initSt = Start True

  update :: GthLinkRstSt -> (Bool, Bool, Bool) -> GthLinkRstSt
  update st (tx_done, rx_done, rx_good) =
    case st of
      Start _ -> TxWait 0

      TxWait cntr
        | tx_done -> RxWait 0
        | cntr <= tx_timer -> TxWait (succ cntr)
        | otherwise -> Start True

      RxWait cntr
        | rx_done && rx_good -> Monitor
        | cntr <= rx_timer -> RxWait (succ cntr)
        | otherwise -> Start False

      Monitor
        | rx_done && rx_good -> Monitor
        | otherwise -> Start False

  extractOutput st = case st of
    Start rstAll -> (rstAll, not rstAll, False)
    TxWait _     -> (False,  False,      False)
    RxWait _     -> (False,  False,      False)
    Monitor      -> (False,  False,      True)

  tx_timer = cyclesForMilliSeconds @freerun (SNat @30 )
  rx_timer = cyclesForMilliSeconds @freerun (SNat @130)

-- | Calculates how many cycles of a certain domain fit in some number of milliseconds
cyclesForMilliSeconds :: forall dom ms a . (Num a, KnownDomain dom) => SNat ms -> a
cyclesForMilliSeconds x =
  fromInteger ((snatToInteger x * 1000_000_000) `div` period)
 where
  period = natVal (Proxy @(DomainPeriod dom))

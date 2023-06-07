-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE FlexibleContexts #-}

module Bittide.Transceiver (transceiverPrbs, transceiverPrbsN) where
import qualified Clash.Prelude as C
import Clash.Explicit.Prelude
import Clash.Cores.Xilinx.GTH hiding (ChansUsed)
import Clash.Xilinx.ClockGen
import Clash.Annotations.Primitive    (hasBlackBox, Primitive (InlineYamlPrimitive), HDL(..))

import Clash.Annotations.SynthesisAttributes
import Data.String.Interpolate (__i)

import Clash.Cores.Xilinx.VIO


concatBvs :: (KnownNat n, KnownNat m) => Vec n (BitVector m) -> BitVector (n*m)
concatBvs = bitCoerce

splitBvs :: (KnownNat n, KnownNat m) => BitVector (n*m) -> Vec n (BitVector m)
splitBvs = bitCoerce

transceiverPrbsN
  :: (KnownNat chansUsed, KnownDomain freeclk, KnownDomain tx, KnownDomain rx,_)
  => Clock refclk
  -> Clock freeclk

  -> Signal freeclk Bool  -- rst all
  -> Signal freeclk Bool  -- rst txStim
  -> Signal freeclk Bool  -- rst prbsChk

  -> Vec chansUsed String
  -> Vec chansUsed String

  -> Vec chansUsed (Signal rxS (BitVector 1))
  -> Vec chansUsed (Signal rxS (BitVector 1))

  -> Vec chansUsed
       ( Clock tx
       , Clock rx
       , Signal txS (BitVector 1)
       , Signal txS (BitVector 1)
       , Signal rx Bool  -- link up
       )
transceiverPrbsN refclk freeclk rst_all rst_txStim rst_prbsChk chanNms clkPaths rxns rxps =
  zipWith4 (transceiverPrbs refclk freeclk rst_all rst_txStim rst_prbsChk) chanNms clkPaths rxns rxps

transceiverPrbs
  :: (KnownDomain freeclk, KnownDomain tx, KnownDomain rx,_)

  => Clock refclk
  -> Clock freeclk

  -> Signal freeclk Bool  -- rst all
  -> Signal freeclk Bool  -- rst txStim
  -> Signal freeclk Bool  -- rst prbsChk

  -> String -- ^ channel, example X0Y18
  -> String -- ^ clkPath, example clk0-2

  -> Signal rxS (BitVector 1)
  -> Signal rxS (BitVector 1)


  -> ( Clock tx
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
        -- gtrefclk -- gtrefclk00_in
        freeclk -- drpclk_in
        gtrefclk -- gtrefclk0_in

  (gtwiz_userdata_tx_in,txctrl2) = prbsStimuliGen tx_clk txStimRst
  rstPrbsChk = resetGlitchFilter (SNat @125000) rx_clk (unsafeFromHighPolarity $ unsafeSynchronizer freeclk rx_clk (rst_prbsChk .||. vio_rst_prbsChk))

  prbsErrors = C.withClockResetEnable rx_clk rstPrbsChk enableGen prbsChecker prbsConf31w64 rx_data
  anyErrors = fmap (pack . reduceOr) prbsErrors
  link_up = linkStateTracker rx_clk noReset anyErrors

  rst_all_in = resetGlitchFilter (SNat @125000) freeclk (unsafeFromHighPolarity (rst_all_btn .||. vio_rst_all_btn)) -- $ unsafeSynchronizer freeclk rx_clk sw_n)

  (rst_all, rst_rx, init_done) = gthResetManager freeclk rst_all_in rx_clk (unpack <$> reset_tx_done) (unpack <$> reset_rx_done) link_up

  txStimRst' = resetGlitchFilter (SNat @125000) tx_clk (unsafeFromHighPolarity $ unsafeSynchronizer freeclk tx_clk (rst_txStim .||. vio_rst_txStim))
  txStimRst = resetSynchronizer tx_clk $ orReset txStimRst' (unsafeFromLowPolarity $ fmap bitCoerce tx_active)

  (  fmap unpack -> vio_rst_all_btn
   , fmap unpack -> vio_rst_txStim
   , fmap unpack -> vio_rst_prbsChk
   ) = unbundle $ vioProbe ("rst_all" :> "rst_rx" :> "init_done" :> Nil) ("rst_all_out" :> "rst_txStim" :> "rst_prbsChk" :> Nil) (0,0,0) freeclk (pack <$> unsafeToHighPolarity rst_all) (pack <$> unsafeToHighPolarity rst_rx) (pack <$> init_done)

orReset :: KnownDomain dom => Reset dom -> Reset dom -> Reset dom
orReset a b = unsafeFromHighPolarity (unsafeToHighPolarity a .||. unsafeToHighPolarity b)


noReset :: KnownDomain dom => Reset dom
noReset = unsafeFromHighPolarity (pure False)

data LinkSt = Down | Up deriving (Eq,Show,Generic,NFDataX)
type LinkStCntr = Index 127

linkStateTracker :: (KnownDomain dom, KnownNat w) => Clock dom -> Reset dom -> Signal dom (BitVector w) -> Signal dom Bool
linkStateTracker clk rst x = fst $ linkStateTracker' clk rst x
linkStateTracker' :: (KnownDomain dom, KnownNat w) => Clock dom -> Reset dom -> Signal dom (BitVector w) -> (Signal dom Bool, Signal dom LinkStCntr)
linkStateTracker' clk rst = mooreB clk rst enableGen update genOutput initSt . (reduceOr <$>)
 where
  initSt = (Down,0)
  update :: (LinkSt,LinkStCntr) -> Bit -> (LinkSt,LinkStCntr)
  update (st,cntr) anyErrs = case (st,anyErrs) of
    (Down,1) -> (Down,0)
    (Down,0) | cntr < maxBound -> (Down, cntr+1)
             | otherwise       -> (Up,cntr)
    (Up,0) -> (Up, boundedAdd cntr 1)
    (Up,1) | cntr > 33 -> (Up, cntr-34)
           | otherwise -> (Down, 0)
  -- genOutput = (== Up) . fst
  genOutput (st,cntr) = (st == Up, cntr)

prbsStimuliGen :: KnownDomain dom => Clock dom -> Reset dom -> (Signal dom (BitVector 64), Signal dom (BitVector 8))
prbsStimuliGen clk rst = (mux sendCommas (pure commas) prbs, mux sendCommas (pure maxBound) (pure 0))
 where
   comma = 0xbc :: BitVector 8
   commas = pack $ repeat comma
   sendCommas = moore clk rst enableGen (\s _ -> satSucc SatBound s) (/= maxBound) (0::Index 10240) (pure ())
   prbs = C.withClockResetEnable clk rst enableGen prbsGen prbsConf31w64

type PrbsConfig polyLength polyTap nBits = (SNat polyLength, SNat polyTap, SNat nBits, Bool)

-- prbsConf7w8 :: PrbsConfig 7 6 8
-- prbsConf7w8 = (d7,d6,d8,False)
-- prbsConf7w4 :: PrbsConfig 7 6 4
-- prbsConf7w4 = (d7,d6,d4,False)
prbsConf31w64 :: PrbsConfig 31 28 64
prbsConf31w64 = (d31,d28,d64,True)

prbsGen
  :: forall dom polyLength polyTap nBits _n0 _n1 _n2 _n3.
    (C.HiddenClockResetEnable dom, (_n0 + 1) ~ nBits, (polyTap + _n1) ~ polyLength, polyTap ~ (_n2 + 1), _n1 ~ (_n3 + 1))
  => PrbsConfig polyLength polyTap nBits
  -> Signal dom (BitVector nBits)

prbsGen (pLen@SNat, tap'@SNat, SNat, inv) = C.mealy go (maxBound,maxBound) (pure ())
 where
   go :: (BitVector polyLength, BitVector nBits) -> () -> ((BitVector polyLength, BitVector nBits), BitVector nBits)
   go (prbs_reg,prbs_out_prev) _ = ((last prbs, (if inv then complement else id) $ pack (reverse $ map msb prbs)), prbs_out_prev)
    where
      prbs :: Vec nBits (BitVector polyLength)
      prbs = unfoldrI goPrbs prbs_reg
      goPrbs :: BitVector polyLength -> (BitVector polyLength, BitVector polyLength)
      goPrbs bv = (o,o)
       where
        o = nb +>>. bv
        tap = subSNat pLen tap'
        nb = xor (lsb bv) (unpack $ slice tap tap bv)


prbsChecker
  :: forall dom polyLength polyTap nBits _n0 _n1 _n2 .
    (C.HiddenClockResetEnable dom, (_n0 + 1) ~ nBits, (polyTap + _n1) ~ polyLength, polyTap ~ (_n2 + 1))
  => PrbsConfig polyLength polyTap nBits
  -> Signal dom (BitVector nBits)
  -> Signal dom (BitVector nBits)
prbsChecker (pLen@SNat, tap'@SNat, SNat, inv) sigPrbsIn = C.mealy go (maxBound,maxBound) (fmap (if inv then complement else id)  sigPrbsIn)
 where
   go :: (BitVector polyLength, BitVector nBits) -> BitVector nBits -> ((BitVector polyLength,BitVector nBits), BitVector nBits)
   go (prbs_reg,prbs_out_prev) prbsIn = ((prbs_state,pack $ reverse prbs_out), prbs_out_prev)
    where
      prbs_out :: Vec nBits Bit
      prbs_state :: BitVector polyLength
      (prbs_state,prbs_out) = mapAccumL goPrbs prbs_reg (reverse $ unpack prbsIn)
      goPrbs :: BitVector polyLength -> Bit -> (BitVector polyLength, Bit)
      goPrbs bv inp = (o,bitErr)
       where
        o = inp +>>. bv
        tap = subSNat pLen tap'
        bitErr = xor inp (xor (lsb bv) (unpack $ slice tap tap bv))

type Counter = Unsigned 25
data GthLinkRstSt = Start Bool | TxWait Counter | RxWait Counter | Monitor deriving (Generic,NFDataX)

gthResetManager
  :: forall freerun rxUser2
   . (KnownDomain freerun, KnownDomain rxUser2, _)
  => Clock freerun
  -> "reset_all_in" ::: Reset freerun
  -> Clock rxUser2
  -> "tx_init_done" ::: Signal freerun Bool
  -> "rx_init_done" ::: Signal freerun Bool
  -> "rx_data_good" ::: Signal rxUser2 Bool
  -> ( "reset_all_out" ::: Reset freerun
     , "reset_rx"  ::: Reset freerun
     , "init_done" ::: Signal freerun Bool
     )
gthResetManager free_clk reset_all_in tx_clk tx_init_done rx_init_done rx_data_good' =
  (unsafeFromHighPolarity reset_all_out_sig, unsafeFromHighPolarity reset_rx_sig, init_done)
 where
  rx_data_good = dualFlipFlopSynchronizer tx_clk free_clk reset_all_in enableGen False rx_data_good'
  (reset_all_out_sig, reset_rx_sig, init_done) = mooreB free_clk reset_all_in enableGen update extractOutput initSt (tx_init_done,rx_init_done,rx_data_good)
  initSt = Start True
  update :: GthLinkRstSt -> (Bool,Bool,Bool) -> GthLinkRstSt
  update st (tx_done,rx_done,rx_good) = case st of
    Start _   -> TxWait 0
    TxWait cntr | tx_done -> RxWait 0
                | cntr <= tx_timer -> TxWait (succ cntr)
                | otherwise -> Start True
    RxWait cntr | rx_done && rx_good -> Monitor
                | cntr <= rx_timer -> RxWait (succ cntr)
                | otherwise -> Start False
    Monitor     | rx_done && rx_good -> Monitor
                | otherwise -> Start False

  extractOutput st = case st of
    Start rstAll   -> (rstAll,not rstAll,False)
    TxWait _ -> (False,False,False)
    RxWait _ -> (False,False,False)
    Monitor  -> (False,False,True)
  tx_timer = cyclesForMilliSeconds @freerun (SNat @30 )
  rx_timer = cyclesForMilliSeconds @freerun (SNat @130)

-- cyclesForMilliSecondsSNat :: forall dom ms cycles. (KnownDomain dom, 1 <= (DomainPeriod dom)) => SNat ms -> SNat _ -- , _ {- cycles ~ (ms `Div` (DomainPeriod dom)) -}) => SNat ms -> _ -- SNat cycles
-- cyclesForMilliSecondsSNat SNat = SNat @(ms `Div` (DomainPeriod dom))

cyclesForMilliSeconds
  :: forall dom ms a period edge reset init polarity
   . ( KnownDomain dom
     , Num a
     , KnownConf dom ~ 'DomainConfiguration dom period edge reset init polarity
     )
  => SNat ms -> a
-- cyclesForMilliSeconds = snatToNum . cyclesForMilliSecondsSNat
cyclesForMilliSeconds ms = fromInteger ((snatToInteger ms * 1000_000_000) `div` period)
  where
    period :: Integer
    period = snatToInteger (sPeriod (knownDomain @dom))

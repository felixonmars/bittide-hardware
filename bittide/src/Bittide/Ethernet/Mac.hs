{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
module Bittide.Ethernet.Mac where

import Clash.Prelude

import Clash.Annotations.Primitive
import Clash.Annotations.TH
import Data.String.Interpolate (__i)
import Clash.Annotations.TH
import Protocols
import Protocols.Axi4.Stream
import Protocols.Internal(CSignal(..))
import Bittide.Extra.Maybe

data GmiiTx = GmiiTx
  { _txd :: "gmii_txd" ::: BitVector 8
  , _tx_en :: "gmii_tx_en" ::: Bool
  , _tx_er :: "gmii_tx_er" ::: Bool
  }

data GmiiRx = GmiiRx
  { _rxd :: "gmii_rxd" ::: BitVector 8
  , _rx_dv :: "gmii_rx_dv" ::: Bool
  , _rx_er :: "gmii_rx_er" ::: Bool
  }

type EthMacAxiConf = 'Axi4StreamConfig 1 0 0
data EthMacConfig =
  EthMacConfig
  { _enable_padding :: "ENABLE_PADDING" ::: Bit
  , _min_frame_length :: "MIN_FRAME_LENGTH" ::: Integer
  , _tx_fifo_depth :: "TX_FIFO_DEPTH" ::: Integer
  , _tx_frame_fifo :: "TX_FRAME_FIFO" ::: Bit
  , _rx_fifo_depth :: "RX_FIFO_DEPTH" ::: Integer
  , _rx_frame_fifo :: "RX_FRAME_FIFO" ::: Bit
  , _ifg_delay :: "ifg_delay" ::: BitVector 8
  }

instance Default EthMacConfig where
  def = EthMacConfig
    { _enable_padding = 1
    , _min_frame_length = 64
    , _tx_fifo_depth = 4096
    , _tx_frame_fifo = 1
    , _rx_fifo_depth = 4096
    , _rx_frame_fifo = 1
    , _ifg_delay = 12
    }

data EthMacStatus = EthMacStatus
  { txFifoOverflow :: "txFifoOverflow" ::: Bool
  , txFifoBadFrame :: "txFifoBadFrame" ::: Bool
  , txFifoGoodFrame :: "txFifoGoodFrame" ::: Bool
  , rxBadFrame :: "rxBadFrame" ::: Bool
  , rxBadFcs :: "rxBadFcs" ::: Bool
  , rxFifoOverflow :: "rxFifoOverflow" ::: Bool
  , rxFifoBadFrame :: "rxFifoBadFrame" ::: Bool
  , rxFifoGoodFrame :: "rxFifoGoodFrame" ::: Bool
  }

data EthMacOutput txDom logicDom axiWidth =
  EthMacOutput
  { _tx_axis_tready :: "tx_axis_tready" ::: Signal logicDom Bool
  , _rx_axis_tdata :: "rx_axis_tdata" ::: Signal logicDom (Vec axiWidth (Unsigned 8))
  , _rx_axis_tvalid :: "rx_axis_tvalid" ::: Signal logicDom Bool
  , _rx_axis_tlast :: "rx_axis_tlast" ::: Signal logicDom Bool
  , _rx_axis_tuser :: "rx_axis_tuser" ::: Signal logicDom Bool
  , _gmii_txd :: "gmii_txd" ::: Signal txDom (BitVector 8)
  , _gmii_tx_en :: "gmii_tx_en" ::: Signal txDom Bool
  , _gmii_tx_er :: "gmii_tx_er" ::: Signal txDom Bool
  , _tx_fifo_overflow :: "tx_fifo_overflow" ::: Signal logicDom Bool
  , _tx_fifo_bad_frame :: "tx_fifo_bad_frame" ::: Signal logicDom Bool
  , _tx_fifo_good_frame :: "tx_fifo_good_frame" ::: Signal logicDom Bool
  , _rx_error_bad_frame :: "rx_error_bad_frame" ::: Signal logicDom Bool
  , _rx_error_bad_fcs :: "rx_error_bad_fcs" ::: Signal logicDom Bool
  , _rx_fifo_overflow :: "rx_fifo_overflow" ::: Signal logicDom Bool
  , _rx_fifo_bad_frame :: "rx_fifo_bad_frame" ::: Signal logicDom Bool
  , _rx_fifo_good_frame :: "rx_fifo_good_frame" :::Signal logicDom Bool
  } deriving (Generic, NFDataX)

ethMac1GCircuit ::
  ( KnownDomain logicDom
  , KnownDomain rxDom
  , KnownDomain txDom) =>
  EthMacConfig ->
  "rx_clk" ::: Clock rxDom ->
  "rx_rst" ::: Reset rxDom ->
  "tx_clk" ::: Clock txDom ->
  "tx_rst" ::: Reset txDom ->
  "logic_clk" ::: Clock logicDom ->
  "logic_rst" ::: Reset logicDom ->
  "rx_clk_enable" ::: Signal rxDom Bool ->
  "tx_clk_enable" ::: Signal txDom Bool ->
  "rx_mii_select" ::: Signal rxDom Bool ->
  "tx_mii_select" ::: Signal txDom Bool ->
  Circuit
    (Axi4Stream logicDom EthMacAxiConf Bool, CSignal rxDom GmiiRx)
    (Axi4Stream logicDom EthMacAxiConf Bool, CSignal txDom GmiiTx, CSignal logicDom EthMacStatus)
ethMac1GCircuit ethMacConfig rxClk rxRst txClk txRst logicClk logicRst rxClkEnable txClkEnable
  rxMiiSelect txMiiSelect = Circuit go
 where
  go ((txAxiM, CSignal gmiiRx), (rxAxiS, _ , _)) =
    ((txAxiS, CSignal $ pure ()), (rxAxiM, CSignal gmiiTx, CSignal ethMacStatus))
   where
    (rxAxiM, txAxiS, gmiiTx, ethMacStatus) =
      ethMac1G ethMacConfig rxClk rxRst txClk txRst logicClk logicRst rxClkEnable
      txClkEnable rxMiiSelect txMiiSelect txAxiM rxAxiS gmiiRx

ethMac1G ::
  ( KnownDomain logicDom
  , KnownDomain rxDom
  , KnownDomain txDom) =>
  EthMacConfig ->
  "rx_clk" ::: Clock rxDom ->
  "rx_rst" ::: Reset rxDom ->
  "tx_clk" ::: Clock txDom ->
  "tx_rst" ::: Reset txDom ->
  "logic_clk" ::: Clock logicDom ->
  "logic_rst" ::: Reset logicDom ->
  "rx_clk_enable" ::: Signal rxDom Bool ->
  "tx_clk_enable" ::: Signal txDom Bool ->
  "rx_mii_select" ::: Signal rxDom Bool ->
  "tx_mii_select" ::: Signal txDom Bool ->
  "rxAxi" ::: Signal logicDom (Maybe (Axi4StreamM2S EthMacAxiConf Bool)) ->
  "txAxi_tready" ::: Signal logicDom Axi4StreamS2M ->
  "gmiiRx" ::: Signal rxDom GmiiRx ->
  "" :::
  ( "txAxi" ::: Signal logicDom (Maybe (Axi4StreamM2S EthMacAxiConf Bool))
  , "rxAxi_tvalid" ::: Signal logicDom Axi4StreamS2M
  , "gmiiTx" ::: Signal txDom GmiiTx
  , "ethStatus" ::: Signal logicDom EthMacStatus
  )
ethMac1G
  EthMacConfig{..} rxClk rxRst txClk txRst logicClk logicRst rxClkEnable txClkEnable
  rxMiiSelect txMiiSelect txAxiM rxAxiS gmiiRx =
  (rxAxiM, txAxiS, gmiiTx, ethMacStatus)
 where
  rxAxiM = (orNothing <$> _rx_axis_tvalid <*>) $ Axi4StreamM2S
    <$> _rx_axis_tdata
    <*> pure (repeat True) --Keep
    <*> pure (repeat True) --Strobe
    <*> _rx_axis_tlast
    <*> pure 0 --id
    <*> pure 0 --dest
    <*> _rx_axis_tuser

  txAxiS = Axi4StreamS2M <$> _tx_axis_tready

  gmiiTx = GmiiTx <$> _gmii_txd <*> _gmii_tx_en <*> _gmii_tx_er
  ethMacStatus = EthMacStatus
    <$> _tx_fifo_overflow
    <*> _tx_fifo_bad_frame
    <*> _tx_fifo_good_frame
    <*> _rx_error_bad_frame
    <*> _rx_error_bad_fcs
    <*> _rx_fifo_overflow
    <*> _rx_fifo_bad_frame
    <*> _rx_fifo_good_frame

  rxAxiReady = _tready <$> rxAxiS
  (txAxiData, txAxiLast, txAxiUser, txAxiValid) = unbundle
    $ maybe (repeat 0, False, False, False)
    (\Axi4StreamM2S{..} -> (_tdata, _tlast, _tuser, True)) <$> txAxiM
  (gmiiRxd, gmiiRxDv, gmiiRxEr) = unbundle $ (\GmiiRx{..} -> (_rxd, _rx_dv, _rx_er)) <$> gmiiRx
  EthMacOutput{..} = ethMac1G#
    _enable_padding _min_frame_length _tx_fifo_depth _tx_frame_fifo _rx_fifo_depth
    _rx_frame_fifo _ifg_delay rxClk rxRst txClk txRst logicClk logicRst rxClkEnable
    txClkEnable rxMiiSelect txMiiSelect rxAxiReady txAxiData txAxiLast txAxiUser
    txAxiValid gmiiRxd gmiiRxDv gmiiRxEr

ethMac1G# ::
  ( KnownDomain logicDom
  , KnownDomain rxDom
  , KnownDomain txDom) =>
  "ENABLE_PADDING" ::: Bit ->
  "MIN_FRAME_LENGTH" ::: Integer ->
  "TX_FIFO_DEPTH" ::: Integer ->
  "TX_FRAME_FIFO" ::: Bit ->
  "RX_FIFO_DEPTH" ::: Integer ->
  "RX_FRAME_FIFO" ::: Bit ->
  "ifg_delay" ::: BitVector 8 ->
  "rx_clk" ::: Clock rxDom ->
  "rx_rst" ::: Reset rxDom ->
  "tx_clk" ::: Clock txDom ->
  "tx_rst" ::: Reset txDom ->
  "logic_clk" ::: Clock logicDom ->
  "logic_rst" ::: Reset logicDom ->
  "rx_clk_enable" ::: Signal rxDom Bool ->
  "tx_clk_enable" ::: Signal txDom Bool ->
  "rx_mii_select" ::: Signal rxDom Bool ->
  "tx_mii_select" ::: Signal txDom Bool ->
  "rx_axis_tready" ::: Signal logicDom Bool ->
  "tx_axis_tdata" ::: Signal logicDom (Vec 1 (Unsigned 8)) ->
  "tx_axis_tlast" ::: Signal logicDom Bool ->
  "tx_axis_tuser" ::: Signal logicDom Bool ->
  "tx_axis_tvalid" ::: Signal logicDom Bool ->
  "gmii_rxd" ::: Signal rxDom (BitVector 8) ->
  "gmii_rx_dv" ::: Signal rxDom Bool ->
  "gmii_rx_er" ::: Signal rxDom Bool ->
  EthMacOutput txDom logicDom 1
ethMac1G# !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ = deepErrorX ""

{-# NOINLINE ethMac1G# #-}
{-# ANN ethMac1G# (
    let
      primName = 'ethMac1G#
      ( _
       :> _
       :> _
       :> enable_padding
       :> min_frame_length
       :> tx_fifo_depth
       :> tx_frame_fifo
       :> rx_fifo_depth
       :> rx_frame_fifo
       :> ifg_delay
       :> rx_clk
       :> rx_rst
       :> tx_clk
       :> tx_rst
       :> logic_clk
       :> logic_rst
       :> rx_clk_enable
       :> tx_clk_enable
       :> rx_mii_select
       :> tx_mii_select
       :> rx_axis_tready
       :> tx_axis_tdata
       :> tx_axis_tlast
       :> tx_axis_tuser
       :> tx_axis_tvalid
       :> gmii_rxd
       :> gmii_rx_dv
       :> gmii_rx_er
       :> tx_axis_tready
       :> rx_axis_tdata
       :> rx_axis_tvalid
       :> rx_axis_tlast
       :> rx_axis_tuser
       :> gmii_txd
       :> gmii_tx_en
       :> gmii_tx_er
       :> tx_fifo_overflow
       :> tx_fifo_bad_frame
       :> tx_fifo_good_frame
       :> rx_error_bad_frame
       :> rx_error_bad_fcs
       :> rx_fifo_overflow
       :> rx_fifo_bad_frame
       :> rx_fifo_good_frame
       :> mac
       :> Nil
       ) = indicesI @45

    in
      InlineYamlPrimitive [Verilog] [__i|
  BlackBox:
    name: #{primName}
    kind: Declaration
    template: |-
      // vexRiscv begin

      wire ~GENSYM[tx_axis_tready][#{tx_axis_tready}];
      wire [7:0] ~GENSYM[tr_axis_tdata][#{rx_axis_tdata}];
      wire ~GENSYM[rx_axis_tvalid][#{rx_axis_tvalid}];
      wire ~GENSYM[rx_axis_tlast][#{rx_axis_tlast}];
      wire ~GENSYM[rx_axis_tuser][#{rx_axis_tuser}];

      wire [7:0] ~GENSYM[gmii_txd][#{gmii_txd}];
      wire ~GENSYM[gmii_tx_en][#{gmii_tx_en}];
      wire ~GENSYM[gmii_tx_er][#{gmii_tx_er}];

      wire ~GENSYM[tx_fifo_overflow][#{tx_fifo_overflow}];
      wire ~GENSYM[tx_fifo_bad_frame][#{tx_fifo_bad_frame}];
      wire ~GENSYM[tx_fifo_good_frame][#{tx_fifo_good_frame}];
      wire ~GENSYM[rx_error_bad_frame][#{rx_error_bad_frame}];
      wire ~GENSYM[rx_error_bad_fcs][#{rx_error_bad_fcs}];
      wire ~GENSYM[rx_fifo_overflow][#{rx_fifo_overflow}];
      wire ~GENSYM[rx_fifo_bad_frame][#{rx_fifo_bad_frame}];
      wire ~GENSYM[rx_fifo_good_frame][#{rx_fifo_good_frame}];

      eth_mac_1g_fifo \#(
          .ENABLE_PADDING(~LIT[#{enable_padding}])
        , .MIN_FRAME_LENGTH(~LIT[#{min_frame_length}])
        , .TX_FIFO_DEPTH(~LIT[#{tx_fifo_depth}])
        , .TX_FRAME_FIFO(~LIT[#{tx_frame_fifo}])
        , .RX_FIFO_DEPTH(~LIT[#{rx_fifo_depth}])
        , .RX_FRAME_FIFO(~LIT[#{rx_frame_fifo}])
      )
      ~GENSYM[mac][#{mac}] (
          .rx_clk(~ARG[#{rx_clk}])
        , .rx_rst(~ARG[#{rx_rst}])
        , .tx_clk(~ARG[#{tx_clk}])
        , .tx_rst(~ARG[#{tx_rst}])
        , .logic_clk(~ARG[#{logic_clk}])
        , .logic_rst(~ARG[#{logic_rst}])
        , .tx_axis_tdata(~ARG[#{tx_axis_tdata}])
        , .tx_axis_tvalid(~ARG[#{tx_axis_tvalid}])
        , .tx_axis_tready(~SYM[#{tx_axis_tready}])
        , .tx_axis_tlast(~ARG[#{tx_axis_tlast}])
        , .tx_axis_tuser(~ARG[#{tx_axis_tuser}])
        , .rx_axis_tdata(~SYM[#{rx_axis_tdata}])
        , .rx_axis_tvalid(~SYM[#{rx_axis_tvalid}])
        , .rx_axis_tready(~ARG[#{rx_axis_tready}])
        , .rx_axis_tlast(~SYM[#{rx_axis_tlast}])
        , .rx_axis_tuser(~SYM[#{rx_axis_tuser}])
        , .gmii_rxd(~ARG[#{gmii_rxd}])
        , .gmii_rx_dv(~ARG[#{gmii_rx_dv}])
        , .gmii_rx_er(~ARG[#{gmii_rx_er}])
        , .gmii_txd(~SYM[#{gmii_txd}])
        , .gmii_tx_en(~SYM[#{gmii_tx_en}])
        , .gmii_tx_er(~SYM[#{gmii_tx_er}])
        , .rx_clk_enable(~ARG[#{rx_clk_enable}])
        , .tx_clk_enable(~ARG[#{tx_clk_enable}])
        , .rx_mii_select(~ARG[#{rx_mii_select}])
        , .tx_mii_select(~ARG[#{tx_mii_select}])
        , .tx_fifo_overflow(~SYM[#{tx_fifo_overflow}])
        , .tx_fifo_bad_frame(~SYM[#{tx_fifo_bad_frame}])
        , .tx_fifo_good_frame(~SYM[#{tx_fifo_good_frame}])
        , .rx_error_bad_frame(~SYM[#{rx_error_bad_frame}])
        , .rx_error_bad_fcs(~SYM[#{rx_error_bad_fcs}])
        , .rx_fifo_overflow(~SYM[#{rx_fifo_overflow}])
        , .rx_fifo_bad_frame(~SYM[#{rx_fifo_bad_frame}])
        , .rx_fifo_good_frame(~SYM[#{rx_fifo_good_frame}])
        , .ifg_delay(~ARG[#{ifg_delay}])
      );
      assign ~RESULT = {
        ~SYM[#{tx_axis_tready}],
        ~SYM[#{rx_axis_tdata}],
        ~SYM[#{rx_axis_tvalid}],
        ~SYM[#{rx_axis_tlast}],
        ~SYM[#{rx_axis_tuser}],
        ~SYM[#{gmii_txd}],
        ~SYM[#{gmii_tx_en}],
        ~SYM[#{gmii_tx_er}],
        ~SYM[#{tx_fifo_overflow}],
        ~SYM[#{tx_fifo_bad_frame}],
        ~SYM[#{tx_fifo_good_frame}],
        ~SYM[#{rx_error_bad_frame}],
        ~SYM[#{rx_error_bad_fcs}],
        ~SYM[#{rx_fifo_overflow}],
        ~SYM[#{rx_fifo_bad_frame}],
        ~SYM[#{rx_fifo_good_frame}]
      };

      // vexRiscv end

    |] ) #-}

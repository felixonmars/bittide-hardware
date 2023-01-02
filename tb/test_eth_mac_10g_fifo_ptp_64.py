#!/usr/bin/env python
"""

Copyright (c) 2019 Alex Forencich

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

"""

from myhdl import *
import os

import axis_ep
import eth_ep
import xgmii_ep
import ptp

module = 'eth_mac_10g_fifo'
testbench = 'test_%s_ptp_64' % module

srcs = []

srcs.append("../rtl/%s.v" % module)
srcs.append("../rtl/lfsr.v")
srcs.append("../rtl/axis_xgmii_rx_64.v")
srcs.append("../rtl/axis_xgmii_tx_64.v")
srcs.append("../rtl/eth_mac_10g.v")
srcs.append("../rtl/ptp_clock_cdc.v")
srcs.append("../rtl/ptp_tag_insert.v")
srcs.append("../rtl/ptp_ts_extract.v")
srcs.append("../lib/axis/rtl/axis_async_fifo.v")
srcs.append("../lib/axis/rtl/axis_async_fifo_adapter.v")
srcs.append("../lib/axis/rtl/axis_fifo.v")
srcs.append("%s.v" % testbench)

src = ' '.join(srcs)

build_cmd = "iverilog -o %s.vvp %s" % (testbench, src)

def bench():

    # Parameters
    DATA_WIDTH = 64
    CTRL_WIDTH = (DATA_WIDTH/8)
    AXIS_DATA_WIDTH = DATA_WIDTH
    AXIS_KEEP_ENABLE = (AXIS_DATA_WIDTH>8)
    AXIS_KEEP_WIDTH = (AXIS_DATA_WIDTH/8)
    ENABLE_PADDING = 1
    ENABLE_DIC = 1
    MIN_FRAME_LENGTH = 64
    TX_FIFO_DEPTH = 4096
    TX_FIFO_PIPELINE_OUTPUT = 2
    TX_FRAME_FIFO = 1
    TX_DROP_BAD_FRAME = TX_FRAME_FIFO
    TX_DROP_WHEN_FULL = 0
    RX_FIFO_DEPTH = 4096
    RX_FIFO_PIPELINE_OUTPUT = 2
    RX_FRAME_FIFO = 1
    RX_DROP_BAD_FRAME = RX_FRAME_FIFO
    RX_DROP_WHEN_FULL = RX_FRAME_FIFO
    LOGIC_PTP_PERIOD_NS = 0x6
    LOGIC_PTP_PERIOD_FNS = 0x6666
    PTP_PERIOD_NS = 0x6
    PTP_PERIOD_FNS = 0x6666
    PTP_USE_SAMPLE_CLOCK = 0
    TX_PTP_TS_ENABLE = 1
    RX_PTP_TS_ENABLE = 1
    TX_PTP_TS_FIFO_DEPTH = 64
    RX_PTP_TS_FIFO_DEPTH = 64
    PTP_TS_WIDTH = 96
    TX_PTP_TAG_ENABLE = 1
    PTP_TAG_WIDTH = 16

    # Inputs
    clk = Signal(bool(0))
    rst = Signal(bool(0))
    current_test = Signal(intbv(0)[8:])

    rx_clk = Signal(bool(0))
    rx_rst = Signal(bool(0))
    tx_clk = Signal(bool(0))
    tx_rst = Signal(bool(0))
    logic_clk = Signal(bool(0))
    logic_rst = Signal(bool(0))
    ptp_sample_clk = Signal(bool(0))
    tx_axis_tdata = Signal(intbv(0)[AXIS_DATA_WIDTH:])
    tx_axis_tkeep = Signal(intbv(0)[AXIS_KEEP_WIDTH:])
    tx_axis_tvalid = Signal(bool(0))
    tx_axis_tlast = Signal(bool(0))
    tx_axis_tuser = Signal(bool(0))
    s_axis_tx_ptp_ts_tag = Signal(intbv(0)[PTP_TAG_WIDTH:])
    s_axis_tx_ptp_ts_valid = Signal(bool(0))
    m_axis_tx_ptp_ts_ready = Signal(bool(0))
    rx_axis_tready = Signal(bool(0))
    m_axis_rx_ptp_ts_ready = Signal(bool(0))
    xgmii_rxd = Signal(intbv(0x0707070707070707)[DATA_WIDTH:])
    xgmii_rxc = Signal(intbv(0xff)[CTRL_WIDTH:])
    ptp_ts_96 = Signal(intbv(0)[PTP_TS_WIDTH:])
    ifg_delay = Signal(intbv(0)[8:])

    # Outputs
    tx_axis_tready = Signal(bool(0))
    s_axis_tx_ptp_ts_ready = Signal(bool(1))
    m_axis_tx_ptp_ts_96 = Signal(intbv(0)[PTP_TS_WIDTH:])
    m_axis_tx_ptp_ts_tag = Signal(intbv(0)[PTP_TAG_WIDTH:])
    m_axis_tx_ptp_ts_valid = Signal(bool(0))
    rx_axis_tdata = Signal(intbv(0)[AXIS_DATA_WIDTH:])
    rx_axis_tkeep = Signal(intbv(0)[AXIS_KEEP_WIDTH:])
    rx_axis_tvalid = Signal(bool(0))
    rx_axis_tlast = Signal(bool(0))
    rx_axis_tuser = Signal(bool(0))
    m_axis_rx_ptp_ts_96 = Signal(intbv(0)[PTP_TS_WIDTH:])
    m_axis_rx_ptp_ts_valid = Signal(bool(0))
    xgmii_txd = Signal(intbv(0x0707070707070707)[DATA_WIDTH:])
    xgmii_txc = Signal(intbv(0xff)[CTRL_WIDTH:])
    tx_error_underflow = Signal(bool(0))
    tx_fifo_overflow = Signal(bool(0))
    tx_fifo_bad_frame = Signal(bool(0))
    tx_fifo_good_frame = Signal(bool(0))
    rx_error_bad_frame = Signal(bool(0))
    rx_error_bad_fcs = Signal(bool(0))
    rx_fifo_overflow = Signal(bool(0))
    rx_fifo_bad_frame = Signal(bool(0))
    rx_fifo_good_frame = Signal(bool(0))

    # sources and sinks
    axis_source_pause = Signal(bool(0))
    axis_sink_pause = Signal(bool(0))

    xgmii_source = xgmii_ep.XGMIISource()

    xgmii_source_logic = xgmii_source.create_logic(
        rx_clk,
        rx_rst,
        txd=xgmii_rxd,
        txc=xgmii_rxc,
        name='xgmii_source'
    )

    xgmii_sink = xgmii_ep.XGMIISink()

    xgmii_sink_logic = xgmii_sink.create_logic(
        tx_clk,
        tx_rst,
        rxd=xgmii_txd,
        rxc=xgmii_txc,
        name='xgmii_sink'
    )

    axis_source = axis_ep.AXIStreamSource()

    axis_source_logic = axis_source.create_logic(
        logic_clk,
        logic_rst,
        tdata=tx_axis_tdata,
        tkeep=tx_axis_tkeep,
        tvalid=tx_axis_tvalid,
        tready=tx_axis_tready,
        tlast=tx_axis_tlast,
        tuser=tx_axis_tuser,
        pause=axis_source_pause,
        name='axis_source'
    )

    axis_sink = axis_ep.AXIStreamSink()

    axis_sink_logic = axis_sink.create_logic(
        logic_clk,
        logic_rst,
        tdata=rx_axis_tdata,
        tkeep=rx_axis_tkeep,
        tvalid=rx_axis_tvalid,
        tready=rx_axis_tready,
        tlast=rx_axis_tlast,
        tuser=rx_axis_tuser,
        pause=axis_sink_pause,
        name='axis_sink'
    )

    tx_ptp_ts_tag_source = axis_ep.AXIStreamSource()

    tx_ptp_ts_tag_source_logic = tx_ptp_ts_tag_source.create_logic(
        logic_clk,
        logic_rst,
        tdata=s_axis_tx_ptp_ts_tag,
        tvalid=s_axis_tx_ptp_ts_valid,
        tready=s_axis_tx_ptp_ts_ready,
        name='tx_ptp_ts_tag_source'
    )

    tx_ptp_ts_sink = axis_ep.AXIStreamSink()

    tx_ptp_ts_sink_logic = tx_ptp_ts_sink.create_logic(
        logic_clk,
        logic_rst,
        tdata=(m_axis_tx_ptp_ts_96, m_axis_tx_ptp_ts_tag),
        tvalid=m_axis_tx_ptp_ts_valid,
        tready=m_axis_tx_ptp_ts_ready,
        name='tx_ptp_ts_sink'
    )

    rx_ptp_ts_sink = axis_ep.AXIStreamSink()

    rx_ptp_ts_sink_logic = rx_ptp_ts_sink.create_logic(
        logic_clk,
        logic_rst,
        tdata=m_axis_rx_ptp_ts_96,
        tvalid=m_axis_rx_ptp_ts_valid,
        tready=m_axis_rx_ptp_ts_ready,
        name='rx_ptp_ts_sink'
    )

    # PTP clock
    ptp_clock = ptp.PtpClock(period_ns=LOGIC_PTP_PERIOD_NS, period_fns=LOGIC_PTP_PERIOD_FNS)

    ptp_logic = ptp_clock.create_logic(
        logic_clk,
        logic_rst,
        ts_64=ptp_ts_96
    )

    # DUT
    if os.system(build_cmd):
        raise Exception("Error running build command")

    dut = Cosimulation(
        "vvp -m myhdl %s.vvp -lxt2" % testbench,
        clk=clk,
        rst=rst,
        current_test=current_test,

        rx_clk=rx_clk,
        rx_rst=rx_rst,
        tx_clk=tx_clk,
        tx_rst=tx_rst,
        logic_clk=logic_clk,
        logic_rst=logic_rst,
        ptp_sample_clk=ptp_sample_clk,

        tx_axis_tdata=tx_axis_tdata,
        tx_axis_tkeep=tx_axis_tkeep,
        tx_axis_tvalid=tx_axis_tvalid,
        tx_axis_tready=tx_axis_tready,
        tx_axis_tlast=tx_axis_tlast,
        tx_axis_tuser=tx_axis_tuser,

        s_axis_tx_ptp_ts_tag=s_axis_tx_ptp_ts_tag,
        s_axis_tx_ptp_ts_valid=s_axis_tx_ptp_ts_valid,
        s_axis_tx_ptp_ts_ready=s_axis_tx_ptp_ts_ready,

        m_axis_tx_ptp_ts_96=m_axis_tx_ptp_ts_96,
        m_axis_tx_ptp_ts_tag=m_axis_tx_ptp_ts_tag,
        m_axis_tx_ptp_ts_valid=m_axis_tx_ptp_ts_valid,
        m_axis_tx_ptp_ts_ready=m_axis_tx_ptp_ts_ready,

        rx_axis_tdata=rx_axis_tdata,
        rx_axis_tkeep=rx_axis_tkeep,
        rx_axis_tvalid=rx_axis_tvalid,
        rx_axis_tready=rx_axis_tready,
        rx_axis_tlast=rx_axis_tlast,
        rx_axis_tuser=rx_axis_tuser,

        m_axis_rx_ptp_ts_96=m_axis_rx_ptp_ts_96,
        m_axis_rx_ptp_ts_valid=m_axis_rx_ptp_ts_valid,
        m_axis_rx_ptp_ts_ready=m_axis_rx_ptp_ts_ready,

        xgmii_rxd=xgmii_rxd,
        xgmii_rxc=xgmii_rxc,

        xgmii_txd=xgmii_txd,
        xgmii_txc=xgmii_txc,

        tx_error_underflow=tx_error_underflow,
        tx_fifo_overflow=tx_fifo_overflow,
        tx_fifo_bad_frame=tx_fifo_bad_frame,
        tx_fifo_good_frame=tx_fifo_good_frame,
        rx_error_bad_frame=rx_error_bad_frame,
        rx_error_bad_fcs=rx_error_bad_fcs,
        rx_fifo_overflow=rx_fifo_overflow,
        rx_fifo_bad_frame=rx_fifo_bad_frame,
        rx_fifo_good_frame=rx_fifo_good_frame,

        ptp_ts_96=ptp_ts_96,

        ifg_delay=ifg_delay
    )

    @always(delay(4))
    def clkgen():
        clk.next = not clk
        tx_clk.next = not tx_clk
        rx_clk.next = not rx_clk
        logic_clk.next = not logic_clk
        ptp_sample_clk.next = not ptp_sample_clk

    @instance
    def check():
        yield delay(100)
        yield clk.posedge
        rst.next = 1
        tx_rst.next = 1
        rx_rst.next = 1
        logic_rst.next = 1
        yield clk.posedge
        rst.next = 0
        tx_rst.next = 0
        rx_rst.next = 0
        logic_rst.next = 0
        yield clk.posedge
        yield delay(100)
        yield clk.posedge

        ifg_delay.next = 12

        # testbench stimulus

        yield clk.posedge
        print("test 1: test rx packet")
        current_test.next = 1

        test_frame = eth_ep.EthFrame()
        test_frame.eth_dest_mac = 0xDAD1D2D3D4D5
        test_frame.eth_src_mac = 0x5A5152535455
        test_frame.eth_type = 0x8000
        test_frame.payload = bytearray(range(32))
        test_frame.update_fcs()

        axis_frame = test_frame.build_axis_fcs()

        xgmii_source.send(b'\x55\x55\x55\x55\x55\x55\x55\xD5'+bytearray(axis_frame))

        yield axis_sink.wait()
        rx_frame = axis_sink.recv()

        eth_frame = eth_ep.EthFrame()
        eth_frame.parse_axis(rx_frame)
        eth_frame.update_fcs()

        assert eth_frame == test_frame

        yield delay(100)

        yield clk.posedge
        print("test 2: test tx packet")
        current_test.next = 2

        test_frame = eth_ep.EthFrame()
        test_frame.eth_dest_mac = 0xDAD1D2D3D4D5
        test_frame.eth_src_mac = 0x5A5152535455
        test_frame.eth_type = 0x8000
        test_frame.payload = bytearray(range(32))
        test_frame.update_fcs()

        axis_frame = test_frame.build_axis()

        tx_ptp_ts_tag_source.send([1234])
        axis_source.send(axis_frame)

        yield xgmii_sink.wait()
        rx_frame = xgmii_sink.recv()

        assert rx_frame.data[0:8] == bytearray(b'\x55\x55\x55\x55\x55\x55\x55\xD5')

        eth_frame = eth_ep.EthFrame()
        eth_frame.parse_axis_fcs(rx_frame.data[8:])

        print(hex(eth_frame.eth_fcs))
        print(hex(eth_frame.calc_fcs()))

        assert len(eth_frame.payload.data) == 46
        assert eth_frame.eth_fcs == eth_frame.calc_fcs()
        assert eth_frame.eth_dest_mac == test_frame.eth_dest_mac
        assert eth_frame.eth_src_mac == test_frame.eth_src_mac
        assert eth_frame.eth_type == test_frame.eth_type
        assert eth_frame.payload.data.index(test_frame.payload.data) == 0

        yield delay(100)

        raise StopSimulation

    return instances()

def test_bench():
    sim = Simulation(bench())
    sim.run()

if __name__ == '__main__':
    print("Running test...")
    test_bench()

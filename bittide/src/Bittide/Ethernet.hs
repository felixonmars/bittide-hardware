{-# LANGUAGE RecordWildCards #-}
module Bittide.Ethernet where

import Bittide.Ethernet.Types
import Clash.Annotations.TH (makeTopEntity)
import Clash.Explicit.Prelude
import Bittide.DoubleBufferedRam
import Bittide.Axi4
import Clash.Prelude(withClockResetEnable)

data ClientState = ClientState
  { serverMac :: MacAddress
  , serverIp :: IpAddress
  , serverPort :: Port
  , fpgaPort :: Port
  } deriving (Generic, NFDataX)

udpClient ::
  "clk" ::: Clock System ->
  "rst" ::: Reset System ->
  "ena" ::: Enable System ->
  "fpgaMac" ::: Signal System MacAddress ->
  "fpgaIp" ::: Signal System IpAddress ->
  "incomingUdp" ::: Signal System UdpFrame ->
  "outgoingUdp_axisPayload_axisReady" ::: Signal System Bool ->
  "outgoingUdp_headerReady" ::: Signal System Bool ->
  "" :::
    ( "outgoingUdp" :::  Signal System UdpFrame
    , "incomingUdp_headerReady" ::: Signal System Bool
    , "incomingUdp_axisPayload_axisReady" ::: Signal System Bool
    , "leds" ::: Signal System (BitVector 8)
    )
udpClient clk rst ena fpgaMac fpgaIp incomingUdp txAxisReady txHeaderReady =
  (udpOut, rxHeaderReady, rxAxisdReady, leds)
  where
  (udpOut, rxHeaderReady, rxAxisdReady, axisM2S) = mealyB clk rst ena go initState
    (fpgaMac, fpgaIp, incomingUdp, txAxisReady, txHeaderReady, axisS2M)

  (leds, wbS2M) = withClockResetEnable clk rst ena $
    registerWb @_ @_ @4 @32 WishbonePriority (0 :: BitVector 8) wbM2S (pure Nothing)
  (axisS2M, wbM2S) = withClockResetEnable clk rst ena $
    axisToWishbone @_ @8 axisM2S wbS2M


  initState = ClientState
    { serverMac = deepErrorX "Initial server MAC undefined."
    , serverIp = deepErrorX "Initial server IP undefined."
    , serverPort = 20001
    , fpgaPort = 1234
    }

  go ClientState{..} (mac, ip, udpIn@UdpFrame{..}, axisOutReady, outgoingHeaderReady, ReducedAxiStreamS2M internalAxisReady) =
    (nextState, (udpOutGo, consumeIncomingHeader, consumeIncomingAxis, internalAxisM2S))
   where
    consumeIncomingAxis = (axisOutReady && internalAxisReady) || not portsMatch
    consumeIncomingHeader = outgoingHeaderReady || not portsMatch
    udpOutGo
      | portsMatch = (udpSetSource mac ip fpgaPort $ (udpLoopback udpIn){ipHeader = (ipLoopback ipHeader){ipTtl = 255}})
        {headerValid = headerValid && portsMatch}
      | otherwise = udpOutgoingIdle{ipHeader = (ipLoopback ipHeader){ipTtl = 255}}


    isBroadcast = slice d7 d0 (ipDestIp ipHeader) == maxBound
    portsMatch = udpSourcePort udpHeader == serverPort && udpDestPort udpHeader == fpgaPort

    (nextServerMac, nextServerIp)
      | headerValid && isBroadcast && portsMatch = (ethSourceMac ethHeader, ipDestIp ipHeader)
      | otherwise = (serverMac, serverIp)

    nextState = ClientState
      { serverMac = nextServerMac
      , serverIp = nextServerIp
      , serverPort = serverPort
      , fpgaPort = fpgaPort }

    internalAxisM2S = axisPayload
      { axisUser = axisUser axisPayload && portsMatch
      , axisValid = axisValid axisPayload && portsMatch && axisOutReady && internalAxisReady && not isBroadcast}
makeTopEntity 'udpClient

{-# LANGUAGE RecordWildCards #-}
module Bittide.Ethernet.Types where

import Clash.Prelude
import Bittide.Axi4

type MacAddress = BitVector 48
type IpAddress = (BitVector 8,BitVector 8,BitVector 8,BitVector 8)
type Port = BitVector 16

axisIdle :: KnownNat n => ReducedAxiStreamM2S n
axisIdle = ReducedAxiStreamM2S
  { axisData = deepErrorX "Axi stream idle data undefined"
  , axisValid = False
  , axisLast = False
  , axisUser = False
  }

data IpHeader = IpHeader
  { ipVersion        :: "ipVersion"        ::: BitVector 4
  , ipIhl            :: "ipIhl"            ::: BitVector 4
  , ipDscp           :: "ipDscp"           ::: BitVector 6
  , ipEcn            :: "ipEcn"            ::: BitVector 2
  , ipLength         :: "ipLength"         ::: BitVector 16
  , ipIdentification :: "ipIdentification" ::: BitVector 16
  , ipFlags          :: "ipFlags"          ::: BitVector 3
  , ipFragmentOffset :: "ipFragmentOffset" ::: BitVector 13
  , ipTtl            :: "ipTtl"            ::: BitVector 8
  , ipProtocol       :: "ipProtocol"       ::: BitVector 8
  , ipHeaderChecksum :: "ipHeaderChecksum" ::: BitVector 16
  , ipSourceIp       :: "ipSourceIp"       ::: IpAddress
  , ipDestIp         :: "ipDestIp"         ::: IpAddress
  } deriving (Generic, NFDataX)

ipLoopback :: IpHeader -> IpHeader
ipLoopback hdr@IpHeader{..} = hdr
  { ipDestIp          = ipSourceIp
  , ipSourceIp        = ipDestIp
  , ipTtl             = 64
  , ipDscp            = 0
  , ipEcn             = 0
  }

data EthernetHeader = EthernetHeader
  { ethDestMac :: "ethDestMac"     ::: MacAddress
  , ethSourceMac :: "ethSourceMac" ::: MacAddress
  , ethType :: "ethType"           ::: BitVector 16
  } deriving (Generic, NFDataX)

ethernetLoopback :: EthernetHeader -> EthernetHeader
ethernetLoopback eth@EthernetHeader{..} = eth
  { ethDestMac = ethSourceMac
  , ethSourceMac = ethDestMac
  }

data UdpHeader = UdpHeader
  { udpSourcePort :: "udpSourcePort" ::: Port
  , udpDestPort   :: "udpDestPort" ::: Port
  , udpLength     :: "udpLength" ::: BitVector 16
  , udpChecksum   :: "udpChecksum" ::: BitVector 16
  } deriving (Generic, NFDataX)

data UdpFrame = UdpFrame
  { ipHeader  :: "ipHeader" :::  IpHeader
  , ethHeader ::  "ethHeader" ::: EthernetHeader
  , headerValid  :: "headerValid" :::  Bool
  , udpHeader ::  "udpHeader" ::: UdpHeader
  , axisPayload ::  "axisPayload" ::: ReducedAxiStreamM2S 8
  } deriving (Generic, NFDataX)

udpOutgoingIdle :: UdpFrame
udpOutgoingIdle = (deepErrorX "Outgoing data undefined")
  { headerValid = False
  , axisPayload = axisIdle
  }

udpLoopback :: UdpFrame -> UdpFrame
udpLoopback frame@UdpFrame{..} = frame
  { ipHeader = ipLoopback ipHeader
  , ethHeader = ethernetLoopback ethHeader
  , udpHeader = UdpHeader
    { udpSourcePort = udpDestPort udpHeader
    , udpDestPort = udpSourcePort udpHeader
    , udpLength = udpLength udpHeader
    , udpChecksum = 0
    }
  }

udpSetSource :: MacAddress -> IpAddress -> Port -> UdpFrame -> UdpFrame
udpSetSource mac ip port udpOut@UdpFrame{..} = udpOut
  { ipHeader = ipHeader {ipSourceIp = ip}
  , ethHeader = ethHeader{ethSourceMac = mac}
  , udpHeader = udpHeader{udpSourcePort = port}
  }

udpSetDest :: MacAddress -> IpAddress -> Port -> UdpFrame -> UdpFrame
udpSetDest mac ip port udpOut@UdpFrame{..} = udpOut
  { ipHeader = ipHeader {ipDestIp = ip}
  , ethHeader = ethHeader{ethDestMac = mac}
  , udpHeader = udpHeader{udpDestPort = port}
  }

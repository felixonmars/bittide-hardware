module Bittide.Instances.Ethernet where

import Clash.Prelude

import Clash.Annotations.TH
import Bittide.Instances.Domains

import qualified Bittide.Ethernet.Mac as Mac

ethMac1G = Mac.ethMac1G @Basic200 @Basic125A @Basic125B def

makeTopEntity 'ethMac1G

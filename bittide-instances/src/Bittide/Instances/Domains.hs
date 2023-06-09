-- SPDX-FileCopyrightText: 2022-2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-orphans #-}

module Bittide.Instances.Domains where

import Clash.Explicit.Prelude

createDomain vXilinxSystem{vName="Basic125", vPeriod= hzToPeriod 125e6}
createDomain vXilinxSystem{vName="Basic199", vPeriod=hzToPeriod 199e6}
createDomain vXilinxSystem{vName="Basic200", vPeriod=hzToPeriod 200e6}
createDomain vXilinxSystem{vName="Basic200A", vPeriod=hzToPeriod 200e6}
createDomain vXilinxSystem{vName="Basic200B", vPeriod=hzToPeriod 200e6}
createDomain vXilinxSystem{vName="Internal", vPeriod=hzToPeriod 200e6}
createDomain vXilinxSystem{vName="External", vPeriod=hzToPeriod 200e6}
createDomain vXilinxSystem{vName="Basic300", vPeriod=hzToPeriod 300e6}

createDomain vXilinxSystem{vName="TxS", vPeriod=hzToPeriod 10e9}  -- used for signals sent by the GTH
createDomain vXilinxSystem{vName="RxS", vPeriod=hzToPeriod 10e9}  -- used for signals received by the GTH
createDomain vXilinxSystem{vName="TxUser2", vPeriod=hzToPeriod 125e6}
createDomain vXilinxSystem{vName="RxUser2", vPeriod=hzToPeriod 125e6}

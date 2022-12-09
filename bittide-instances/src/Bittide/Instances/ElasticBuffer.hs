-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-orphans #-}

module Bittide.Instances.ElasticBuffer where

import Clash.Prelude
import Clash.Annotations.TH

import Bittide.ElasticBuffer

createDomain vXilinxSystem{vPeriod=hzToPeriod 201e6, vName="Fast"}
createDomain vXilinxSystem{vPeriod=hzToPeriod 199e6, vName="Slow"}

elasticBuffer5 ::
  "clkReadFast" ::: Clock Fast ->
  "clkWriteSlow" :::Clock Slow ->
  "resetRead" ::: Reset Fast ->
  ( "dataCount" ::: Signal Fast (Unsigned 5)
  , "underflow" ::: Signal Fast Underflow
  , "overrflow" ::: Signal Fast Overflow
  , "ebMode" ::: Signal Fast EbMode
  )
elasticBuffer5 = resettableXilinxElasticBuffer

makeTopEntity 'elasticBuffer5

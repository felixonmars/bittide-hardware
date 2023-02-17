-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

import           Prelude
import           Test.Tasty                                   (defaultMain,
                                                               testGroup)
import qualified Tests.ContranomySim.Print

import           Tests.ContranomySim.FirmwareIntegrationTests (generateTests)

main :: IO ()
main = do

  integ <- generateTests "firmware-integration-tests/"

  let tests  = testGroup "ContranomySim Tests"
                [ Tests.ContranomySim.Print.tests
                , integ
                ]
  defaultMain tests

{-|
Copyright  :  (C) 2022, Google LLC
License    :  Apache-2.0
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

import           Prelude
import           Test.Tasty                                   (defaultMain,
                                                               testGroup)
import qualified Tests.ContranomySim.Print
import qualified Tests.ContranomySim.ReadElf

import           Tests.ContranomySim.FirmwareIntegrationTests (generateTests)

main :: IO ()
main = do

  integ <- generateTests "firmware-integration-tests/"

  let tests  = testGroup "ContranomySim Tests"
                [ Tests.ContranomySim.Print.tests
                , Tests.ContranomySim.ReadElf.tests
                , integ
                ]
  defaultMain tests

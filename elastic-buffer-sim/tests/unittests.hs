module Main where

import Prelude
import Test.Tasty

import qualified Tests.Bittide.Simulate

tests :: TestTree
tests =
  testGroup "Tests"
    [ testGroup "Bittide"
      [ Tests.Bittide.Simulate.tests
      ]
    ]

main :: IO ()
main = defaultMain tests

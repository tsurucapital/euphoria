module Main where

import Test.Framework (defaultMain)

import qualified Test.Collection.Enum
import qualified Test.Collection.Hashable
import Test.Event (eventTestGroup)
import Test.Update (updateTestGroup)

main :: IO ()
main = defaultMain
    [ Test.Collection.Enum.tests
    , Test.Collection.Hashable.tests
    , eventTestGroup
    , updateTestGroup
    ]

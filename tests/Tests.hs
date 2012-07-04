module Main where

import Test.Framework (defaultMain)

import Test.Collection (collectionTestGroup)
import Test.Event (eventTestGroup)
import Test.Update (updateTestGroup)

main :: IO ()
main = defaultMain
    [ collectionTestGroup
    , eventTestGroup
    , updateTestGroup
    ]

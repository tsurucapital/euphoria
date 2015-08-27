module Main where

import Criterion.Main (defaultMain)

import qualified FRP.Euphoria.Collection.Bench     as Collection

main :: IO ()
main = defaultMain
    [ Collection.benchmarks
    ]

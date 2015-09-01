module Main where

import Criterion.Main (defaultMain)

import qualified FRP.Euphoria.EnumCollection.Lazy.Bench as ECL
import qualified FRP.Euphoria.EnumCollection.Strict.Bench as ECS
import qualified FRP.Euphoria.HashCollection.Lazy.Bench as HCL
import qualified FRP.Euphoria.HashCollection.Strict.Bench as HCS

main :: IO ()
main = defaultMain
    [ ECL.benchmarks
    , ECS.benchmarks
    , HCL.benchmarks
    , HCS.benchmarks
    ]

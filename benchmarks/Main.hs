module Main where

import Criterion.Main (defaultMain)

import qualified FRP.Euphoria.Collection.Enum.Bench     as CollectionEnum
import qualified FRP.Euphoria.Collection.Hashable.Bench as CollectionHashable

main :: IO ()
main = defaultMain
    [ CollectionEnum.benchmarks
    , CollectionHashable.benchmarks
    ]

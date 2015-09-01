module Main where

import Test.Framework (defaultMain)

import qualified FRP.Euphoria.EnumCollection.Lazy.Test
import qualified FRP.Euphoria.HashCollection.Strict.Test
import qualified FRP.Euphoria.Event.Test
import qualified FRP.Euphoria.Update.Test

main :: IO ()
main = defaultMain
    [ FRP.Euphoria.EnumCollection.Lazy.Test.tests
    , FRP.Euphoria.HashCollection.Strict.Test.tests
    , FRP.Euphoria.Event.Test.tests
    , FRP.Euphoria.Update.Test.tests
    ]

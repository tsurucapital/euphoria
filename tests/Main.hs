module Main where

import qualified FRP.Euphoria.Event.Test
import qualified FRP.Euphoria.Update.Test
import qualified FRP.Euphoria.HashCollection.Strict.Test
import qualified FRP.Euphoria.EnumCollection.Lazy.Test

import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "FRP.Euphoria"
    [ FRP.Euphoria.Event.Test.tests
    , FRP.Euphoria.Update.Test.tests
    , FRP.Euphoria.HashCollection.Strict.Test.tests
    , FRP.Euphoria.EnumCollection.Lazy.Test.tests
    ]

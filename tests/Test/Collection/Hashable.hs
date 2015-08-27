{-# LANGUAGE TemplateHaskell #-}
module Test.Collection.Hashable
    ( tests
    ) where

import Test.Framework (Test)
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import FRP.Euphoria.Event
import FRP.Euphoria.Collection.Hashable

tests :: Test
tests = $(testGroupGenerator)

case_switchCollection :: Assertion
case_switchCollection = do
    result <- networkToList 5 $ do
        col0 <- collectionFromDiscreteList (0::Int) =<< mkD [[10::Int], [], [10,20,30], [20,30], [30]]
        col1 <- collectionFromDiscreteList 0 =<< mkD [[11], [], [11,21,31], [21,31], [31]]
        col2 <- collectionFromDiscreteList 0 =<< mkD [[12], [], [12,22,32], [22,32], [32]]
        colD <- stepperD col0 =<< eventFromList [[], [], [col1], [], [col2]]
        col <- switchD colD
        (_, updates) <- openCollection col
        listS <- discreteToSignal $ collectionToDiscreteList col
        return $ (,) <$> listS <*> eventToSignal updates
    result @?=
        [ ([(0, 10)]
            , [])
        , ([]
            , [RemoveItem 0])
        , ([(1, 11), (2, 21), (3, 31)]
            , [AddItem 1 11, AddItem 2 21, AddItem 3 31])
        , ([(2, 21), (3, 31)]
            , [RemoveItem 1])
        , ([(3, 32)]
            , [RemoveItem 2, RemoveItem 3, AddItem 3 32])
        ]
    where
        mkD list = signalToDiscrete <$> signalFromList list

case_mapCollection :: Assertion
case_mapCollection = do
    result <- networkToList 1 $ do
        col <- mapCollection show $
            collectionFromList [(0 :: Int, 1 :: Int), (1, 2), (2, 3)]
        discreteToSignal $ collectionToDiscreteList col

    result @?= [[(0, "1"), (1, "2"), (2, "3")]]

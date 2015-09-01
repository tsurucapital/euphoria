{-# LANGUAGE TemplateHaskell #-}
module FRP.Euphoria.Event.Test (tests) where
import Control.Applicative ((<$>))
import Data.Monoid (mempty)

import Test.Framework (Test)
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import FRP.Euphoria.Event

tests :: Test
tests = $(testGroupGenerator)

case_takeE :: Assertion
case_takeE = do
    result <- networkToList 5 $ do
        evt <- eventFromList [[1], [1::Int], [2,3], [], [4]]
        evt2 <- takeE 3 evt
        accumS 0 $ (+) <$> evt2
    result @?= [1, 2, 4, 4, 4]

case_takeWhileE :: Assertion
case_takeWhileE = do
    result <- networkToList 5 $ do
        evt <- eventFromList [[1], [1::Int], [2,3], [], [4]]
        evt2 <- takeWhileE (<3) evt
        accumS 0 $ (+) <$> evt2
    result @?= [1, 2, 4, 4, 4]

case_groupE :: Assertion
case_groupE = do
    result <- networkToList 5 $ do
        evt <- eventFromList [[1], [1::Int], [2,3], [], [3,3,4]]
        evt2 <- groupE evt
        threes <- takeE 1 =<< dropE 2 evt2
        dyn <- stepperS mempty threes
        return $ eventToSignal $ joinEventSignal dyn
    result @?= [[], [], [3], [], [3,3]]

case_splitOnE :: Assertion
case_splitOnE = do
    result <- networkToList 5 $ do
        ev1 <- eventFromList [[1::Int], [2,3,4], [], [5,6], [7,8]]
        ev2 <- eventFromList [[], [()], [], [], [()]]
        eventToSignal <$> splitOnE ev2 ev1
    result @?= [[], [[1,2,3,4]], [], [], [[5,6,7,8]]]

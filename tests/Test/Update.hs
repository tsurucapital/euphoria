{-# LANGUAGE TemplateHaskell #-}
module Test.Update (updateTestGroup) where
import Control.Applicative ((<$>), (<*>))
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend)

import Test.Framework (Test)
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import FRP.Euphoria.Event
import FRP.Euphoria.Update

updateTestGroup :: Test
updateTestGroup = $(testGroupGenerator)

case_startUpdateNetwork :: Assertion
case_startUpdateNetwork = do
    (sample, step) <- startUpdateNetwork $ do
        evt <- eventFromList [["a"], ["bc","d"], [], ["e"]]
        return $ updateUseAll evt
    step
    val0 <- sample
    val1 <- sample
    val2 <- sample

    [val0, val1, val2] @?= ["abcd", "", "e"]

case_skip :: Assertion
case_skip = do
    (sample, step) <- startUpdateNetwork $ do
        update <- updateUseLast <$> eventFromList [[1], [2, 3], [], [4::Int]]
        return $ update
    step
    val0 <- sample
    val1 <- sample
    step
    val2 <- sample

    val0 @?= Just 3
    val1 @?= Nothing
    val2 @?= Just 4

case_mappendUpdate :: Assertion
case_mappendUpdate = do
    (sample, step) <- startUpdateNetwork $ do
        update0 <- updateUseAll <$> eventFromList [["a"], ["bc","d"], [], ["e"]]
        update1 <- updateUseAll <$> eventFromList [["f"], [], ["g"], ["hij"]]
        return $ update0 `mappend` update1
    step
    val0 <- sample
    val1 <- sample
    step
    val2 <- sample

    val0 @?= "abcdf"
    val1 @?= "g"
    val2 @?= "ehij"

case_applicativeUpdate :: Assertion
case_applicativeUpdate = do
    (sample, step) <- startUpdateNetwork $ do
        update0 <- updateUseAll <$> eventFromList [["a"], ["bc","d"], [], ["e"]]
        update1 <- updateUseAll <$> eventFromList [[[1]], [], [[2]], [[3,4],[5::Int]]]
        return $ f <$> update0 <*> update1
    step
    val0 <- sample
    val1 <- sample
    step
    val2 <- sample

    val0 @?= [([1], "abcd")]
    val1 @?= [([2], "")]
    val2 @?= [([3,4,5], "e")]
        where
        f str num = [(num, str)]

case_switchUD :: Assertion
case_switchUD = do
    (sample, step) <- startUpdateNetwork $ do
        update0 <- fmap (fromMaybe "") . updateUseLast <$>
            eventFromList [["1"], ["2", "3"], ["4"], ["5"]]
        update1 <- updateUseAll <$> eventFromList [["a"], ["bc","d"], [], ["e"]]
        updatesD <- stepperD update0 =<< eventFromList [[], [], [], [update1]]
        switchD updatesD
    val0 <- sample
    step
    step
    val1 <- sample

    val0 @?= "1"
    val1 @?= "4e"

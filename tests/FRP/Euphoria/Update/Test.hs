module FRP.Euphoria.Update.Test (tests) where

import FRP.Euphoria.Event
import FRP.Euphoria.Update
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "FRP.Euphoria.Update"
    [ testStartUpdateNetwork
    , testSkip
    , testAppend
    , testSwitchD
    ]

testStartUpdateNetwork :: TestTree
testStartUpdateNetwork = testCase "startUpdateNetwork" $ do
    (sample, step) <- startUpdateNetwork $ do
        evt <- eventFromList [["a"], ["bc","d"], [], ["e"]]
        return $ updateUseAll evt
    step
    val0 <- sample
    val1 <- sample
    val2 <- sample

    [val0, val1, val2] @?= [Just "abcd", Nothing, Just "e"]

testSkip :: TestTree
testSkip = testCase "skip" $ do
    (sample, step) <- startUpdateNetwork $
        updateUseLast <$> eventFromList [[1], [2, 3], [], [4::Int]]
    step
    val0 <- sample
    val1 <- sample
    step
    val2 <- sample

    val0 @?= Just 3
    val1 @?= Nothing
    val2 @?= Just 4

testAppend :: TestTree
testAppend = testCase "append" $ do
    (sample, step) <- startUpdateNetwork $ do
        update0 <- updateUseAll <$> eventFromList [["a"], ["bc","d"], [], ["e"]]
        update1 <- updateUseAll <$> eventFromList [["f"], [], ["g"], ["hij"]]
        return $ update0 <> update1
    step
    val0 <- sample
    val1 <- sample
    step
    val2 <- sample

    val0 @?= Just "abcdf"
    val1 @?= Just "g"
    val2 @?= Just "ehij"

testSwitchD :: TestTree
testSwitchD = testCase "switchD" $ do
    (sample, step) <- startUpdateNetwork $ do
        update0 <- updateUseLast <$>
            eventFromList [["1"], ["2", "3"], ["4"], ["5"]]
        update1 <- updateUseAll <$> eventFromList [["a"], ["bc","d"], [], ["e"]]
        updatesD <- stepperD update0 =<< eventFromList [[], [], [], [update1]]
        switchD updatesD
    val0 <- sample
    step
    step
    val1 <- sample

    val0 @?= Just "1"
    val1 @?= Just "4e"

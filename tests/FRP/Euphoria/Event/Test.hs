module FRP.Euphoria.Event.Test (tests) where

import FRP.Euphoria.Event
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "FRP.Euphoria.Event"
    [ testTakeE
    , testTakeWhileE
    , testGroupE
    , testSplitOnE
    , testFreezeD
    ]

testTakeE :: TestTree
testTakeE = testCase "takeE" $ do
    result <- networkToList 5 $ do
        evt <- eventFromList [[1], [1::Int], [2,3], [], [4]]
        evt2 <- takeE 3 evt
        accumS 0 $ (+) <$> evt2
    result @?= [1, 2, 4, 4, 4]

testTakeWhileE :: TestTree
testTakeWhileE = testCase "takeWhileE" $ do
    result <- networkToList 5 $ do
        evt <- eventFromList [[1], [1::Int], [2,3], [], [4]]
        evt2 <- takeWhileE (<3) evt
        accumS 0 $ (+) <$> evt2
    result @?= [1, 2, 4, 4, 4]

testGroupE :: TestTree
testGroupE = testCase "groupE" $ do
    result <- networkToList 5 $ do
        evt <- eventFromList [[1], [1::Int], [2,3], [], [3,3,4]]
        evt2 <- groupE evt
        threes <- takeE 1 =<< dropE 2 evt2
        dyn <- stepperS mempty threes
        return $ eventToSignal $ joinEventSignal dyn
    result @?= [[], [], [3], [], [3,3]]

testSplitOnE :: TestTree
testSplitOnE = testCase "splitOnE" $ do
    result <- networkToList 5 $ do
        ev1 <- eventFromList [[1::Int], [2,3,4], [], [5,6], [7,8]]
        ev2 <- eventFromList [[], [()], [], [], [()]]
        eventToSignal <$> splitOnE ev2 ev1
    result @?= [[], [[1,2,3,4]], [], [], [[5,6,7,8]]]

testFreezeD :: TestTree
testFreezeD = testCase "freezeD" $ do
    result <- networkToList 5 $ do
        ev <- eventFromList [[], [], [()], [], [()]]
        dis <- signalToDiscrete <$> signalFromList [1..]
        discreteToSignal =<< freezeD ev dis
    result @?= [1, 2, 3, 3, 3 :: Int]

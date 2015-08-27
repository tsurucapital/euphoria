{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module FRP.Euphoria.Collection.Bench
    ( benchmarks
    ) where

import Control.DeepSeq
import Control.Monad
import Criterion
import qualified Data.EnumMap.Lazy as EML
import Data.List
import FRP.Euphoria.Event
import FRP.Euphoria.Collection
import GHC.Generics

benchmarks :: Benchmark
benchmarks = env mkEnv $ \ ~e -> bgroup "Collection"
    [ bench "mapToCollection" $ bench_mapToCollection (dict e)
    , bench "accum"           $ bench_accum (updates e)
    , bench "transformAccum"  $ bench_transformAccum (updates e)
    , bench "fromListSmall"   $ bench_fromList (smallList e)
    , bench "transformSmall"  $ bench_transform (smallList e)
    , bench "fromListBig"     $ bench_fromList (bigList e)
    , bench "transformBig"    $ bench_transform (bigList e)
    ]

data BenchmarkEnv = BenchmarkEnv
    { smallList :: [[Int]]
    , bigList   :: [[Int]]
    , updates   :: [CollectionUpdate Int Int]
    , dict      :: EML.EnumMap Int Int
    } deriving (Generic)

instance NFData BenchmarkEnv
instance (NFData k, NFData v) => NFData (CollectionUpdate k v)

mkEnv :: IO BenchmarkEnv
mkEnv = do
    let smallList = map (\i -> [1..i]) [0..10]
    let bigList = map (\i -> [1..i]) [0..100]
    let updates = map (\i -> let (v,k) = 100 `divMod` i in AddItem k v) [1..101]
    let dict = foldl' (\acc i -> let (v,k) = 100 `divMod` i in EML.insert k v acc) EML.empty [1..101]
    return BenchmarkEnv{..}

bench_fromList :: [[Int]] -> Benchmarkable
bench_fromList list =
    nfIO $ networkToList (length list) $ do
        listD <- signalToDiscrete <$> signalFromList list
        coll <- collectionFromDiscreteList (0 :: Int) listD
        discreteToSignal $ collectionToDiscreteList coll

bench_transform :: [[Int]] -> Benchmarkable
bench_transform list =
    nfIO $ networkToList (length list) $ do
        listD <- signalToDiscrete <$> signalFromList list
        coll0 <- collectionFromDiscreteList (0 :: Int) listD
        coll1 <- filterCollection even coll0
        coll2 <- mapCollection (\x -> if x `mod` 5 == 0 then Just x else Nothing) coll1
        coll3 <- justCollection coll2
        discreteToSignal $ collectionToDiscreteList coll3

bench_accum :: [CollectionUpdate Int Int] -> Benchmarkable
bench_accum updates =
    nfIO $ networkToList (length updates) $ do
        updatesE <- signalToEvent <$> signalFromList (map pure updates)
        coll <- accumCollection updatesE
        discreteToSignal $ collectionToDiscreteList coll

bench_transformAccum :: [CollectionUpdate Int Int] -> Benchmarkable
bench_transformAccum updates =
    nfIO $ networkToList (length updates) $ do
        updatesE <- signalToEvent <$> signalFromList (map pure updates)
        coll0 <- accumCollection updatesE
        coll1 <- filterCollection even coll0
        coll2 <- mapCollection (\x -> if x `mod` 5 == 0 then Just x else Nothing) coll1
        coll3 <- justCollection coll2
        discreteToSignal $ collectionToDiscreteList coll3

bench_mapToCollection :: EML.EnumMap Int Int -> Benchmarkable
bench_mapToCollection dict =
    nfIO $ networkToList 1 $ do
        coll <- enummapToCollection (pure dict)
        let mangle :: Discrete [(a, Discrete b)] -> Discrete [(a, b)]
            mangle = join . fmap (sequence . map sequence)
        discreteToSignal $ mangle $ collectionToDiscreteList coll

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DoRec, ScopedTypeVariables, TupleSections
            ,DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

-- | Collection signals with incremental updates.
module FRP.Euphoria.Collection
( CollectionUpdate (..)
, Collection
-- * creating collections
, simpleCollection
, emptyCollection
, collectionFromList
, collectionFromDiscreteList
, mapToCollection
, makeCollection
-- * observing collections
, watchCollection
, followCollectionKey
, collectionToDiscreteList
, openCollection
-- * other functions
, mapCollection
, sequenceCollection
) where

import Control.Applicative
import Control.Monad (join)
import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EnumMap
import Data.List
import Data.Maybe (mapMaybe)
import Data.Traversable
import Data.Foldable (Foldable)
import Data.Monoid
import Test.HUnit

import FRP.Euphoria.Event


-- | Represents an incremental change to a collection of items.
data CollectionUpdate k a
    = AddItem k a
    | RemoveItem k
    deriving (Functor, Eq, Show, Foldable, Traversable)

-- | An FRP interface for representing an incrementally updated
-- collection of items. The items are identified by a unique key.
-- Items may be added or removed from the current collection.
--
-- This type is useful because it allows you to manage the incremental
-- state updates to something that needs a collection of items without
-- having to rebuild it completely every time the collection changes.
-- Consider the type Signal [a] -- functionally, it also represents a
-- collection of items that changes over time. However, there is no
-- state carried between changes. If, for example, we have a GUI
-- widget that lists items whose content is represented as a Signal
-- [a], we would have to destroy and rebuild the widget's internal
-- state every time the list contents change. But with the Collection
-- type, we can add or remove from the GUI widget only the necessary
-- items. This is useful both from a performance (most existing GUI
-- toolkits exhibit worse performance when adding and removing all
-- items with every change) and behavior standpoint, because the GUI
-- toolkit can, for example, remember which items the user had
-- selected between list updates.
--
-- Usage of 'Collection' implies there could be some caching/state by
-- the consumer of the Events, otherwise one might as well use a
-- Signal [a].
newtype Collection k a = Collection {
  unCollection :: Discrete ([(k, a)], Event (CollectionUpdate k a))
  }

instance SignalSet (Collection k a) where
    basicSwitchD dis0 = do
        dis <- memoD dis0
        listD <- memoD $ join (fmap fst . unCollection <$> dis)
        listS <- discreteToSignal listD
        prevListS <- delayS [] listS

        chE <- dropStepE $ changesD dis
        (_, initialUpdatesE) <- openCollection =<< snapshotD dis
        updatesE <- generatorD' =<< stepperD (return initialUpdatesE)
            (updates <$> prevListS <*> listS <@> chE)

        makeCollection listD updatesE
        where
            updates prevList list (Collection newCol) = do
                rebuild <- flattenE <$> onCreation (map remove prevList ++ map add list)
                (_, newUpdates) <- snapshotD newCol
                memoE $ rebuild `mappend` newUpdates
            remove (k, _) = RemoveItem k
            add (k, v) = AddItem k v

    memoizeSignalSet (Collection dis)= Collection <$> memoD dis

-- | Like 'fmap', but the Collection and interior 'Event' stream are memoized
mapCollection :: (a -> b) -> Collection k a -> SignalGen (Collection k b)
mapCollection f aC = do
  updateE <- snd <$> openCollection aC
  newCurD    <- memoD $ fmap ((fmap . fmap) f . fst) $ unCollection aC
  newUpdateE <- memoE $ (fmap . fmap) f updateE
  makeCollection newCurD newUpdateE

-- | Create an 'Event' stream of all updates from a collection, including
-- the items currently in it.
collectionToUpdates
    :: forall k a. Collection k a
    -> SignalGen (Event (CollectionUpdate k a))
collectionToUpdates aC = do
    (cur,updateE) <- openCollection aC
    initE  <- onCreation (map (uncurry AddItem) cur)
    initE' <- memoE $ flattenE initE
    return (updateE `mappend` initE')

sequenceCollection
    :: Enum k
    => Collection k (SignalGen a)
    -> SignalGen (Collection k a)
sequenceCollection col = collectionToUpdates col
  >>= generatorE . fmap sequenceA
  >>= accumCollection

-- | A collection whose items are created by an event, and removed by
-- another event.
simpleCollection :: (Enum k)
                 => k
                 -- ^ The initial value for the unique keys. 'succ'
                 -- will be used to get further keys.
                 -> Event (a, Event ())
                 -- ^ An Event that introduces a new item and its
                 -- subsequent removal Event. The item will be removed
                 -- from the collection when the Event () fires.
                 -> SignalGen (Collection k a)
simpleCollection initialK evs =
    simpleCollectionUpdates initialK evs >>= accumCollection

simpleCollectionUpdates :: (Enum k) => k
                        -> Event (a, Event ())
                        -> SignalGen (Event (CollectionUpdate k a))
simpleCollectionUpdates initialK evs = do
    let addKey (a, ev) k = (succ k, (k, a, ev))
    newEvents <- scanAccumE initialK (addKey <$> evs)
    let addItem (k, _a, ev) = EnumMap.insert k ev
    rec
        removalEvent' <- delayE removalEvent
        removalEvents <- accumD EnumMap.empty
            ((addItem <$> newEvents) `mappend` (EnumMap.delete <$> removalEvent'))
        removalEvent <- switchD $ EnumMap.foldWithKey
            (\k ev ev' -> (k <$ ev) `mappend` ev') mempty <$> removalEvents
    let -- updateAddItem :: (Enum k) => (k, a, Event ()) -> CollectionUpdate k a
        updateAddItem (k, a, _) = AddItem k a
    memoE $ (updateAddItem <$> newEvents) `mappend` (RemoveItem <$> removalEvent)

-- Turns adds the necessary state for holding the existing [(k, a)]
-- and creating the unique Event stream for each change of the
-- collection.
accumCollection :: (Enum k)
                => Event (CollectionUpdate k a)
                -> SignalGen (Collection k a)
accumCollection ev = do
    let toMapOp (AddItem k a) = EnumMap.insert k a
        toMapOp (RemoveItem k) = EnumMap.delete k
    mapping <- accumD EnumMap.empty (toMapOp <$> ev)
    listD <- memoD $ EnumMap.toList <$> mapping
    makeCollection listD ev

-- | The primitive interface for creating a 'Collection'. The two
-- arguments must be coherent, i.e. the value of the discrete at
-- time /t+1/ should be obtained by applying the updates
-- at /t+1/ to the value of the discrete at /t/. This invariant
-- is not checked.
makeCollection
    :: Discrete [(k, a)]
    -> Event (CollectionUpdate k a)
    -> SignalGen (Collection k a)
makeCollection listD updE = Collection <$> generatorD (gen <$> listD)
    where
        gen list = do
            updE' <- dropStepE updE
            return (list, updE')

-- | Prints add/remove diagnostics for a Collection. Useful for debugging
watchCollection :: (Show k, Show a)
                => Collection k a -> SignalGen (Event (IO ()))
watchCollection (Collection coll) = do
    ev1 <- takeE 1 =<< preservesD coll
    now <- onCreation ()
    let f (items, ev) = ((putStrLn . showUpdate) <$> ev) `mappend`
            (mapM_ (putStrLn . showExisting) items <$ now)
        showUpdate (AddItem k a) = "Add: " ++ show k ++ ", " ++ show a
        showUpdate (RemoveItem k) = "Remove: " ++ show k
        showExisting (k, a) = "Existing: " ++ show k ++ ", " ++ show a
    switchD =<< stepperD mempty (f <$> ev1)

-- | An empty, unchanging Collection.
emptyCollection :: Collection k a
emptyCollection = collectionFromList []

-- | A pure function to create a Collection from key-value pairs. This
-- collection will never change.
collectionFromList :: [(k, a)] -> Collection k a
collectionFromList kvs = Collection $ pure (kvs, mempty)

-- | A somewhat inefficient but easy-to-use way of turning a list of
-- items into a Collection. Probably should only be used for temporary
-- hacks. Will perform badly with large lists.
collectionFromDiscreteList
    :: (Enum k, Eq a)
    => k
    -> Discrete [a]
    -> SignalGen (Collection k a)
collectionFromDiscreteList initialK valsD = do
    valsE <- preservesD valsD
    evs <- scanAccumE (initialK, EnumMap.empty) (stepListCollState <$> valsE)
    accumCollection (flattenE evs)

-- This could obviously be implemented more efficiently.
stepListCollState :: (Enum k, Eq a) => [a]
                  -> (k, EnumMap k a)
                  -> ((k, EnumMap k a), [CollectionUpdate k a])
stepListCollState xs (initialK, existingMap) = ((k', newMap'), removeUpdates ++ addUpdates)
  where
    keyvals = EnumMap.toList existingMap
    newItems = xs \\ map snd keyvals
    removedKeys = map fst $ deleteFirstsBy
        (\(_, x) (_, y) -> x == y)
        keyvals
        (map (\x -> (initialK, x)) xs)
    (newMap, removeUpdates) = foldl
        (\(em, upds) k -> (EnumMap.delete k em, upds ++ [RemoveItem k]))
        (existingMap, []) removedKeys
    (k', newMap', addUpdates) = foldl
        (\(k, em, upds) x -> (succ k, EnumMap.insert k x em, upds ++ [AddItem k x]))
        (initialK, newMap, []) newItems

data MapCollEvent k a
    = MCNew k a
    | MCChange k a
    | MCRemove k

mapCollDiff :: (Enum k, Eq a) => EnumMap k a -> EnumMap k a -> [MapCollEvent k a]
mapCollDiff prevmap newmap = newEvs ++ removeEvs ++ changeEvs
  where
    newStuff = newmap EnumMap.\\ prevmap
    removedStuff = prevmap EnumMap.\\ newmap
    keptStuff = newmap `EnumMap.intersection` prevmap
    changedStuff = mapMaybe f (EnumMap.toList keptStuff)
      where f (k, v1) = case EnumMap.lookup k prevmap of
                Nothing -> Nothing
                Just v2 | v1 /= v2 -> Just (k, v1)
                        | otherwise -> Nothing
    makeNew (k, v) = MCNew k v
    makeRemove (k, _) = MCRemove k
    makeChange (k, v) = MCChange k v
    newEvs = map makeNew (EnumMap.toList newStuff)
    removeEvs = map makeRemove (EnumMap.toList removedStuff)
    changeEvs = map makeChange changedStuff

dispatchCollEvent :: (Enum k, Eq k, Eq a)
                  => Event (MapCollEvent k a)
                  -> SignalGen (Collection k (Discrete a))
dispatchCollEvent mapcollE = do
    let f (MCChange k a) = Just (k, a)
        f _ = Nothing
    changeEv <- memoE $ justE (f <$> mapcollE)
    let g (MCNew k a) = Just $
            AddItem k <$> followCollItem a k changeEv
        g (MCRemove k) = Just $ return $ RemoveItem k
        g (MCChange _ _) = Nothing
    updateEv <- generatorE $ justE (g <$> mapcollE)
    accumCollection updateEv

followCollItem :: (Eq k) => a -> k
               -> Event (k, a)
               -> SignalGen (Discrete a)
followCollItem val k1 ev = stepperD val (justE (f <$> ev))
  where f (k2, v) | k1 == k2 = Just v
                  | otherwise = Nothing

-- | Turns mapping of values into a collection of first-class FRP
-- values that are updated. If items are added to the EnumMap, then
-- they will be added to the Collection. Likewise, if they are removed
-- from the mapping, they will be removed from the collection. Keys
-- that are present in both but have new values will have their
-- Discrete value updated, and keys with values that are still present
-- will not have their Discrete values updated.
mapToCollection :: forall k a.
                  (Enum k, Eq k, Eq a)
                => Discrete (EnumMap k a)
                -> SignalGen (Collection k (Discrete a))
mapToCollection mapD = do
    m1 <- delayD EnumMap.empty mapD
    let collDiffs :: Discrete [MapCollEvent k a]
        collDiffs = mapCollDiff <$> m1 <*> mapD
    dispatchCollEvent . flattenE =<< preservesD collDiffs

-- | Look for a key in a collection, and give its (potentially
-- nonexistant) value over time.
followCollectionKey :: forall k a. (Eq k)
                    => k
                    -> Collection k a
                    -> SignalGen (Discrete (Maybe a))
followCollectionKey k (Collection coll) = do
    collAsNow <- takeE 1 =<< preservesD coll
        :: SignalGen (Event ([(k, a)], Event (CollectionUpdate k a)))
    let existing :: Event (CollectionUpdate k a)
        existing = flattenE $ initialAdds . fst <$> collAsNow
        further :: Event (Event (CollectionUpdate k a))
        further  = snd <$> collAsNow
    further' <- switchD =<< stepperD mempty further
        :: SignalGen (Event (CollectionUpdate k a))
    accumMatchingItem (== k) (existing `mappend` further')

-- Turn the existing items into AddItems for our state accumulation
initialAdds :: [(k, a)] -> [CollectionUpdate k a]
initialAdds = map (uncurry AddItem)

-- Accumulate CollectionUpdates, and keep the newest value whose key
-- is True for the given function.
accumMatchingItem :: forall k a.
                  (k -> Bool)
                  -> Event (CollectionUpdate k a)
                  -> SignalGen (Discrete (Maybe a))
accumMatchingItem f updateE =
    stepperD Nothing $ justE (g <$> updateE)
  where
    g :: CollectionUpdate k a -> Maybe (Maybe a)
    g (AddItem k a)  | f k       = Just (Just a)
                     | otherwise = Nothing
    g (RemoveItem k) | f k       = Just Nothing
                     | otherwise = Nothing

-- | Extracts a 'Discrete' which represents the current state of
-- a collection.
collectionToDiscreteList :: Collection k a -> Discrete [(k, a)]
collectionToDiscreteList = fmap fst . unCollection

-- | Extracts a snapshot of the current values in a collection with
-- an 'Event' stream of further updates
openCollection :: Collection k a -> SignalGen ([(k,a)], Event (CollectionUpdate k a))
openCollection = snapshotD . unCollection

--------------------------------------------------------------------------------
-- Unit tests

test_switchCollection :: Test
test_switchCollection = test $ do
    result <- networkToList 5 $ do
        col0 <- collectionFromDiscreteList (0::Int) =<< mkD [[10::Int], [], [10,20,30], [20,30], [30]]
        col1 <- collectionFromDiscreteList 0 =<< mkD [[11], [], [11,21,31], [21,31], [31]]
        col2 <- collectionFromDiscreteList 0 =<< mkD [[12], [], [12,22,32], [22,32], [32]]
        colD <- stepperD col0 =<< eventFromList [[], [], [col1], [], [col2]]
        col <- switchD colD
        (_, updates) <- openCollection col
        listS <- discreteToSignal $ collectionToDiscreteList col
        return $ (,) <$> listS <*> (eventToSignal updates)
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

_unitTest :: IO Counts
_unitTest = runTestTT $ test
    [ test_switchCollection
    ]

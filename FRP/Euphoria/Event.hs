{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}

#if __GLASGOW_HASKELL__ <= 706
{-# LANGUAGE DoRec #-}
#else
{-# LANGUAGE RecursiveDo #-}
#endif

-- | Event/discrete layer constructed on top of Elerea.
-- The API is largely inspired by reactive-banana.
module FRP.Euphoria.Event
(
-- * Events
  Event
-- ** Creation
, externalEvent
, eachStep
, onCreation
, signalToEvent
-- ** Sampling
, apply
, eventToSignal
-- ** State accumulation
-- | With these functions, any input event occurrence will affect the output
-- immediately, without any delays.
, stepperS
, accumS
, accumSIO
, accumE
, accumEM
, scanAccumE
, scanAccumEM
-- ** Filtering and other list-like operations
, filterE
, justE
, mapMaybeE
, flattenE
, expandE
, withPrevE
, dropE
, dropWhileE
, takeE
, takeWhileE
, partitionEithersE
, leftE
, rightE
, groupByE
, groupWithInitialByE
, groupE
, groupWithInitialE
, splitOnE
, differentE
-- ** Other event operations
, delayE
, dropStepE
, mapEIO
, memoE
, joinEventSignal
, generatorE
-- * Discrete signals
, Discrete
-- ** Sampling 'Discrete's
-- $sampling_discrete

-- ** Accumulation
, stepperD
, stepperMaybeD
, justD
, accumD
-- ** Conversion into events
, eachStepD
, changesD
, preservesD
-- ** Other discrete operations
, snapshotD -- broken? crashes?
, memoD
, delayD
, generatorD
, minimizeChanges
, discreteToSignal
, freezeD
, signalToDiscrete
, keepJustsD
, keepDJustsD
-- * Signals
, module FRP.Euphoria.Signal
-- * Application operators
, Apply (..)
-- $app_discrete_maybe
, (<$?>), (<?*?>), (<-*?>), (<?*->)
, EasyApply (..)
-- * Switching
, switchD
, switchDE
, switchDS
, generatorD'
, SignalSet (..)
-- * Evaluation control
, forceD
, forceE
, rnfD
, rnfE
-- * Debugging
-- | Side-effecting trace functions
, traceSignalMaybe
, traceSignalT
, traceEventT
, traceDiscreteT
-- * Testing
, signalFromList
, eventFromList
, networkToList
) where

import Control.Arrow ((&&&))
import Control.Applicative
import Control.DeepSeq
import Control.Monad (join, replicateM)
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Either (partitionEithers, lefts, rights)
import Data.List (foldl')
import Data.Monoid
import Data.Maybe
import Data.Typeable
import Debug.Trace
import FRP.Euphoria.Signal
import FRP.Elerea.Simple (externalMulti, effectful1, until, stateful)
import Prelude hiding (until)

-- | @Event a@ represents a stream of events whose occurrences carry
-- a value of @a@. The event can have zero, one or more occurrences
-- in a single network step.
--
-- Two event occurrences are said to be simultaneous iff they are within
-- the same step. Simultaneous occurrences are ordered within a single
-- event stream, but not across different event streams.
newtype Event a = Event (Signal [a])
  deriving (Functor, Typeable)
-- | @Discrete a@ is much like @'Signal' a@, but the user can get notified
-- every time the value may have changed. See 'changesD'.
newtype Discrete a = Discrete (Signal (Bool, a))
  -- The first component indicates if the value may be new.
  -- If it is False, the consumer should avoid evaluating the
  -- second component whenever possible.
  -- FIXME: This trick alone cannot remove all redundant recomputations.
  -- Consider the case where a Discrete is
  -- read every iteration in a fresh SignalGen run.
  deriving (Functor, Typeable)
-- type Behavior a = Signal a

-- | Event streams can be merged together. In case of simultaneous occurrences,
-- occurrences from the left stream comes first.
instance Monoid (Event a) where
  mempty = Event $ pure []
  Event a `mappend` Event b = Event $ (++) <$> a <*> b

infixl 4 <@>, <@

-- | A generalization of @Applicative@ where the lhs and the rhs can have
-- different container types.
class (Functor f, Functor g) => Apply f g where
  (<@>) :: f (a -> b) -> g a -> g b
  (<@) :: f a -> g b -> g a

  f <@ g = const <$> f <@> g

instance Apply Signal Event where
  (<@>) = apply

-- It's difficult to implement this without causing needless recalculation:
--instance Apply Discrete Event where

-- | Create an event that can be triggered as an IO action.
externalEvent :: (MonadSignalGen g, MonadIO m, MonadIO m') => m (g (Event a), a -> m' ())
externalEvent = liftIO $ do
  (gen, trigger) <- externalMulti
  return (Event . fmap reverse <$> liftSignalGen gen, liftIO . trigger)

-- | Transform an event stream using a time-varying transformation function.
--
-- There is also an infix form '<@>'.
apply :: Signal (a -> b) -> Event a -> Event b
apply sig (Event evt) = Event $ map <$> sig <*> evt

-- | Filter an event stream.
filterE :: (a -> Bool) -> Event a -> Event a
filterE cond (Event evt) = Event $ filter cond <$> evt

-- | @stepperS initial evt@ returns a signal whose value is the last occurrence
-- of @evt@, or @initial@ if there has been none.
stepperS :: MonadSignalGen m => a -> Event a -> m (Signal a)
stepperS initial (Event evt) = transferS initial upd evt
  where
    upd [] old = old
    upd occs _ = last occs

-- | @eachStep sig@ is an event that occurs every step, having the same
-- value as @sig@.
eachStep :: Signal a -> Event a
eachStep = Event . fmap (:[])

-- | 'Discrete' version of eachStep.
eachStepD :: MonadSignalGen m => Discrete a -> m (Event a)
eachStepD d = do
  sig <- discreteToSignal d
  return $ eachStep sig

-- | The basic construct to build a stateful signal. @accumS initial evt@
-- returns a signal whose value is originally @initial@. For each occurrence
-- of @evt@ the value of the signal gets updated using the function.
--
-- Example:
--
--   If we have an event stream of numbers, (nums :: Event Int), then
--   we can make a signal that remembers the sum of the numbers seen
--   so far, as follows:
--
-- > accumS 0 $ (+) <$> nums
accumS :: MonadSignalGen m => a -> Event (a -> a) -> m (Signal a)
accumS initial (Event evt) = transferS initial upd evt
  where
    upd occs old = foldl' (flip ($)) old occs

-- | @accumS@ with side-effecting updates.
accumSIO :: (MonadSignalGen m) => a -> Event (a -> IO a) -> m (Signal a)
accumSIO initial (Event evt) = mfix $ \self -> do
  prev <- delayS initial self
  liftSignalGen $ effectful1 id $ update <$> prev <*> evt
  where
    update prev upds = foldl' (>>=) (return prev) upds

-- | @accumE initial evt@ maintains an internal state just like @accumS@.
-- It returns an event which occurs every time an update happens.
-- The resulting event, once created, will have the same number of
-- occurrences as @evt@ each step.
accumE :: (MonadSignalGen m) => a -> Event (a -> a) -> m (Event a)
accumE initial (Event evt) = fmap Event $ do
  (_, occs) <- mfix $ \ ~(self, _) -> do
    prev <- delayS initial self
    vs <- memoS $ scanl (flip ($)) <$> prev <*> evt
    return (last <$> vs, tail <$> vs)
  return occs

-- | A useful special case of 'accumE'.
scanAccumE :: MonadSignalGen m => s -> Event (s -> (s, a)) -> m (Event a)
scanAccumE initial ev = (snd <$>) <$> accumE (initial, undefined) (f <$> ev)
  where
    f fn (s, _) = fn s

-- | Monadic version of @accumE@.
accumEM :: (MonadSignalGen m) => s -> Event (s -> SignalGen s) -> m (Event s)
accumEM initial (Event evt) = fmap Event $ do
  rec
    prevState <- delayS initial (fst <$> state_out)
    state_out <- generatorS $ stateGen <$> prevState <*> evt
  memoS $ snd <$> state_out
  where
    stateGen prev occs = foldr app end occs prev []
    app occ next val history = do
      val' <- occ val
      next val' (val':history)
    end val history = return (val, reverse history)

-- | A useful special case of @accumEM@.
scanAccumEM :: MonadSignalGen m => s -> Event (s -> SignalGen (s, a)) -> m (Event a)
scanAccumEM initial ev = (snd <$>) <$> accumEM (initial, undefined) (f <$> ev)
  where
    f fn (s, _) = fn s

-- | Drops all events in this network step
dropStepE :: MonadSignalGen m => Event a -> m (Event a)
dropStepE ev = do
    initial <- delayS True (pure False)
    memoE $ justE $ discardIf <$> initial <@> ev
    where
        discardIf True _ = Nothing
        discardIf False x = Just x

-- | Converts an event stream of lists into a stream of their elements.
-- All elements of a list become simultaneous occurrences.
flattenE :: Event [a] -> Event a
flattenE (Event evt) = Event $ concat <$> evt

-- | Expand simultaneous events (if any)
expandE :: Event a -> Event [a]
expandE (Event evt) = Event $ f <$> evt
    where
        f [] = []
        f xs = [xs]

-- | Like 'mapM' over events.
mapEIO :: MonadSignalGen m => (t -> IO a) -> Event t -> m (Event a)
mapEIO mkAction (Event evt) = Event <$> liftSignalGen (effectful1 (mapM mkAction) evt)

-- | Memoization of events. See the doc for 'FRP.Elerea.Simple.memo'.
memoE :: MonadSignalGen m => Event a -> m (Event a)
memoE (Event evt) = Event <$> memoS evt

-- | An event whose occurrences come from different event stream
-- each step.
joinEventSignal :: Signal (Event a) -> Event a
joinEventSignal sig = Event $ do
  Event occs <- sig
  occs

-- | Remove occurrences that are 'Nothing'.
justE :: Event (Maybe a) -> Event a
justE (Event evt) = Event $ catMaybes <$> evt

-- | Like 'mapMaybe' over events.
mapMaybeE :: (a -> Maybe b) -> Event a -> Event b
mapMaybeE f evt = justE $ f <$> evt

-- | @onCreation x@ creates an event that occurs only once,
-- immediately on creation.
onCreation :: MonadSignalGen m => a -> m (Event a)
onCreation x = Event <$> delayS [x] (return [])

-- | @delayE evt@ creates an event whose occurrences are
-- same as the occurrences of @evt@ in the previous step.
delayE :: MonadSignalGen m => Event a -> m (Event a)
delayE (Event x) = Event <$> delayS [] x

-- | @withPrevE initial evt@ is an Event which occurs every time
-- @evt@ occurs. Each occurrence carries a pair, whose first element
-- is the value of the current occurrence of @evt@, and whose second
-- element is the value of the previous occurrence of @evt@, or
-- @initial@ if there has been none.
withPrevE :: MonadSignalGen m => a -> Event a -> m (Event (a, a))
withPrevE initial evt = accumE (initial, undefined) $ toUpd <$> evt
  where
    toUpd val (new, _old) = (val, new)

-- | @generatorE evt@ creates a subnetwork every time @evt@ occurs.
generatorE :: MonadSignalGen m => Event (SignalGen a) -> m (Event a)
generatorE (Event evt) = Event <$> generatorS (sequence <$> evt)

-- | @dropE n evt@ returns an event, which behaves similarly to
-- @evt@ except that its first @n@ occurrences are dropped.
dropE :: MonadSignalGen m => Int -> Event a -> m (Event a)
dropE n (Event evt) = Event . fmap fst <$> transferS ([], n) upd evt
    where
        upd occs (_, k)
            | k <= 0 = (occs, 0)
            | otherwise = let
                !k' = k - length occs
                in (drop k occs, k')

-- | @dropWhileE p evt@ returns an event, which behaves similarly to
-- @evt@ except that all its occurrences before the first one
-- that satisfies @p@ are dropped.
dropWhileE :: MonadSignalGen m => (a -> Bool) -> Event a -> m (Event a)
dropWhileE p (Event evt) = Event . fmap fst <$> transferS ([], False) upd evt
  where
    upd occs (_, True) = (occs, True)
    upd occs (_, False) = case span p occs of
      (_, []) -> ([], False)
      (_, rest) -> (rest, True)

-- | Take the first n occurrences of the event and discard the rest.
-- It drops the reference to the original event after
-- the first n occurrences are seen.
takeE :: MonadSignalGen m => Int -> Event a -> m (Event a)
takeE n evt = generalPrefixE (primTakeE n) evt

primTakeE :: MonadSignalGen m => Int -> Signal [a] -> m (Signal (Bool, [a]))
primTakeE n evt = fmap fst <$> transferS ((True, []), n) upd evt
    where
        upd occs (_, k) = ((k > 0, take k occs), k')
            where
                !k' = k - length occs

-- | Take the first occurrences satisfying the predicate and discard the rest.
-- It drops the reference to the original event after
-- the first non-satisfying occurrence is seen.
takeWhileE :: MonadSignalGen m => (a -> Bool) -> Event a -> m (Event a)
takeWhileE p evt = generalPrefixE (primTakeWhileE p) evt

primTakeWhileE :: MonadSignalGen m => (a -> Bool) -> Signal [a] -> m (Signal (Bool, [a]))
primTakeWhileE p evt = memoS $ f <$> evt
    where
        f occs = case span p occs of
          (_, []) -> (True, occs)
          (end, _) -> (False, end)

generalPrefixE
  :: MonadSignalGen m
  => (Signal [a] -> m (Signal (Bool, [a])))
  -> Event a
  -> m (Event a)
generalPrefixE prefixTaker (Event evt) = do
    rec
        done <- liftSignalGen $ until $ not . fst <$> active_occs
        prevDone <- delayS False done
        eventSource <- transferS evt upd prevDone
        active_occs <- prefixTaker (join eventSource)
    Event <$> memoS (snd <$> active_occs)
    where
        upd done prev = ifelse done (pure []) prev

        {-# NOINLINE ifelse #-}
        ifelse b x y = if b then x else y

        -- Here we hide an if expression from GHC's optimizer.
        -- If GHC finds this conditional, its "state hack"
        -- transforation turns the definition into:
        --
        -- upd done prev s = if done then [] else prev s
        --
        -- which is a disaster, because now (upd False prev)
        -- doesn't reduce to prev. This means each iteration
        -- the signal gets bigger and more expensive to evaluate.

-- | Split a stream of 'Either's into two, based on tags. This needs to be
-- in SignalGen in order to memoise the intermediate result.
partitionEithersE :: MonadSignalGen m => Event (Either a b) -> m (Event a, Event b)
partitionEithersE (Event eithersS) = (Event . fmap fst &&& Event . fmap snd)
  <$> memoS (partitionEithers <$> eithersS)

-- | Keep occurrences which are Left.
leftE :: Event (Either e a) -> Event e
leftE (Event eithersS) = Event (lefts <$> eithersS)

-- | Keep occurrences which are Right.
rightE :: Event (Either e a) -> Event a
rightE (Event eithersS) = Event (rights <$> eithersS)

-- | @groupByE eqv evt@ creates a stream of event streams, each corresponding
-- to a span of consecutive occurrences of equivalent elements in the original
-- stream. Equivalence is tested using @eqv@.
groupByE :: MonadSignalGen m => (a -> a -> Bool) -> Event a -> m (Event (Event a))
groupByE eqv sourceEvt = fmap snd <$> groupWithInitialByE eqv sourceEvt

-- | @groupWithInitialByE eqv evt@ creates a stream of event streams, each corresponding
-- to a span of consecutive occurrences of equivalent elements in the original
-- stream. Equivalence is tested using @eqv@. In addition, each outer event
-- occurrence contains the first occurrence of its inner event.
groupWithInitialByE :: MonadSignalGen m => (a -> a -> Bool) -> Event a -> m (Event (a, Event a))
groupWithInitialByE eqv sourceEvt = do
    networkE <- justE <$> scanAccumE Nothing (makeNetwork <$> sourceEvt)
    generatorE networkE
    where
        makeNetwork val currentVal
            | maybe False (eqv val) currentVal = (currentVal, Nothing)
            | otherwise = (Just val, Just $ (,) val <$> network val)
        network val = takeWhileE (eqv val) =<< dropWhileE (not . eqv val) sourceEvt

-- | Same as @'groupByE' (==)@
groupE :: (Eq a, MonadSignalGen m) => Event a -> m (Event (Event a))
groupE = groupByE (==)

-- | Same as @groupWithInitialByE (==)@
groupWithInitialE :: (Eq a, MonadSignalGen m) => Event a -> m (Event (a, Event a))
groupWithInitialE = groupWithInitialByE (==)

-- | For each Event () received, emit all 'a' in a list since the last
-- Event () was received. In the case of simultaneous 'a' and '()' in
-- a step, the 'a' are included in the emitted list.
splitOnE :: MonadSignalGen m => Event () -> Event a -> m (Event [a])
splitOnE completeE aE = do
    let inE = (Right <$> aE) `mappend` (Left <$> completeE)
    let f (Left ()) accAs = ([], Just (reverse accAs))
        f (Right a) accAs = (a : accAs, Nothing)
    memoE =<< justE <$> scanAccumE [] (f <$> inE)

-- | @eventToSignal evt@ is a signal whose value is the list of current
-- occurrences of @evt@.
eventToSignal :: Event a -> Signal [a]
eventToSignal (Event x) = x

-- | The inverse of 'eventToSignal'.
signalToEvent :: Signal [a] -> Event a
signalToEvent = Event

-- $sampling_discrete
-- 'Signal's can be sampled using 'apply' or equivalently '<@>'.
-- However, currently there are no corresponding functions for 'Discrete'
-- due to implementation difficulty. To sample a 'Discrete', you need to
-- first convert it into a 'Signal' using 'discreteToSignal'.

-- | @changesD dis@ is an event that occurs when the value of @dis@ may
-- have changed. It never occurs more than once a step.
changesD :: Discrete a -> Event a
changesD (Discrete dis) = Event $ conv <$> dis
  where
    conv (new, x) = if new then [x] else []

-- | Like 'changesD', but uses the current value in the Discrete even if
-- it is not new.
preservesD :: MonadSignalGen m => Discrete a -> m (Event a)
preservesD dis = do
    ev <- onCreation ()
    sig <- discreteToSignal dis
    memoE $ (const <$> sig <@> ev) `mappend` changesD dis

-- | @snapshotD dis@ returns the current value of @dis@.
snapshotD :: MonadSignalGen m => Discrete a -> m a
-- Seems to cause problems with the network. Is the underlying
-- 'snapshot' actually safe?
snapshotD (Discrete a) = snd <$> snapshotS a

-- | Like 'stepperS', but creates a 'Discrete'.
stepperD :: MonadSignalGen m => a -> Event a -> m (Discrete a)
stepperD initial (Event evt) = Discrete <$> transferS (False, initial) upd evt
  where
    upd [] (_, old) = (False, old)
    upd occs _ = (True, last occs)

-- | Use 'Nothing' to supply the initial value, and wrap the returned
-- type in 'Maybe'.
stepperMaybeD :: MonadSignalGen m => Event a -> m (Discrete (Maybe a))
stepperMaybeD ev = stepperD Nothing (Just <$> ev)

-- | Given an initial value, filter out the Nothings.
justD :: MonadSignalGen m => a -> Discrete (Maybe a) -> m (Discrete a)
justD initial mD = do
    mE <- preservesD mD
    stepperD initial (justE mE)

-- | Like @accumS@, but creates a 'Discrete'.
accumD :: MonadSignalGen m => a -> Event (a -> a) -> m (Discrete a)
accumD initial (Event evt) = Discrete <$> transferS (False, initial) upd evt
  where
    upd [] (_, old) = (False, old)
    upd upds (_, old) = (True, new)
      where !new = foldl' (flip ($)) old upds

-- | Filter events to only those which are different than the previous event.
differentE :: (Eq a, MonadSignalGen m) => Event a -> m (Event a)
differentE ev = (justE . (f <$>)) <$> withPrevE Nothing (Just <$> ev)
  where
    f :: (Eq a) => (Maybe a, Maybe a) -> Maybe a
    f (new, old) = if new /= old then new else old

instance Applicative Discrete where
  pure x = Discrete $ pure (False, x)
  Discrete f <*> Discrete a = Discrete $ app <$> f <*> a
    where
      app (newFun, fun) (newArg, arg) = (new, fun arg)
        where !new = newFun || newArg

instance Monad Discrete where
  return x = Discrete $ return (False, x)
  Discrete x >>= f = Discrete $ do
    (newX, v) <- x
    let Discrete y = f v
    (newY, r) <- y
    let !new = newX || newY
    return (new, r)

-- | Memoization of discretes. See the doc for 'FRP.Elerea.Simple.memo'.
memoD :: MonadSignalGen m => Discrete a -> m (Discrete a)
memoD (Discrete dis) = Discrete <$> memoS dis

-- | Like 'delayS'.
delayD :: MonadSignalGen m => a -> Discrete a -> m (Discrete a)
delayD initial (Discrete subsequent) = Discrete <$> delayS (True, initial) subsequent

-- | Like 'generatorS'. A subnetwork is only created when the value of the
-- discrete may have changed.
generatorD :: MonadSignalGen m => Discrete (SignalGen a) -> m (Discrete a)
generatorD (Discrete sig) = do
    first <- delayS True $ pure False
    listResult <- generatorS $ networkOnChanges <$> first <*> sig
    stepperD undefined (Event listResult)
    where
        networkOnChanges first (new, gen)
            | first || new = (:[]) <$> gen
            | otherwise = return []

-- | Executes a dynamic 'SignalGen' in a convenient way.
--
-- > generatorD' dis = generatorD dis >>= switchD
generatorD' :: (MonadSignalGen m, SignalSet s) => Discrete (SignalGen s) -> m s
generatorD' dis = generatorD dis >>= switchD

-- | @minimizeChanges dis@ creates a Discrete whose value is same as @dis@.
-- The resulting discrete is considered changed only if it is really changed.
minimizeChanges :: (MonadSignalGen m, Eq a) => Discrete a -> m (Discrete a)
minimizeChanges (Discrete dis) = Discrete . fmap fromJust <$> transferS Nothing upd dis
  where
    upd (False, _) (Just (_, cache)) = Just (False, cache)
    upd (True, val) (Just (_, cache))
      | val == cache = Just (False, cache)
    upd (new, val) _ = Just (new, val)

recordDiscrete :: MonadSignalGen m => Discrete a -> m (Discrete a)
recordDiscrete (Discrete dis) = Discrete . fmap fromJust <$> transferS Nothing upd dis
  where
    upd (False, _) (Just (_, cache)) = Just (False, cache)
    upd new_val _ = Just new_val

-- | Converts a 'Discrete' to an equivalent 'Signal'.
discreteToSignal :: MonadSignalGen m => Discrete a -> m (Signal a)
discreteToSignal dis = discreteToSignalNoMemo <$> recordDiscrete dis

-- | @switchD dis@ creates some signal-like thing whose value is
-- same as the thing @dis@ currently contains.
switchD :: (SignalSet s, MonadSignalGen m) => Discrete s -> m s
switchD dis = recordDiscrete dis >>= basicSwitchD >>= memoizeSignalSet

-- | @switchDS@ selects current @Signal a@ of a 'Discrete'.
--
-- See @switchD@ for a more general function.
switchDS :: MonadSignalGen m => Discrete (Signal a) -> m (Signal a)
switchDS = switchD

-- | @switchDE@ selects the current 'Event' stream contained in a 'Discrete'
--
-- See @switchD@ for a more general function.
switchDE :: MonadSignalGen m => Discrete (Event a) -> m (Event a)
switchDE = switchD

-- | @freezeD fixEvent dis@ returns a discrete whose value is same as
-- @dis@ before @fixEvent@ is activated first. Its value gets fixed once
-- an occurrence of @fixEvent@ is seen.
freezeD :: MonadSignalGen m => Event () -> Discrete a -> m (Discrete a)
freezeD evt dis = do
    dis' <- memoD dis
    now <- onCreation ()
    sig <- discreteToSignal dis'
    initialization <- takeE 1 $ const <$> sig <@> now
    filteredChanges <- switchD =<< stepperD (changesD dis') (mempty <$ evt)
    stepperD (error "freezeD: not initialized") $ initialization `mappend` filteredChanges

-- | Convert a 'Signal' to an equivalent 'Discrete'. The resulting discrete
-- is always considered to \'possibly have changed\'.
signalToDiscrete :: Signal a -> Discrete a
signalToDiscrete x = Discrete $ (,) True <$> x

traceSignalMaybe :: String -> (a -> Maybe String) -> Signal a -> Signal a
traceSignalMaybe loc f sig = do
  v <- sig
  case f v of
    Nothing -> pure v
    Just str -> trace (loc ++ ": " ++ str) $ pure v

traceSignalT :: (Show b) => String -> (a -> b) -> Signal a -> Signal a
traceSignalT loc f = traceSignalMaybe loc (Just . show . f)

traceEventT :: (Show b) => String -> (a -> b) -> Event a -> Event a
traceEventT loc f (Event sig) = Event $ traceSignalMaybe loc msg sig
  where
    msg [] = Nothing
    msg occs = Just $ show (map f occs)

traceDiscreteT :: (Show b) => String -> (a -> b) -> Discrete a -> Discrete a
traceDiscreteT loc f (Discrete sig) = Discrete $ traceSignalMaybe loc msg sig
  where
    msg (True, val) = Just $ show (f val)
    msg (False, _) = Nothing

keepJustsD :: MonadSignalGen m => Discrete (Maybe (Maybe a))
           -> m (Discrete (Maybe a))
keepJustsD tm = do
    emm <- preservesD tm
    stepperD Nothing (justE emm)

keepDJustsD :: MonadSignalGen m => Discrete (Maybe (Discrete a))
            -> m (Discrete (Maybe a))
keepDJustsD dmd =
    fmap (fmap Just) . justE <$> preservesD dmd
    >>= stepperD (return Nothing) >>= switchD

-- $app_discrete_maybe
-- Convenience combinators for working with \''Discrete' a\' and \''Discrete'
-- (Maybe a)\' in applicative style. You can choose the right one by
-- representing what's on the left and right side of the operator with
-- the following rules:
--
-- * \'-' is for Discrete a
--
-- * \'?' is for Discrete (Maybe a)
--
infixl 4 <$?>, <?*?>, <-*?>, <?*->
(<$?>) :: (a -> b) -> Discrete (Maybe a) -> Discrete (Maybe b)
f <$?> valmD = fmap f <$> valmD

(<?*?>) :: Discrete (Maybe (a -> b)) -> Discrete (Maybe a) -> Discrete (Maybe b)
fmD <?*?> valmD = do
    fm <- fmD
    valm <- valmD
    return (fm <*> valm)

(<-*?>) :: Discrete (a -> b) -> Discrete (Maybe a) -> Discrete (Maybe b)
f <-*?> valmD = (fmap <$> f) <*> valmD

(<?*->) :: Discrete (Maybe (a -> b)) -> Discrete a -> Discrete (Maybe b)
fmD <?*-> valD = do
    fm <- fmD
    case fm of
        Just f -> Just . f <$> valD
        Nothing -> return Nothing

infixl 4 <~~>
-- | When using applicative style and mixing @('Discrete' a)@ and
-- @('Discrete' ('Maybe' a))@, EasyApply's \<~~> will attempt to choose the
-- right combinator. This is an experimental idea, and may be more
-- trouble than it's worth in practice.
--
-- GHC will fail to find instances under various circumstances, such
-- as when when anonymous functions are applied to tuples, so you will
-- have to fall back to using explicit combinators.
class EasyApply a b c | a b -> c where
  (<~~>) :: a -> b -> c

instance EasyApply (a -> b) (Discrete a) (Discrete b) where
    (<~~>) = (<$>)
instance EasyApply (Discrete (a -> b)) (Discrete a) (Discrete b) where
    (<~~>) = (<*>)
instance EasyApply (a -> b) (Discrete (Maybe a)) (Discrete (Maybe b)) where
    (<~~>) = (<$?>)
instance EasyApply (Discrete (Maybe (a -> b))) (Discrete (Maybe a)) (Discrete (Maybe b)) where
    (<~~>) = (<?*?>)
instance EasyApply (Discrete (a -> b)) (Discrete (Maybe a)) (Discrete (Maybe b)) where
    (<~~>) = (<-*?>)
instance EasyApply (Discrete (Maybe (a -> b))) (Discrete a) (Discrete (Maybe b)) where
    (<~~>) = (<?*->)

instance EasyApply (Signal (a -> b)) (Event a) (Event b) where
    (<~~>) = apply

-- Some instances which may be less common
instance EasyApply (Maybe (a -> b)) (Discrete a) (Discrete (Maybe b)) where
    Just f <~~> valD = Just . f <$> valD
    Nothing <~~> _ = return Nothing

-- Add more as necessary. TODO the application of some more brainpower
-- should be able to get all possible instances using type-level
-- programming, I think.

-- Evaluation control

-- | Forces the value in a Discrete.
forceD :: MonadSignalGen m => Discrete a -> m (Discrete a)
forceD aD = generatorD $ (\x -> x `seq` return x) <$> aD

-- | Like forceD, but for Event.
forceE :: MonadSignalGen m => Event a -> m (Event a)
forceE aE = generatorE $ (\x -> x `seq` return x) <$> aE

-- | Completely evaluates the value in a Discrete.
rnfD :: (NFData a, MonadSignalGen m) => Discrete a -> m (Discrete a)
rnfD = forceD . fmap force

-- | Like rnfD, but for Event.
rnfE :: (NFData a, MonadSignalGen m) => Event a -> m (Event a)
rnfE = forceE . fmap force

#if !MIN_VERSION_deepseq(1,2,0)
force :: NFData a => a -> a
force x = x `deepseq` x
#endif


--------------------------------------------------------------------------------
-- SignalSet

-- | A class of signal-like types.
class SignalSet a where
    -- | Create a dynamically switched @a@. The returned value doesn't need
    -- to be properly memoized. The user should call `switchD` instead.
    basicSwitchD :: MonadSignalGen m => Discrete a -> m a
    -- | Memoize a signal set.
    memoizeSignalSet :: MonadSignalGen m => a -> m a

instance SignalSet (Signal a) where
    basicSwitchD dis = return $ join $ discreteToSignalNoMemo dis
    memoizeSignalSet = memoS

instance SignalSet (Event a) where
    basicSwitchD dis = return $ joinEventSignal $ discreteToSignalNoMemo dis
    memoizeSignalSet = memoE

instance SignalSet (Discrete a) where
    basicSwitchD dis = return $ join dis
    memoizeSignalSet = memoD

instance (SignalSet a, SignalSet b) => SignalSet (a, b) where
    basicSwitchD dis = (,)
        <$> (basicSwitchD $ fst <$> dis)
        <*> (basicSwitchD $ snd <$> dis)
    memoizeSignalSet (x, y) = (,) <$> memoizeSignalSet x <*> memoizeSignalSet y

instance (SignalSet a, SignalSet b, SignalSet c) => SignalSet (a, b, c) where
    basicSwitchD dis = (,,)
        <$> (basicSwitchD $ e30 <$> dis)
        <*> (basicSwitchD $ e31 <$> dis)
        <*> (basicSwitchD $ e32 <$> dis)
        where
            e30 (a, _, _) = a
            e31 (_, a, _) = a
            e32 (_, _, a) = a
    memoizeSignalSet (x, y, z) =
        (,,) <$> memoizeSignalSet x <*> memoizeSignalSet y <*> memoizeSignalSet z

instance (SignalSet a, SignalSet b, SignalSet c, SignalSet d) =>
        SignalSet (a, b, c, d) where
    basicSwitchD dis = (,,,)
        <$> (basicSwitchD $ e40 <$> dis)
        <*> (basicSwitchD $ e41 <$> dis)
        <*> (basicSwitchD $ e42 <$> dis)
        <*> (basicSwitchD $ e43 <$> dis)
        where
            e40 (a, _, _, _) = a
            e41 (_, a, _, _) = a
            e42 (_, _, a, _) = a
            e43 (_, _, _, a) = a
    memoizeSignalSet (x0, x1, x2, x3) = (,,,)
      <$> memoizeSignalSet x0
      <*> memoizeSignalSet x1
      <*> memoizeSignalSet x2
      <*> memoizeSignalSet x3

instance (SignalSet a, SignalSet b, SignalSet c, SignalSet d, SignalSet e) =>
        SignalSet (a, b, c, d, e) where
    basicSwitchD dis = (,,,,)
        <$> (basicSwitchD $ e50 <$> dis)
        <*> (basicSwitchD $ e51 <$> dis)
        <*> (basicSwitchD $ e52 <$> dis)
        <*> (basicSwitchD $ e53 <$> dis)
        <*> (basicSwitchD $ e54 <$> dis)
        where
            e50 (a, _, _, _, _) = a
            e51 (_, a, _, _, _) = a
            e52 (_, _, a, _, _) = a
            e53 (_, _, _, a, _) = a
            e54 (_, _, _, _, a) = a
    memoizeSignalSet (x0, x1, x2, x3, x4) = (,,,,)
      <$> memoizeSignalSet x0
      <*> memoizeSignalSet x1
      <*> memoizeSignalSet x2
      <*> memoizeSignalSet x3
      <*> memoizeSignalSet x4

-- | discreteToSignal outside the SignalGen monad.
-- A careless use leads to repeated computation.
discreteToSignalNoMemo :: Discrete a -> Signal a
discreteToSignalNoMemo (Discrete x) = snd <$> x

--------------------------------------------------------------------------------
-- Testing

signalFromList :: [a] -> SignalGen (Signal a)
signalFromList list = fmap hd <$> stateful list tl
    where
        hd [] = error "signalFromList: list exhausted"
        hd (x:_) = x

        tl [] = error "signalFromList: list exhausted"
        tl (_:xs) = xs

eventFromList :: [[a]] -> SignalGen (Event a)
eventFromList list = Event <$> signalFromList (list ++ repeat [])

networkToList :: Int -> SignalGen (Signal a) -> IO [a]
networkToList n network = do
    sample <- start network
    replicateM n sample

-- vim: ts=2 sts=2

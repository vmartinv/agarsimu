{-# LANGUAGE Arrows #-}
{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
-- |
-- Module:     AgarSimu.Utils
-- Copyright:  (c) 2015 Martin Villagra
-- License:    BSD3
-- Maintainer: Martin Villagra <mvillagra0@gmail.com>

module AgarSimu.Utils
    ( -- * Wire Transformers
      mkGen_'
    , addFeedBack
    , combine
    , multicast
    , dynMulticast
    , foldlWire
    , addMonad
    , delRandom
            
      -- * General Wire Runner
    , runWire
      
      -- * Miscellaneous
    , clamp
    , tmap
    )
    where

import Prelude hiding ((.), id, until)
import Data.Either
import Data.Traversable hiding (for)
import Control.Monad.Fix (MonadFix)
import Control.Wire
import Control.Wire.Unsafe.Event
import Control.Monad.Random
import Control.Monad.IO.Class

mkGen_' :: Monad m => (a -> m b) -> Wire s e m a b
mkGen_' f = let f' x = fmap Right (f x)
            in mkGen_ f'

-- | Create a simple loop with a wire.
addFeedBack :: (MonadFix m, Monad m) => a -> Wire s e m a a -> Wire s e m a' a
addFeedBack init w = proc _ -> do
        rec input <- w . delay init -< input
        returnA -< input
    --or equivalently: loop (arr (\x->(x,x)) . w . delay init . arr snd)

-- | Combine a list of wires. Supply input for each wire, get the outputs
-- Wires that inhibit are deleted
combine :: (Monad m, Monoid s) => [Wire s e m a b] -> Wire s e m [a] [b]
combine ws = mkGen $ \dt xs -> do
        let stepper (w, x) = stepWire w dt (Right x)
        res <- mapM stepper (zip ws xs)
        let (outputs, ws') = unzip $ filter (isRight.fst) res
        return (Right (rights outputs), combine ws')

-- | Static set of wires.
-- Wires that inhibit are deleted
multicast :: (Monad m, Monoid s) => [Wire s e m a b] -> Wire s e m a [b]
multicast ws = mkGen $ \dt x -> do
            res <- mapM (\w -> stepWire w dt (Right x)) ws
            let (outputs, ws') = unzip $ filter (isRight.fst) res
            return (Right (rights outputs), multicast $ ws')

-- | Dynamic set of wires. Wires are created with the second input
-- Wires that inhibit are deleted
dynMulticast :: (Monad m, Monoid s, Monoid e) => Wire s e m (a, Event (Wire s e m a b)) [b]
dynMulticast = krSwitch (pure []) . second (mkSF_ (fmap addWire))
    where addWire :: (Monad m, Monoid s, Monoid e) => Wire s e m a b -> Wire s e m a [b] -> Wire s e m a [b]
          addWire w ws = let mw = fmap Just w --> pure Nothing
                             f :: (Maybe b, [b]) -> [b]
                             f = uncurry $ maybe id (:)
                         in fmap f (mw &&& ws)

foldlWire :: Foldable t => (c -> b -> a -> b) -> b -> Wire s e m (c, t a) b
foldlWire f i = mkSFN $ \(x, xs) -> let  i' = foldl (f x) i xs
                                    in (i', foldlWire f i') 

addMonad :: Monad m => WireP s e a b -> Wire s e m a b
addMonad = mapWire $ return.runIdentity

delRandom :: (Monad m, Monoid s) =>
    g -> Wire s e (Rand g) a b -> Wire s e m a b
delRandom gen w = mkGen $ \dt x -> do
            let ((y, w'), gen') = runRand (stepWire w dt (Right x)) gen
            return (y, delRandom gen' w')


-- | This function runs the given wire using the given state delta
-- generator. Press Ctrl-C to abort, closes on inhibition.
runWire ::
    (Monad m', MonadIO m)
    => (forall a. m' a -> m a)
    -> Session m s
    -> (forall a. Wire s e m' a b)
    -> m e
runWire run s0 w0 = loop s0 w0
    where loop s' w' = do
            (ds, s) <- stepSession s'
            (mx, w) <- run (stepWire w' ds (Right ()))
            case mx of
                Right _ -> loop s w
                Left x -> return x

clamp :: Ord a => a -> a -> a -> a
clamp mn mx = max mn . min mx

tmap :: (a -> b) -> (a, a) -> (b, b)
tmap f (x, y) = (f x, f y)

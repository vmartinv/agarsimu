{-# LANGUAGE Arrows #-}
{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
-- |
-- Module:     AgarSimu.Utils
-- Copyright:  (c) 2015 Martin Villagra
-- License:    BSD3
-- Maintainer: Martin Villagra <mvillagra0@gmail.com>

module AgarSimu.Utils
    ( -- * Wire Transformers
      mkGen_',
      addFeedBack,
      combine,
      addMonad,
      delRandom,
      
      -- * Specialized Wires
      accumOutput,
      
      -- * General Wire Runner
      runAnimation,
      
      -- * Environment generator
      mkEnvs,
      clamp
    )
    where

import Prelude hiding ((.), id, until)
import Data.Either
import Data.Traversable
import Control.Monad.Fix (MonadFix)
import Control.Wire
import Control.Wire.Unsafe.Event
import Control.Monad.Random
import Control.Monad.IO.Class

mkGen_' :: Monad m => (a -> m b) -> Wire s e m a b
mkGen_' f = let f' x = fmap Right (f x)
            in mkGen_ f'

addFeedBack :: (MonadFix m, Monad m) => a -> Wire s e m a a -> Wire s e m a' a
addFeedBack init w = proc _ -> do
        rec input <- w . delay init -< input
        returnA -< input
    --or equivalently: loop (arr (\x->(x,x)) . w . delay init . arr snd)

combine :: (Monad m, Monoid s) => [Wire s e m a b] -> Wire s e m [a] [b]
combine ws = mkGen $ \dt xs -> do
        let stepper (w, x) = stepWire w dt (Right x)
        res <- mapM stepper (zip ws xs)
        let (outputs, ws') = unzip $ filter (isRight.fst) res
        return (Right (rights outputs), combine ws')

        
addMonad :: Monad m => WireP s e a b -> Wire s e m a b
addMonad = mapWire $ return.runIdentity

delRandom :: (Monad m, Monoid s) =>
    g -> Wire s e (Rand g) a b -> Wire s e m a b
delRandom gen w = mkGen $ \dt x -> do
            let ((y, w'), gen') = runRand (stepWire w dt (Right x)) gen
            return (y, delRandom gen' w')
        
accumOutput :: (Monad m, Monoid s) => a -> Wire s e m a b -> Wire s e m [a] b
accumOutput empty = loop
    where loop w = mkGen $ \dt inputs -> do
            let stepper x = stepWire w dt (Right x)
            let folder m z = do (_, w') <- m 
                                stepWire w' mempty (Right z)
            (y, w'') <- case inputs of
                          [] -> stepper empty
                          (x:xs) -> foldl folder (stepper x) xs
            return (y, loop w'')
            
-- | This function runs the given wire using the given state delta
-- generator. Press Ctrl-C to abort.

runAnimation ::
    (Monad m', MonadIO m)
    => (forall a. m' a -> m a)
    -> Session m s
    -> (forall a. Wire s e m' a b)
    -> m c
runAnimation run s0 w0 = loop s0 w0
    where
    loop s' w' = do
        (ds, s) <- stepSession s'
        (mx, w) <- run (stepWire w' ds (Right ()))
        loop s w


mkEnvs :: [a] -> [[a]]
mkEnvs xs = mkEnvs' [] xs
  where mkEnvs' izq [] = []
        mkEnvs' izq (x:der) = (izq++der):mkEnvs' (x:izq) der
        

clamp :: Ord a => a -> a -> a -> a
clamp mn mx = max mn . min mx

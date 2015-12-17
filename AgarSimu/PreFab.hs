{-# LANGUAGE Arrows #-}
-- |
-- Module:     AgarSimu.PreFab
-- Copyright:  (c) 2015 Martin Villagra
-- License:    BSD3
-- Maintainer: Martin Villagra <mvillagra0@gmail.com>

module AgarSimu.PreFab
    ( -- * Useful fuctions
      comibles
    , meComen
    , closest
    , biggest
    , chase
    
      -- * Wire constructor
    , liftAI
    , mkStateW
      
      -- * Random
    , randomW
    , randomWR
    , randomDir

      -- * Sample AIS 
    , go 
    , stop    
    , vibrar    
    , randomAI    
    , greedy    
    )
    where

import Prelude hiding ((.), id)
import Control.Monad.State
import Control.Monad.Random
import Data.List
import Data.VectorSpace
import Control.Lens hiding (at, perform, wrapped)
import Control.Wire
import AgarSimu.PublicEntities
import AgarSimu.Scene
import AgarSimu.Utils

comibles :: Bola -> [Bola] -> [Bola]
comibles yo otros = filter (yo `eats`) otros

meComen :: Bola -> [Bola] -> [Bola]
meComen yo otros = filter (`eats` yo) otros

closest :: Bola -> [Bola] -> Bola
closest yo otros = minimumBy byDist otros
    where byDist a b = compare (distBolas a yo) (distBolas b yo)
    
biggest :: [Bola] -> Bola
biggest otros = maximumBy byMass otros
    where byMass a b = compare (view bolMass a) (view bolMass b)

chase :: Bola -> Bola -> Vector
chase yo otro = normalized $ (uncurry (^-^)) $ view (bolPos `alongside` bolPos) (otro, yo)

liftAI :: (Vector -> Bola -> [Bola] -> Vector) -> RandomWire Environment Vector
liftAI f = mkSF_ $ \(ws, yo, otros) -> f ws yo otros
    
mkStateW :: m -> (Environment -> State m Vector) -> RandomWire Environment Vector
mkStateW init f = loop $ mkSF_ (uncurry runState) . first (mkSF_ f) . second (delay init)

randomW :: (MonadRandom m, Random b) => Wire s e m a b
randomW = mkConstM getRandom

randomWR :: (MonadRandom m, Random b) => (b, b) -> Wire s e m a b
randomWR (x, y) = mkConstM $ getRandomR (x, y)

randomDir :: RandomWire a Vector
randomDir = go . randomWR (-pi, pi)

go :: RandomWire Double Vector
go = arr sin &&& arr cos

stop :: RandomWire a Vector
stop = pure (0, 0)

vibrar :: RandomWire a Vector
vibrar = for 0.2 . pure (0, 1) --> for 0.2 . pure (0, -1) --> vibrar

randomAI :: RandomWire a Vector
randomAI = for 0.2 . hold . now . randomDir --> randomAI

greedy :: RandomWire Environment Vector
greedy = liftAI $ \wc yo otros -> 
    case comibles yo otros of
            [] -> (0, 0)
            xs -> chase yo (closest yo xs)

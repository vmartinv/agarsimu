-- |
-- Module:     AgarSimu.PreFab
-- Copyright:  (c) 2015 Martin Villagra
-- License:    BSD3
-- Maintainer: Martin Villagra <mvillagra0@gmail.com>

module AgarSimu.PreFab
    ( -- * Wire constructor
      mkConstM,
      
      -- * Random
      randomW,
      randomWR,    
      randomDir,

      -- * Basic Wires 
      go,    
      stop    
    )
    where

import Prelude hiding ((.), id)
import Control.Monad.Random
import Control.Wire
import AgarSimu.PublicEntities


mkConstM :: Monad m => m b -> Wire s e m a b
mkConstM = mkGen_ . const . fmap Right

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

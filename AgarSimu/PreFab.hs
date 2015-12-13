{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module:     AgarSimu.PreFab
-- Copyright:  (c) 2015 Martin Villagra
-- License:    BSD3
-- Maintainer: Martin Villagra <mvillagra0@gmail.com>

module AgarSimu.PreFab
    ( -- * Wire constructor
      mkConst',
      
      -- * Random
      randomW,
      randomWR,    
      randomDir,

      -- * Basic Wires 
      go,    
      stop    
    )
    where

import Control.Lens hiding (at, perform, wrapped)
import Control.Wire
import Control.Monad.Random
import AgarSimu.Utils
import AgarSimu.PublicEntities
import Prelude hiding ((.), id)


mkConst' :: Monad m => m b -> Wire s e m a b
mkConst' = mkGen_.const.fmap Right

randomW :: (MonadRandom m, Random b) => Wire s e m a b
randomW = mkConst' getRandom
randomWR :: (MonadRandom m, Random b) => (b, b) -> Wire s e m a b
randomWR (x, y) = mkConst' $ getRandomR (x, y)

randomDir :: RandomWire a Vector
randomDir = go . randomWR (-pi, pi)

go :: RandomWire Double Vector
go = arr sin &&& arr cos

stop :: RandomWire a Vector
stop = pure (0, 0)

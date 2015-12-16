{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
-- Module:     AgarSimu.Scene
-- Copyright:  (c) 2015 Martin Villagra
-- License:    BSD3
-- Maintainer: Martin Villagra <mvillagra0@gmail.com>

module AgarSimu.Scene
    ( -- * World
      WorldConsts(..)
    , worlSize, worlWindowSize
    
      -- * Scene
    , Scene
    , Builder
    , setWindowSize
    , setWorldSize
    
    , newBola
    , randomBola
    , newBolas
    , addBola
    , addBolas
    , versus
    , getScene
    )
    where

import Prelude
import Control.Monad.State
import Control.Monad.Random
import Control.Arrow
import Control.Lens hiding (at, perform, wrapped)
import AgarSimu.PublicEntities
import qualified Graphics.UI.SDL as SDL (Pixel(..))

data WorldConsts = WorldConsts { _worlSize :: !Vector
                               , _worlWindowSize :: !(Int, Int)
                               } deriving Show
$(makeLenses ''WorldConsts)

type Scene = (WorldConsts, [(AI, Bola)])
type Builder a = StateT Scene (Rand StdGen) a

setWindowSize :: (Int, Int) -> Builder ()
setWindowSize ws = modify $ first (set worlWindowSize ws)

setWorldSize :: Vector -> Builder ()
setWorldSize ws = modify $ first (set worlSize ws)

newBola :: (Double, Double) -> Builder Bola
newBola range = do ws <- gets $ (view worlSize).fst
                   mass <- getRandomR range
                   randomBola ws mass

randomBola :: MonadRandom m => (Double, Double) -> Double -> m Bola
randomBola (wx, wy) m = let r = massToRadio m 
                        in do col <- randomColor
                              x <- getRandomR (r, wx-r)
                              y <- getRandomR (r, wy-r)
                              return $ Bola col (x, y) m

newBolas :: Int -> (Double, Double) -> Builder [Bola]
newBolas n range = sequence $ replicate n (newBola range)

addBola :: AI -> Bola -> Builder ()
addBola ai bola = modify $ second ((ai, bola):)

addBolas :: AI -> [Bola] -> Builder ()
addBolas ai bolas = sequence (map (addBola ai) bolas) >> return ()

versus :: AI -> AI -> Builder ()
versus x y = do setWorldSize (300, 300)
                setWindowSize (600, 600)
                bolasA <- newBolas 20 (15, 30)
                bolasB <- newBolas 20 (15, 30)
                let colA = SDL.Pixel 0xff0000ff
                    colB = SDL.Pixel 0x0000ffff
                addBolas x (map (set bolColor colA) bolasA)
                addBolas y (map (set bolColor colB) bolasB)

getScene :: Builder a -> StdGen -> (Scene, StdGen)
getScene b g = runRand (execStateT b (defWorldConsts, [])) g
    where defWorldConsts = WorldConsts (100, 100) (400, 400)


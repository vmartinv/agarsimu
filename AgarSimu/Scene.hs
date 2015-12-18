-- Module:     AgarSimu.Scene
-- Copyright:  (c) 2015 Martin Villagra
-- License:    BSD3
-- Maintainer: Martin Villagra <mvillagra0@gmail.com>

module AgarSimu.Scene
    ( -- * Scene
      Scene
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
versus ai1 ai2 = do setWorldSize (200, 200)
                    setWindowSize (1024, 768)
                    bolas1 <- newBolas 20 (15, 150)
                    bolas2 <- newBolas 20 (15, 150)
                    let col1 = rgb 0x07 0xff 0xb0
                        col2 = rgb 0xe4 0x07 0xff
                    addBolas ai1 (map (set bolColor col1) bolas1)
                    addBolas ai2 (map (set bolColor col2) bolas2)

getScene :: Builder a -> StdGen -> (Scene, StdGen)
getScene b g = runRand (execStateT b (defWorldConsts, [])) g
    where defWorldConsts = WorldConsts (100, 100) (400, 400)


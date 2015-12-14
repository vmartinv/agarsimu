{-# LANGUAGE Arrows #-}
-- Module:     AgarSimu.Core
-- Copyright:  (c) 2015 Martin Villagra
-- License:    BSD3
-- Maintainer: Martin Villagra <mvillagra0@gmail.com>

module AgarSimu.Core
    ( -- * Runner
      runSimulation
    )
    where

import Prelude hiding ((.), id, until)
import Control.Monad.Random
import Control.Lens hiding (at, perform, wrapped)
import Control.Wire
import AgarSimu.Bola
import AgarSimu.Input
import AgarSimu.PreFab
import AgarSimu.PublicEntities
import AgarSimu.Render
import AgarSimu.Utils

defFPS :: Num a => a
defFPS = 60

runSimulation :: WorldConsts -> Scene -> IO ()
runSimulation wc scene = withPrepareRendering wc defFPS $ \cam -> do
        let inputwire = inputLogic cam
        g <- getStdGen
        let gamewire = delRandom g (gameLogic wc scene)
        let renderwire = renderLogic wc
        let mainwire = proc x -> do
                (camera, speed) <- inputwire -< x
                frame <- gamewire -< x
                renderwire -< (camera, frame, speed)
        runAnimation id (countSession_ $ 1/defFPS) mainwire
    where (ais, players) = unzip scene
          (vx,vy) = view worlWindowSize wc
    
gameLogic :: WorldConsts -> Scene -> RandomWire a [Bola]
gameLogic wc scene = proc _ -> do
        rec
            oldBolas <- delay inits -< bolas
            oldComida <- delay [] -< comida
            bolas <- aiswire -< map (++oldComida) (mkEnvs oldBolas)
            comida <- foodwire . (id &&& genFood) -< oldBolas
        returnA -< bolas++comida
    where (wx, wy) = view worlSize wc
          inits = map snd scene
          aiswire = combine $ map (bolaLogic wc) scene
          foodwire = dynMulticast [] 
          genFood = periodic prob . fmap comidaLogic (mkConstM (randomBola (wx, wy) 1))
          prob = realToFrac $ 1/(0.0005 *  wx * wy)

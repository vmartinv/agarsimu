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
import AgarSimu.Scene
import AgarSimu.Utils

import FRP.Netwire.Noise
import qualified Graphics.UI.SDL as SDL (Pixel(..))

defFPS :: Num a => a
defFPS = 60

runSimulation :: Builder a -> IO ()
runSimulation builder = do
    g' <- getStdGen
    let ((wc,players), g) = getScene builder g'
    withPrepareRendering wc defFPS $ \cam -> do
        let inputwire = inputLogic cam
        let gamewire = delRandom g (gameLogic (wc,players))
        let renderwire = renderLogic wc
        let mainwire = proc x -> do
                (camera, speed) <- inputwire -< x
                frame <- gamewire -< x
                renderwire -< (camera, frame, speed)
        runWire id (countSession_ $ 1/defFPS) mainwire

gameLogic :: Scene -> RandomWire a [Bola]
gameLogic (wc, players) = proc _ -> do
        rec
            oldBolas <- delay inits -< bolas
            food <- foodGenerator (view worlSize wc) -< oldBolas
            bolas <- aiswire -< map (++food) (mkEnvs oldBolas)
        returnA -<  food ++ bolas
    where inits = map snd players
          aiswire = combine $ map (bolaLogic wc) players

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
import qualified Prelude
import Control.Lens hiding (at, perform, wrapped)
import Control.Monad (void)
import Control.Wire
import Data.VectorSpace ((^+^), (^-^), normalized, magnitude, (*^), (^/))
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDL
import qualified Graphics.UI.SDL.Framerate as Framerate
import AgarSimu.PublicEntities
import AgarSimu.Entities
import AgarSimu.Utils
import Data.Maybe
import Control.Monad.Random

defFPS :: Num a => a
defFPS = 60

runSimulation :: WorldConsts -> Scene -> IO ()
runSimulation wc scene = SDL.withInit [SDL.InitEverything] $ do
        screen <- SDL.setVideoMode x y 0 [SDL.SWSurface]--, SDL.Fullscreen]
        frameRate <- Framerate.new
        Framerate.init frameRate
        Framerate.set frameRate defFPS
        let inputwire = inputLogic wc
        g <- getStdGen
        let gamewire = delRandom g (gameLogic wc scene)
        let renderwire = renderLogic frameRate wc screen
        let mainwire = proc x -> do
                camera <- inputwire -< x
                frame <- gamewire -< x
                renderwire -< (camera, frame)
        testWireM id (countSession_ 0.01) (pure () . mainwire)
    where (ais, players) = unzip scene
          (x,y) = view worlWindowSize wc
          speed = view worlSpeed wc

gameLogic :: WorldConsts -> Scene -> RandomWire a [Bola]
gameLogic wc scene = proc oldBolas -> do
        rec
            oldBolas <- delay (map snd scene) -< bolas
            let envs = zip oldBolas (mkEnvs oldBolas)
            bolas <- aiswire -< map (\(m,o)-> (wc, m, o)) envs
        returnA -< bolas
    where aiswire = combine $ map bolaLogic scene

mkEnvs :: [a] -> [[a]]
mkEnvs xs = mkEnvs' [] xs
  where mkEnvs' izq [] = []
        mkEnvs' izq (x:der) = (izq++der):mkEnvs' (x:izq) der

bolaLogic :: (AI, Bola) -> RandomWire Environment Bola
bolaLogic (ai, init) = mkSF_ fromJust . when (isJust) . proc (wc, yo, otros) -> do
        v <- ai -< (wc, yo, otros)
        pos <- integralVecWith clampW initV -< (mkBolaVec yo v, (wc, getRadio yo))
        let collBola = collideBola otros yo
        let movedBola = fmap (set bolPos pos) collBola
        returnA -< movedBola
    where initV = view bolPos init
          clampW (wc, r) v = clampCircle wc r v

integralVecWith :: HasTime t s
    => (w -> Vector -> Vector)  -- ^ Correction function.
    -> Vector                   -- ^ Integration constant (aka start value).
    -> Wire s e m (Vector, w) Vector
integralVecWith correct = loop
    where
    loop x' =
        mkPure $ \ds (dx, w) ->
            let dt = realToFrac (dtime ds)
                x  = correct w (x' ^+^ dt*^dx)
            in x' `seq` (Right x', loop x)

renderLogic :: Monoid e =>
    Framerate.FPSManager ->
    WorldConsts ->
    SDL.Surface -> 
    Wire s e IO (Camera, [Bola]) ()
renderLogic frameRate wc screen = proc frame -> do
        rec elapsed <- delay 0 -< (elapsed+1) `mod` (view worlSpeed wc)
        if elapsed==0
            then renderFrame -< frame
            else returnA -< ()
    where renderFrame = mkGen_' $ \(cam, bolas) -> do
            SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 100 100 100 >>=
                SDL.fillRect screen Nothing
            mapM (renderBola screen cam) bolas
            renderBorderBox wc screen cam
            SDL.flip screen
            Framerate.delay frameRate
            return ()

inputLogic :: (Monoid s, Monoid e) => WorldConsts -> Wire s e IO a Camera
inputLogic w = addMonad (accumOutput SDL.NoEvent (mouseCam (defCam w))) . readEvents

mouseCam :: Monoid e => Camera -> WireP s e SDL.Event Camera                           
mouseCam init = proc event -> do
        rec
            oldCam <- delay init -< cam
            pressed <- leftClickState -< event
            cam <- returnA -< case event of
                SDL.MouseButtonDown _ _ SDL.ButtonWheelUp -> camZoomIn oldCam
                SDL.MouseButtonDown _ _ SDL.ButtonWheelDown -> camZoomOut oldCam
                SDL.MouseMotion _ _ x y | pressed -> camMove oldCam (fromIntegral x, fromIntegral y)
                _ -> oldCam
        returnA -< cam

leftClickState :: Monoid e => WireP s e SDL.Event Bool
leftClickState =  between . flat . (pure True &&& became isLeftButtonDown &&& became isLeftButtonUp)
              <|> pure False
    where
        flat = mkSF_ (\(a, (b, c))-> (a, b, c))
        isLeftButtonDown (SDL.MouseButtonDown _ _ SDL.ButtonLeft) = True
        isLeftButtonDown _ = False
        isLeftButtonUp (SDL.MouseButtonUp _ _ SDL.ButtonLeft) = True
        isLeftButtonUp _ = False

readEvents :: Monoid e => Wire s e IO a [SDL.Event]
readEvents = mkGen_' $ const (acum [])
    where acum evs = do ev <- SDL.pollEvent
                        case ev of
                            SDL.NoEvent -> return evs
                            _ -> acum (ev:evs)

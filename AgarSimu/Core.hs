{-# LANGUAGE Arrows #-}
{-# LANGUAGE StandaloneDeriving #-}
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
import Control.Wire.Switch
import FRP.Netwire
import Data.VectorSpace ((^+^), (^-^), normalized, magnitude, (*^), (^/))
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDL
import qualified Graphics.UI.SDL.Framerate as Framerate
import AgarSimu.PublicEntities
import AgarSimu.Entities
import AgarSimu.Utils
import AgarSimu.PreFab
import Data.Maybe
import Control.Monad.Random
import Control.Wire.Unsafe.Event
defFPS :: Num a => a
defFPS = 60

deriving instance Show a => Show (Event a)
runSimulation :: WorldConsts -> Scene -> IO ()
runSimulation wc scene = SDL.withInit [SDL.InitEverything] $ do
        screen <- SDL.setVideoMode x y 0 [SDL.SWSurface]--, SDL.Fullscreen]
        let cam = defCam wc
        frameRate <- Framerate.new
        Framerate.init frameRate
        Framerate.set frameRate defFPS
        let inputwire = inputLogic cam
        g <- getStdGen
        -- ~ let testwire = delRandom g (holdFor 1. wackelkontakt' 0.5 0.1 . pure 3)
        let gamewire = delRandom g (gameLogic wc scene)
        let renderwire = renderLogic frameRate wc screen
        let mainwire = proc x -> do
                camera <- inputwire -< x
                frame <- gamewire -< x
                renderwire -< (camera, frame)
        runAnimation id (countSession_ $ 1/defFPS) mainwire
        -- ~ testWireM id (clockSession_) testwire
    where (ais, players) = unzip scene
          (x,y) = view worlWindowSize wc
          speed = view worlSpeed wc


-- ~ wackelkontakt' :: NominalDiffTime -> Double -> RandomWire a (Event a)
-- ~ wackelkontakt' t p = mkSF_ (\(ev, x)->fmap (const x) ev) . (filterE (<p) . periodic t . randomProb &&& id)
        -- ~ where randomProb :: RandomWire a Double
              -- ~ randomProb = randomWR . pure (0, 1)
    
comidaLogic :: Bola -> RandomWire [Bola] Bola
comidaLogic init = pure init . when (isJust) . mkSF_ (flip collideBola init)

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
          genFood = periodic prob . fmap comidaLogic (mkConst' (randomBola (wx, wy) 1))
          prob = realToFrac $ 1/(0.0005 *  wx * wy)


bolaLogic :: WorldConsts -> (AI, Bola) -> RandomWire [Bola] Bola
bolaLogic wc (ai, init) = proc (otros) -> do
        rec
            oldYo <- delay init -< yo
            --Update Mass
            oldRealMass <- delay (view bolMass init) -< realMass
            increment <- mkSF_ (fromJust) . when (isJust) -< collideBola otros oldYo
            realMass <- returnA -< oldRealMass+increment
            varMass <- integralWith min (view bolMass init) -< (realMass, realMass)
            let yo' = set bolMass varMass oldYo

            --Update Position
            v <- ai -< ((wx, wy), yo', otros)
            let v' = mkBolaVec yo' v
            pos <- integralVecWith clampCircle initV -< (v', getRadio yo')
            yo <- returnA -< set bolPos pos yo'
        returnA -< yo
    where initV = view bolPos init
          (wx, wy) = view worlSize wc
          clampCircle r (x, y) = (clamp r (wx-r) x, clamp r (wy-r) y)

integralVecWith :: HasTime t s
    => (w -> Vector -> Vector)  -- ^ Correction function.
    -> Vector                   -- ^ Integration constant (aka start value).
    -> Wire s e m (Vector, w) Vector
integralVecWith correct = loop
    where loop x' = mkPure $ \ds (dx, w) ->
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
            SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0 0 0 >>=
                SDL.fillRect screen Nothing
            renderBackground wc screen cam
            mapM (renderBola screen cam) bolas
            SDL.flip screen
            Framerate.delay frameRate

inputLogic :: (Monoid s, Monoid e) => Camera -> Wire s e IO a Camera
inputLogic cam = addMonad (accumOutput SDL.NoEvent (mouseCam cam)) . readEvents

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
                            
-- ~ readEvent :: Monoid e => Wire s e IO a SDL.Event
-- ~ readEvent = mkGen_' $ const SDL.pollEvent

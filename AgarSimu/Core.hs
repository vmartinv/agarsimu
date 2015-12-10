{-# LANGUAGE Arrows #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
module AgarSimu.Core where

import Prelude hiding ((.), id, until)
import qualified Prelude
import Control.Lens hiding (at, perform, wrapped)
import Control.Monad (void)
import Control.Wire
import Data.VectorSpace ((^+^), (^-^), normalized, magnitude, (*^), (^/))
import qualified Data.Set as Set
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDL
import AgarSimu.Entities
import AgarSimu.Utils
import Control.Monad.Random

import Control.Wire.Unsafe.Event

instance (Random x, Random y) => Random (x, y) where
  random gen1 =
    let (x, gen2) = random gen1
        (y, gen3) = random gen2
    in ((x, y), gen3)
  randomR ((x1, y1), (x2, y2)) gen1 =
    let (x, gen2) = randomR (x1, x2) gen1
        (y, gen3) = randomR (y1, y2) gen2
    in ((x, y), gen3)
    
getRandomAI :: (MonadRandom m, Random b) => Wire s e m a b
getRandomAI = mkGen_' $ const getRandom
getRandomRAI :: (MonadRandom m, Random b) => (b, b) -> Wire s e m a b
getRandomRAI x = mkGen_' $ const (getRandomR x)

--------------------------------------
holdFirst :: Wire s e m a a
holdFirst = mkSFN $ \x -> (x, mkConst (Right x))
                  
type Environment = (Player, [Player])
type RandomWire = Wire (Timed NominalDiffTime ()) () (Rand StdGen)
type AI = RandomWire Environment Vector
type Scene = [(AI, Player)]

------------------------------------------
runSimulation :: WorldConsts -> Scene -> IO ()
runSimulation w scene = SDL.withInit [SDL.InitEverything] $ do
        screen <- SDL.setVideoMode x y 0 [SDL.SWSurface]--, SDL.Fullscreen]
        let inputwire = inputLogic w
        g <- getStdGen
        let gamewire = delRandom g (addBaseCase players (gameLogic w ais))
        let renderwire = renderLogic w screen
        let mainwire = proc x -> do
                camera <- inputwire -< x
                frame <- gamewire -< x
                renderwire -< (camera, frame)
        testWireM id (countSession_ (1.0 / fps)) (pure () . mainwire)
    where (ais, players) = unzip scene
          (x,y) = view worlWindowSize w
          fps = fromIntegral (view worlFPS w)
          duplicate = \x-> (x, x)

gameLogic :: WorldConsts -> [AI] -> RandomWire [Player] [Player]
gameLogic w ais = proc oldPlayers -> do
        newPlayers <- aiswire -< makeEnvs id oldPlayers
        players <- mkSF_ collidePlayers -< newPlayers
        returnA -< players
        where liftAI ai = playerLogic w . (mkSF_ fst &&& ai)
              aiswire = multicast $ map liftAI ais

normalize :: Vector -> Vector
normalize v = let m = magnitude v
              in if m==0 then v else v^/m

playerLogic :: WorldConsts -> RandomWire (Player, Vector) Player
playerLogic w = when (not.died) . proc (yo, v) -> do
        returnA -< movePlayer w (normalize v) yo
    where died p = length (view plaBolas p) == 0


renderLogic :: WorldConsts -> SDL.Surface -> Wire s e IO (Camera, [Player]) (Camera, [Player])
renderLogic w screen = mkGen_' $ \(cam, players) -> do
            SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 100 100 100 >>=
                SDL.fillRect screen Nothing
            mapM (renderPlayer screen cam) players
            renderBorderBox w screen cam
            SDL.flip screen
            SDL.delay (1000 `div` fps)
            return (cam, players)
    where fps = fromIntegral (view worlFPS w)

-- ~ inputLogic :: Monoid e => WorldConsts -> Wire s e IO a Camera
-- ~ inputLogic w = mkSF_ snd . addFeedback (False, defCam w) (mkGen_' mouseCam)
inputLogic :: (Monoid s, Monoid e) => WorldConsts -> Wire s e IO a Camera
inputLogic w = addMonad (useLast $ mouseCam (defCam w)) . readEvents

-- ~ updateCam :: Bool -> SDL.Event -> Camera -> Camera  
-- ~ updateCam _ (SDL.MouseButtonDown _ _ SDL.ButtonWheelUp) oldCam = camZoomIn oldCam
-- ~ updateCam _ (SDL.MouseButtonDown _ _ SDL.ButtonWheelDown) oldCam = camZoomOut oldCam
-- ~ updateCam True (SDL.MouseMotion _ _ x y) oldCam = camMove oldCam (fromIntegral x, fromIntegral y)
-- ~ updateCam _ _ oldCam = oldCam

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
                            SDL.NoEvent -> return (ev:evs)
                            _ -> acum (ev:evs)


--------------------------------------------------------------------------------
-- ~ deriving instance Ord SDL.Keysym
-- ~ parseEvents :: Set.Set SDL.Keysym -> IO (Set.Set SDL.Keysym)
-- ~ parseEvents keysDown = do
  -- ~ event <- SDL.pollEvent
  -- ~ case event of
    -- ~ SDL.NoEvent -> return keysDown
    -- ~ SDL.KeyDown k -> parseEvents (Set.insert k keysDown)
    -- ~ SDL.KeyUp k -> parseEvents (Set.delete k keysDown)
    -- ~ _ -> parseEvents keysDown

-- ~ keyDown :: Foldable f => SDL.SDLKey -> f SDL.Keysym -> Bool
-- ~ keyDown = elemOf (folded . to SDL.symKey)



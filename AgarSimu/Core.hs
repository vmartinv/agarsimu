{-# LANGUAGE Arrows #-}
{-# LANGUAGE StandaloneDeriving #-}
module AgarSimu.Core where

import Prelude hiding ((.), id, until)
import qualified Prelude
import Control.Lens hiding (at, perform, wrapped)
import Control.Monad (void)
import Control.Wire
import Data.VectorSpace ((^+^), (^-^), normalized, (*^))
import qualified Data.Set as Set
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDL
import AgarSimu.Entities
import AgarSimu.Utils

type Environment = (Player, [Player])
type Scene = [(AI, Player)]
type AI = SimpleWire Environment Vector

--------------------------------------------------------------------------------
runSimulation :: WorldConsts -> Scene -> IO ()
runSimulation w scene = SDL.withInit [SDL.InitEverything] $ do
        let (x,y) = view worlWindowSize w
        let fps = fromIntegral (view worlFPS w)
        screen <- SDL.setVideoMode x y 0 [SDL.SWSurface]--, SDL.Fullscreen]
        let gamewire = addFeedback players (game w ais)
        let mainwire = showGame w screen gamewire . (mouseCamWire w)
        testWireM id (countSession_ (1.0 / fps)) (pure () . mainwire)
    where (ais, players) = unzip scene
          (x,y) = view worlWindowSize w
          fps = fromIntegral (view worlFPS w)

renderFrame :: WorldConsts -> SDL.Surface -> Camera -> [Player] -> IO ()
renderFrame w screen cam players = do
            void $ SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0 0 0 >>=
                SDL.fillRect screen Nothing
            mapM_ (renderPlayer screen cam) players
            SDL.flip screen
            SDL.delay (1000 `div` fps)
    where fps = fromIntegral (view worlFPS w)

mouseCamWire :: WorldConsts -> Wire s e IO a Camera
mouseCamWire w = mkSF_ snd . addFeedback (False, defCam w) (mkGen_' mouseCam)

mouseCam :: (Bool, Camera) -> IO (Bool, Camera)
mouseCam st@(b, cam) = do
  event <- SDL.pollEvent
  case event of
    SDL.NoEvent -> return st
    SDL.MouseButtonDown _ _ SDL.ButtonLeft -> mouseCam (True, cam)
    SDL.MouseButtonUp _ _ SDL.ButtonLeft -> mouseCam (False, cam)
    SDL.MouseMotion _ _ x y | b -> mouseCam (b, camMove cam (fromIntegral x, fromIntegral y))
    SDL.MouseButtonDown _ _ SDL.ButtonWheelUp -> mouseCam (b, camZoomIn cam)
    SDL.MouseButtonDown _ _ SDL.ButtonWheelDown -> mouseCam (b, camZoomOut cam)
    _ -> mouseCam st

showGame :: WorldConsts -> SDL.Surface -> WireP s e () [Player] -> Wire s e IO Camera [Player]
showGame w screen game = proc cam -> do
    let cami = cam
    newPlayers <- addMonad game -< ()
    mkGen_' (uncurry $ renderFrame w screen) -< (cam, newPlayers)
    returnA -< newPlayers

addMonad :: (Monad m) => Wire s e Identity a b -> Wire s e m a b
addMonad = mapWire (return.runIdentity)

game :: WorldConsts -> [AI] -> SimpleWire [Player] [Player]
game w ais = proc oldPlayers -> do
        newPlayers <- aiswire -< makeEnvs id oldPlayers
        players <- mkSF_ collidePlayers -< newPlayers
        returnA -< players
        where aiswire = multicast $ map (liftAI w) ais

liftAI :: WorldConsts -> AI -> SimpleWire Environment Player
liftAI w ai = when (not.died) . proc (yo, otros) -> do
        v <- ai -< (yo, otros)
        returnA -< movePlayer w v yo
    where died p = length (view plaBolas p) == 0

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



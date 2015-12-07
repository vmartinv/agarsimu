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
runSimulation w scene = do
    SDL.withInit [SDL.InitEverything] $ do
        let (x,y) = view worlWindowSize w
        let fps = fromIntegral (view worlFPS w)
        screen <- SDL.setVideoMode x y 0 [SDL.SWSurface]--, SDL.Fullscreen]
        let gamewire = addFeedback players (game w ais)
        let controlswire = addFeedback Set.empty (mkGen_' parseEvents)
        let mainwire = showGame w screen gamewire . controlswire
        testWireM id (countSession_ (1.0 / fps)) (pure () . mainwire)
    where (ais, players) = unzip scene

renderFrame :: WorldConsts -> SDL.Surface -> Camera -> [Player] -> IO ()
renderFrame w screen cam players = do
            void $ SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0 0 0 >>=
                SDL.fillRect screen Nothing
            mapM_ (renderPlayer screen cam) players
            SDL.flip screen
            SDL.delay (1000 `div` fromIntegral (view worlFPS w))

showGame :: WorldConsts -> SDL.Surface -> WireP s e () [Player] -> Wire s e IO (Set.Set SDL.Keysym) [Player]
showGame w screen game = proc keys -> do
    newPlayers <- addMonad game -< ()
    mkGen_' (renderFrame w screen (defCam w)) -< newPlayers
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
deriving instance Ord SDL.Keysym
parseEvents :: Set.Set SDL.Keysym -> IO (Set.Set SDL.Keysym)
parseEvents keysDown = do
  event <- SDL.pollEvent
  case event of
    SDL.NoEvent -> return keysDown
    SDL.KeyDown k -> parseEvents (Set.insert k keysDown)
    SDL.KeyUp k -> parseEvents (Set.delete k keysDown)
    _ -> parseEvents keysDown

keyDown :: Foldable f => SDL.SDLKey -> f SDL.Keysym -> Bool
keyDown = elemOf (folded . to SDL.symKey)



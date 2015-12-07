{-# LANGUAGE Arrows #-}
{-# LANGUAGE StandaloneDeriving #-}
module AgarSimu.Core where

import Prelude hiding ((.), id)
import qualified Prelude
import Control.Lens hiding (at, perform, wrapped)
import Control.Monad (void)
import Data.Either
import Control.Wire hiding (until)
import Data.VectorSpace ((^+^), (^-^), normalized, (*^))
import qualified Data.Set as Set
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDL
import AgarSimu.Entities

type Environment = (Player, [Player])
type Scene s = [(AI s, Player)]
type AI s =  Wire s () Identity Environment Vector

defWorldConsts :: WorldConsts
defWorldConsts = WorldConsts (400, 400) (400, 400) 0.0016 60

defCam :: WorldConsts -> Camera
defCam w = Camera (0, 0) 1
--------------------------------------------------------------------------------
runSimulation :: WorldConsts -> Scene (Timed Double ()) -> IO ()
runSimulation w scene = do
    SDL.withInit [SDL.InitEverything] $ do
        let (x,y) = view worlWindowSize w
        screen <- SDL.setVideoMode x y 0 [SDL.SWSurface]--, SDL.Fullscreen]
        let mainwire = addControls $ showGame w screen (game w scene)
        let fps = fromIntegral (view worlFPS w)
        testWireM id (countSession_ (1.0 / fps)) mainwire

renderFrame :: WorldConsts -> SDL.Surface -> Camera -> [Player] -> IO ()
renderFrame w screen cam players = do
            void $ SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0 0 0 >>=
                SDL.fillRect screen Nothing
            mapM_ (renderPlayer screen cam) players
            SDL.flip screen
            SDL.delay (1000 `div` fromIntegral (view worlFPS w))

showGame :: WorldConsts -> SDL.Surface -> WireP s e () [Player] -> Wire s e IO (Set.Set SDL.Keysym) ()
showGame w screen game = proc keys -> do
    mkGen_' (renderFrame w screen (defCam w)) . addMonad game -< ()
    where addMonad :: (Monad m) => Wire s e Identity a b -> Wire s e m a b
          addMonad = mapWire (return.runIdentity)

game :: HasTime t s => WorldConsts -> [(AI s, Player)] -> WireP s () a [Player]
-- ~ game scene = pure []
game w scene = proc _ -> do
        rec
            oldPlayers <- delay init -< players
            newPlayers <- ais -< makeEnvs id oldPlayers
            players <- mkSF_ collidePlayers -< newPlayers
        returnA -< oldPlayers
        where ais = multicast $ map (liftAI w .fst) scene
              init = map snd scene

liftAI :: WorldConsts -> AI s -> WireP s () Environment Player
liftAI w ai = when (not.died) . proc (yo, otros) -> do
        v <- ai -< (yo, otros)
        returnA -< movePlayer w (normalized v) yo
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

addControls :: Wire s e IO (Set.Set SDL.Keysym) b -> Wire s e IO a b
addControls wire =  proc _ -> do
        rec keys <- mkGen_' parseEvents . delay Set.empty -< keys
        wire -< keys
            
multicast :: (Monad m, Monoid s) => [Wire s e m a b] -> Wire s e m [a] [b]
multicast wires = mkGen $ \dt inputs -> do
        let stepper w x = stepWire w dt (Right x)
        res <- sequence $ zipWith stepper wires inputs
        let (outputs, wires') = unzip $ filter (isRight.fst) res
        return (Right (rights outputs), multicast wires')
        
mkGen_' :: Monad m => (a -> m b) -> Wire s e m a b
mkGen_' f = let f' x = fmap Right (f x)
            in mkGen_ f'

{-# LANGUAGE Arrows #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

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
import Entities

type Environment = (Player, [Player])
type Scene s = [(AI s, Player)]
type AI s =  Wire s () Identity Environment Vector

--------------------------------------------------------------------------------
main :: IO ()
main = runSimulation defWorldConsts testScene

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

showGame :: WorldConsts -> SDL.Surface -> Wire s e Identity () [Player] -> Wire s e IO (Set.Set SDL.Keysym) ()
showGame w screen game = proc keys -> do
    mkGen_' (renderFrame w screen testCam) . addMonad game -< ()
    where addMonad :: (Monad m) => Wire s e Identity a b -> Wire s e m a b
          addMonad = mapWire (return.runIdentity)

game :: HasTime t s => WorldConsts -> [(AI s, Player)] -> Wire s () Identity a [Player]
-- ~ game scene = pure []
game w scene = proc _ -> do
        rec
            oldPlayers <- delay init -< players
            newPlayers <- ais -< makeEnvs id oldPlayers
            players <- mkSF_ collidePlayers -< newPlayers
        returnA -< oldPlayers
        where ais = multicast $ map (liftAI w .fst) scene
              init = map snd scene

liftAI :: WorldConsts -> AI s -> Wire s () Identity Environment Player
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

--------------------------------------------------------------------------------
testBola :: Integer -> Double -> Vector -> Bola
testBola n r v = Bola v r

testScene :: (Fractional t, HasTime t s) => Scene s
testScene = [(derecha, Player (show i) [Bola (20, (20+(fromInteger i)*30)) 200] (rgbColor 255 ((fromIntegral i)*40) 40)) | i <- [0..2]]
         ++ [(super, Player (show i) [Bola (120, (20+(fromInteger (i-2))*50)) 300] (rgbColor 40 ((fromIntegral i)*40) 255)) | i <- [2..3]]
testCam = Camera (0,0) 4


derecha ::  AI s
derecha = pure (1, 0)

vibrar :: (Fractional t, HasTime t s) => AI s
vibrar = for 0.2 . pure (0, 1) --> for 0.2 . pure (0, -1) --> vibrar


derechayesperar :: (Fractional t, HasTime t s) => AI s
derechayesperar =  for 0.5 . derecha  --> for 0.5 . pure (0, 0)
                    --> derechayesperar
                    
                    
                                        
                    
super :: (Fractional t, HasTime t s) => AI s



super = (for 1 . pure (1, 1)) --> pure (0.01, 0.01)


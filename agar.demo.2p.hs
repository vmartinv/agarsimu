{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Prelude hiding ((.), id)
import qualified Prelude
import Control.Lens hiding (at, perform, wrapped)
import Control.Monad (replicateM, void)
import Data.Maybe
import Data.Either
import Data.Word (Word8)
import Control.Wire hiding (until)
import Data.VectorSpace ((^+^), (^-^), normalized, (*^))
import Data.AffineSpace (distance)
import qualified Data.Set as Set
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDL

type Vector = (Double, Double)

data WorldConsts = WorldConsts { _worlTam :: Vector
                               , _worlSpeed :: Double
                               }
$(makeLenses ''WorldConsts)


rgbColor :: Word8 -> Word8 -> Word8 -> SDL.Pixel
rgbColor r g b = let fi = fromIntegral
                  in SDL.Pixel (fi r *2^24 + fi g*2^16 + fi b*2^8 + 255)


--------------------------------------------------------------------------------
data Camera = Camera { camPos :: Vector
                     , camZoom :: Double
                     }
                     
--------------------------------------------------------------------------------
data Bola = Bola { _bolPos :: Vector
                 , _bolMass :: Double
                 , _bolColor :: SDL.Pixel
                 } deriving Show
$(makeLenses ''Bola)

getRadio :: Bola -> Double
getRadio b = sqrt $ (view bolMass b)/pi

renderBola :: SDL.Surface -> SDL.Pixel -> Camera -> Bola -> IO ()
renderBola surf color cam b = void $ do
    SDL.filledCircle surf (round x) (round y) (round r) color
    SDL.aaCircle surf (round x) (round y) (round r) color
    where (x, y) = view bolPos b
          r = getRadio b

moveBola :: WorldConsts -> Vector -> Bola -> Bola
moveBola w v bola = over bolPos (clamp.(((view worlSpeed w * view bolMass bola)*^v)^+^)) bola
    where clamp = id

distBolas :: Bola -> Bola -> Double
distBolas p q = distance (view bolPos p) (view bolPos q)

collideBola :: [Bola] -> Bola -> Maybe Bola
collideBola others me = if any (eats me) others
                        then Nothing
                        else let eaten = map (view bolMass) $ filter (flip eats me) others
                             in Just $ over bolMass (+(foldl (+) 0 eaten)) me
        where a `eats` b = let s = getRadio b + getRadio a
                               prop = view bolMass b / view bolMass a
                           in prop > 1.1 && distBolas a b < 0.9*s

collideBolas :: [Bola] -> [Bola] -> [Bola]
collideBolas others mines = map fromJust $ filter isJust (map (collideBola others) mines)
               
--------------------------------------------------------------------------------
data Player = Player { _plaName :: String
                     , _plaBolas :: [Bola]
                     , _plaColor :: SDL.Pixel
                     } deriving Show
$(makeLenses ''Player)

renderPlayer :: SDL.Surface -> Camera -> Player -> IO ()
renderPlayer surf cam p = mapM_ (renderBola surf (view plaColor p) cam) (view plaBolas p)

movePlayer :: WorldConsts -> Vector -> Player -> Player
movePlayer w v p = over plaBolas (map $ moveBola w v) p

collidePlayers :: [Player] -> [Player]
collidePlayers ps = let envs :: [(Player, [[Bola]])]
                        envs = makeEnvs (view plaBolas) ps
                        envs' :: [(Player, [Bola])]
                        envs' = map (\(p, bss) -> (p, concat bss)) envs
                    in map (\(p, bs) -> over plaBolas (collideBolas bs) p) envs'

--------------------------------------------------------------------------------
testBola :: Integer -> Double -> Vector -> Bola
testBola n r v = Bola v r (rgbColor 255 ((fromIntegral n)*30) 255)
-- ~ testFrame :: Integer -> Frame
-- ~ testFrame n = [testBola i | i <- [1..n]]
type Scene = [(AI, Player)]
testScene ::  Scene 
testScene = [(derecha, Player (show i) [testBola i 200 (20, (20+(fromInteger i)*30))] (rgbColor 255 ((fromIntegral i)*40) 40)) | i <- [0..2]]
         ++ [(super, Player (show i) [testBola i 300 (120, (20+(fromInteger (i-2))*50))] (rgbColor 40 ((fromIntegral i)*40) 255)) | i <- [2..3]]
testCam = Camera (0,0) 4
testWorldConsts = WorldConsts (200, 200) 0.0016

--------------------------------------------------------------------------------
framerate :: Num a => a
framerate = 60
main :: IO ()
main = start testWorldConsts testScene

-- ~ start :: (Fractional t, HasTime t s) => WorldConsts -> Scene s -> IO ()
-- ~ start w scene = do

start :: WorldConsts -> Scene -> IO ()
start w scene = do
    SDL.withInit [SDL.InitEverything] $ do
        screen <- SDL.setVideoMode 200 200 0 [SDL.SWSurface]--, SDL.Fullscreen]
        testWireM id (countSession_ (1/framerate)) (mainwire w screen scene)

mainwire :: WorldConsts -> SDL.Surface -> Scene -> Wire s () IO a ()
mainwire w screen scene = addControls $ showGame screen (game w scene) 

renderFrame :: SDL.Surface -> Camera -> [Player] -> IO ()
renderFrame screen cam players = do
            void $ SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0 0 0 >>=
                SDL.fillRect screen Nothing
            mapM_ (renderPlayer screen cam) players
            SDL.flip screen
            SDL.delay (1000 `div` framerate)

showGame :: SDL.Surface -> Wire s e Identity () [Player] -> Wire s e IO (Set.Set SDL.Keysym) ()
showGame screen game = proc keys -> do
    mkGen_' (renderFrame screen testCam) . addMonad game -< ()
    where addMonad :: (Monad m) => Wire s e Identity a b -> Wire s e m a b
          addMonad = mapWire (return.runIdentity)


makeEnvs :: (a -> b) -> [a] -> [(a, [b])]
makeEnvs f xs = makeEnvs' [] xs (map f xs)
  where makeEnvs' izq [] [] = []
        makeEnvs' izq (x:der) (y:der') = (x, izq++der'):makeEnvs' (y:izq) der der'

game :: WorldConsts -> [(AI, Player)] -> Wire s () Identity a [Player]
-- ~ game scene = pure []
game w scene = proc _ -> do
        rec
            oldPlayers <- delay init -< players
            newPlayers <- ais -< makeEnvs id oldPlayers
            players <- mkSF_ collidePlayers -< newPlayers
        returnA -< oldPlayers
        where ais = multicast $ map (liftAI w .fst) scene
              init = map snd scene

type Environment = (Player, [Player])
data AI = forall s t. (Fractional t, HasTime t s, Monoid s) => AI (Wire s () Identity Environment Vector)

derecha ::  AI
derecha = pure (1, 0)

vibrar :: AI
vibrar = for 0.2 . pure (0, 1) --> for 0.2 . pure (0, -1) --> vibrar
super :: AI
super = for 1 . pure (-1, 0) --> vibrar

derechayesperar :: AI
derechayesperar =  for 0.5 . derecha  --> for 0.5 . pure (0, 0)
                    --> derechayesperar

liftAI :: WorldConsts -> AI -> Wire s () Identity Environment Player
liftAI w ai = when (not.died) . proc (yo, otros) -> do
        v <- ai -< (yo, otros)
        returnA -< movePlayer w (normalized v) yo
    where died p = length (view plaBolas p) == 0


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

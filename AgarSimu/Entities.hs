{-# LANGUAGE TemplateHaskell #-}
module AgarSimu.Entities where

import Control.Lens hiding (at, perform, wrapped)
import Control.Monad (void)
import Data.Maybe
import Data.Word (Word8)
import Data.VectorSpace ((^+^), (^-^), normalized, (*^))
import Data.AffineSpace (distance)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDL

type Vector = (Double, Double)

data WorldConsts = WorldConsts { _worlSize :: Vector
                               , _worlWindowSize :: (Int, Int)
                               , _worlSpeed :: Double
                               , _worlFPS :: Int
                               }
$(makeLenses ''WorldConsts)

defWorldConsts :: WorldConsts
defWorldConsts = WorldConsts (200, 200) (400, 400) 0.0016 60

--------------------------------------------------------------------------------
data Camera = Camera { _camPos :: Vector
                     , _camZoom :: Double
                     , _camSize :: Vector
                     }
$(makeLenses ''Camera)

          
camZoomIn :: Camera -> Camera
camZoomIn cam = over camZoom (*1.1) cam   

camZoomOut :: Camera -> Camera
camZoomOut cam = over camZoom (*0.9) cam

camMove :: Camera -> Vector -> Camera      
camMove cam (x, y) = over camPos (^-^v) cam
    where v = (x / view camZoom cam, y / view camZoom cam)
    
defCam :: WorldConsts -> Camera
defCam w = Camera ((wx+px)/2, (wy+py)/2) scale (px, py)
    where (wx, wy) = view worlSize w
          (vx', vy') = view worlWindowSize w
          (vx, vy) = (fromIntegral vx', fromIntegral vy')
          scale = if wx/wy > vx/vy then vx/wx else vy/wy
          (px, py) = (vy/scale, vx/scale)

toScreen :: Camera -> Double -> Double
toScreen cam x = x * (view camZoom cam)
toScreenV :: Camera -> Vector -> Vector
toScreenV cam v = (toScreen cam x', toScreen cam y')
    where (x', y') = v ^-^ (view camPos cam) ^+^ (view camSize cam)

--------------------------------------------------------------------------------
data Bola = Bola { _bolPos :: Vector
                 , _bolMass :: Double
                 } deriving Show
$(makeLenses ''Bola)

getRadio :: Bola -> Double
getRadio b = sqrt $ (view bolMass b)/pi

renderBola :: SDL.Surface -> SDL.Pixel -> Camera -> Bola -> IO ()
renderBola surf color cam b = void $ do
    SDL.filledCircle surf (round x) (round y) (round r) color
    SDL.aaCircle surf (round x) (round y) (round r) color
    where (x, y) = toScreenV cam (view bolPos b)
          r = max 1 (toScreen cam (getRadio b))

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
makeEnvs :: (a -> b) -> [a] -> [(a, [b])]
makeEnvs f xs = makeEnvs' [] xs (map f xs)
  where makeEnvs' izq [] [] = []
        makeEnvs' izq (x:der) (y:der') = (x, izq++der'):makeEnvs' (y:izq) der der'

rgbColor :: Word8 -> Word8 -> Word8 -> SDL.Pixel
rgbColor r g b = let fi = fromIntegral
                  in SDL.Pixel (fi r *2^24 + fi g*2^16 + fi b*2^8 + 255)

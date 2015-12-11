{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module:     AgarSimu.Entities
-- Copyright:  (c) 2015 Martin Villagra
-- License:    BSD3
-- Maintainer: Martin Villagra <mvillagra0@gmail.com>

module AgarSimu.Entities
    ( -- * Camera
      Camera (..),
      camZoomIn,
      camZoomOut,
      camMove,
      defCam,
      toScreen,
      toScreenV,
      
      -- * Bola
      renderBola,
      clampCircle,
      mkBolaVec,
      collideBola,
      
      -- * World
      renderBorderBox
    )
    where

import Control.Lens hiding (at, perform, wrapped)
import Control.Monad (void)
import Data.Maybe
import Data.VectorSpace ((^+^), (^-^), magnitude, (*^), (^/))
import Data.AffineSpace (distance)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDL
import AgarSimu.PublicEntities

data Camera = Camera { _camPos :: Vector
                     , _camZoom :: Double
                     , _camSize :: Vector
                     } deriving Show
$(makeLenses ''Camera)

          
camZoomIn :: Camera -> Camera
camZoomIn cam = over camZoom (*1.1) cam   

camZoomOut :: Camera -> Camera
camZoomOut cam = over camZoom (*0.9) cam

camMove :: Camera -> Vector -> Camera      
camMove cam v = over camPos (^-^ v ^/ (view camZoom cam)) cam
    
defCam :: WorldConsts -> Camera
defCam w = Camera (wx/2, wy/2) scale (vx, vy)
    where (wx, wy) = view worlSize w
          (vx', vy') = view worlWindowSize w
          (vx, vy) = (fromIntegral vx', fromIntegral vy')
          scale = if wx/wy > vx/vy then vx/wx else vy/wy

toScreen :: Camera -> Double -> Double
toScreen cam x = x * (view camZoom cam)
toScreenV :: Camera -> Vector -> Vector
toScreenV cam v = (toScreen cam x', toScreen cam y') ^+^ 0.5 *^ (view camSize cam) 
    where (x', y') = v ^-^ (view camPos cam) 

--------------------------------------------------------------------------------
renderBola :: SDL.Surface -> Camera -> Bola -> IO ()
renderBola surf cam b = void $ do
    SDL.filledCircle surf (round x) (round y) (round r) color
    SDL.aaCircle surf (round x) (round y) (round r) color
    where (x, y) = toScreenV cam (view bolPos b)
          r = max 1 (toScreen cam (getRadio b))
          color = view bolColor b

clampCircle :: WorldConsts -> Double -> Vector -> Vector
clampCircle w r (x, y) = let (wx, wy) = view worlSize w
                         in (clamp r (wx-r) x, clamp r (wy-r) y)
    where clamp mn mx = max mn . min mx
    
mkBolaVec :: Bola -> Vector -> Vector
mkBolaVec b v =  speedConstant *^ normalized ^/ view bolMass b
    where normalized = let m = magnitude v
                       in if m>1 then v^/m else v
          speedConstant = 10000

collideBola :: [Bola] -> Bola -> Maybe Bola
collideBola others me = if any (eats me) others
                        then Nothing
                        else let eaten = map (view bolMass) $ filter (flip eats me) others
                             in Just $ over bolMass (+foldl (+) 0 eaten) me
        where a `eats` b = let s = getRadio b + getRadio a
                               prop = view bolMass b / view bolMass a
                           in prop > 1.1 && distBolas a b < 0.9*s

--------------------------------------------------------------------------------
renderBorderBox :: WorldConsts -> SDL.Surface -> Camera -> IO ()
renderBorderBox wc surf cam = void $ do
    SDL.rectangle surf (SDL.Rect (round x1) (round y1) (round x2) (round y2)) color
    where color = rgbColor 255 255 255
          (x1, y1) = toScreenV cam (0, 0) ^-^ (1,1)
          (x2, y2) = toScreenV cam (view worlSize wc) ^+^ (1,1)


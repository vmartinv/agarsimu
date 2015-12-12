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
      renderBackground
    )
    where

import Control.Lens hiding (at, perform, wrapped)
import Control.Monad (void, when)
import Data.Maybe
import Data.VectorSpace ((^+^), (^-^), magnitude, (*^), (^/))
import Data.AffineSpace (distance)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDL
import qualified Graphics.UI.SDL.TTF as SDLTTF
import AgarSimu.PublicEntities

data Camera = Camera { _camPos :: Vector
                     , _camZoom :: Double
                     , _camSize :: Vector
                     , _camFonts :: SDLTTF.Font
                     } deriving Show
$(makeLenses ''Camera)

          
camZoomIn :: Camera -> Camera
camZoomIn cam = over camZoom (*1.1) cam   

camZoomOut :: Camera -> Camera
camZoomOut cam = over camZoom (*0.9) cam

camMove :: Camera -> Vector -> Camera      
camMove cam v = over camPos (^-^ v ^/ (view camZoom cam)) cam
    
defCam :: WorldConsts -> SDLTTF.Font -> Camera
defCam w fonts = Camera (wx/2, wy/2) scale (vx, vy) fonts
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
    if r>=5
    then do
        SDL.filledCircle surf (round x) (round y) (round r) darkerColor
        SDL.aaCircle surf (round x) (round y) (round r) darkerColor
        SDL.filledCircle surf (round x) (round y) (round r - border) color
        SDL.aaCircle surf (round x) (round y) (round r - border) color
        
        -- ~ nameS <- SDLTTF.renderTextSolid (view camFonts cam) (view bolName b)
                        -- ~ (SDL.Color 255 255 255)

        -- ~ SDL.blitSurface nameS Nothing surf (Just $ SDL.Rect (round $ x-r/2) (round $ y-r/2) (round $ x+r/2) (round $ y+r/2))
    else SDL.filledCircle surf (round x) (round y) (round r) color
    where (x, y) = toScreenV cam (view bolPos b)
          r = max 1 (toScreen cam (getRadio b))
          border = round $ toScreen cam 0.35
          color = view bolColor b
          darkerColor = let (r, g, b) = getColor color
                            val = 30
                            positive = max val
                        in rgbColor (positive r-val) (positive g-val) (positive b-val)

clampCircle :: WorldConsts -> Double -> Vector -> Vector
clampCircle w r (x, y) = let (wx, wy) = view worlSize w
                         in (clamp r (wx-r) x, clamp r (wy-r) y)

clamp :: Ord a => a -> a -> a -> a
clamp mn mx = max mn . min mx
    
mkBolaVec :: Bola -> Vector -> Vector
mkBolaVec b v =  speedConstant *^ normalized ^/ mass
    where mass = view bolMass b
          normalized = let m = magnitude v
                       in if m>1 then v^/m else v
          speedConstant = 10*25

collideBola :: [Bola] -> Bola -> Maybe Bola
collideBola others me = if any (eats me) others
                        then Nothing
                        else let eaten = map (view bolMass) $ filter (flip eats me) others
                             in Just $ over bolMass (+foldl (+) 0 eaten) me
        where a `eats` b = let s = getRadio b + getRadio a
                               prop = view bolMass b / view bolMass a
                           in prop > 1.1 && distBolas a b < 0.9*s

--------------------------------------------------------------------------------
renderBackground :: WorldConsts -> SDL.Surface -> Camera -> IO ()
renderBackground wc surf cam = void $ do
    -- ~ SDL.rectangle surf (SDL.Rect (round x1) (round y1) (round x2) (round y2)) lineColor
    SDL.fillRect surf (Just $ SDL.Rect (round x1) (round y1) (round $ x2-x1) (round $ y2-y1)) backColor
    when (separation>4) $ do
        drawLines (\y -> SDL.hLine surf (round x1) (round x2) (round y) lineColor) y1' (min vy y2) separation
        drawLines (\x -> SDL.vLine surf (round x) (round y1) (round y2) lineColor) x1' (min vx x2) separation
    where (x1, y1) = toScreenV cam (0, 0)
          (x1', y1') = (x1, y1)
          (x2, y2) = toScreenV cam (view worlSize wc)
          (vx, vy) = view camSize cam
          lineColor = rgbColor 216 224 228
          backColor = rgbColor 242 251 255
          separation = let wx = fst $ view worlSize wc :: Double
                           divs = round $ wx / 3 :: Int
                       in  toScreen cam (wx/fromIntegral divs)
          drawLines f p lim step | p >= lim+0.1 = return ()
                                 | otherwise = do f p
                                                  drawLines f (p+step) lim step


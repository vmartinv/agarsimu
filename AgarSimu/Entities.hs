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


tmap :: (a -> b) -> (a, a) -> (b, b)
tmap f (x, y) = (f x, f y)

data Camera = Camera { _camPos :: Vector
                     , _camZoom :: Double
                     , _camSize :: (Int, Int)
                     , _camFonts :: SDLTTF.Font
                     } deriving Show
$(makeLenses ''Camera)

          
camZoomIn :: Camera -> Camera
camZoomIn cam = over camZoom (\z-> min 100 (1.1*z)) cam   

camZoomOut :: Camera -> Camera
camZoomOut cam = over camZoom (*0.9) cam

camMove :: Camera -> Vector -> Camera      
camMove cam v = over camPos (^-^ v ^/ (view camZoom cam)) cam
    
defCam :: WorldConsts -> SDLTTF.Font -> Camera
defCam w fonts = Camera (wx/2, wy/2) scale (vx', vy') fonts
    where (wx, wy) = view worlSize w
          (vx', vy') = view worlWindowSize w
          (vx, vy) = (fromIntegral vx', fromIntegral vy')
          scale = if wx/wy > vx/vy then vx/wx else vy/wy

toScreen :: Camera -> Double -> Double
toScreen cam x = x * (view camZoom cam)
toScreenV :: Camera -> Vector -> Vector
toScreenV cam v = (toScreen cam x', toScreen cam y') ^+^ 0.5 *^ (tmap fromIntegral (view camSize cam))
    where (x', y') = v ^-^ (view camPos cam)

--------------------------------------------------------------------------------
renderBola :: SDL.Surface -> Camera -> Bola -> IO ()
renderBola surf cam b =  when (isInRange (-r) x (w+r) && isInRange (-r) y (h+r)) $ do
    if r>=5
    then void $ do
        SDL.filledCircle surf (round x) (round y) (round r) borderColor
        SDL.aaCircle surf (round x) (round y) (round r) borderColor
        SDL.filledCircle surf (round x) (round y) (round r - border) color
        SDL.aaCircle surf (round x) (round y) (round r - border) color
        -- ~ nameS <- SDLTTF.renderTextSolid (view camFonts cam) (view bolName b)
                        -- ~ (SDL.Color 255 255 255)
        -- ~ SDL.blitSurface nameS Nothing surf (Just $ SDL.Rect (round $ x-r/2) (round $ y-r/2) (round $ x+r/2) (round $ y+r/2))
    else void $ SDL.filledCircle surf (round x) (round y) (round r) color
    where (x, y) = toScreenV cam (view bolPos b)
          (w, h) = tmap fromIntegral (view camSize cam)
          r = max 1 (toScreen cam (getRadio b))
          border = round $ toScreen cam 0.5
          color = view bolColor b
          borderColor = let (r, g, b) = getRgb color
                            darker c = round $ 0.9 * fromIntegral c
                        in rgb (darker r) (darker g) (darker b)
          isInRange lo x hi = lo<=x && x<= hi 
          
    
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
    SDL.fillRect surf (Just $ SDL.Rect x1 y1 (w+1) (h+1)) backColor
    when (separation>4) $ do
        drawLines (\y -> SDL.hLine surf (fromIntegral x1) (fromIntegral x2) (round y) lineColor)
            (correct y1') (fromIntegral $ min vy y2) separation
        drawLines (\x -> SDL.vLine surf (round x) (fromIntegral y1) (fromIntegral y2) lineColor)
            (correct x1') (fromIntegral $ min vx x2) separation
    where lineColor = SDL.Pixel 0xe3ebefff
          backColor = SDL.Pixel 0xf2fbffff
          (vx, vy) =  view camSize cam
          (x1', y1') = toScreenV cam (0, 0)
          (x2', y2') = toScreenV cam (view worlSize wc)
          (x1, y1) = (round x1', round y1')
          (w, h) = (round $ x2'-x1', round $ y2'-y1')
          (x2, y2) = (x1+w, y1+h)
          separation = let wx = fst $ view worlSize wc
                           divs = round $ wx / 3
                       in  toScreen cam (wx/fromIntegral divs)
          correct x = x + separation * max 0 (fromIntegral $ floor $ -x/separation)
          drawLines f p lim step | p >= lim = return ()
                                 | otherwise = f p >> drawLines f (p+step) lim step


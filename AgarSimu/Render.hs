{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module:     AgarSimu.Render
-- Copyright:  (c) 2015 Martin Villagra
-- License:    BSD3
-- Maintainer: Martin Villagra <mvillagra0@gmail.com>

module AgarSimu.Render
    ( -- * Camera
      Camera (..)
    , camZoomIn
    , camZoomOut
    , camMove
      
      -- * Rendering
    , withPrepareRendering  
    , renderLogic      
    )
    where

import qualified Control.Monad as M (void, when) 
import Control.Lens hiding (at, perform, wrapped)
import Data.VectorSpace ((^+^), (^-^), magnitude, (*^), (^/))
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDL
import qualified Graphics.UI.SDL.Framerate as Framerate
import Control.Wire hiding ((.))
import AgarSimu.PublicEntities
import AgarSimu.Utils

data Camera = Camera { _camPos :: !Vector
                     , _camZoom :: !Double
                     , _camSize :: !(Int, Int)
                     , _camSurf :: !SDL.Surface
                     , _camFps :: !Framerate.FPSManager
                     } deriving Show
$(makeLenses ''Camera)

camZoomIn :: Camera -> Camera
camZoomIn cam = over camZoom (\z-> min 100 (1.1*z)) cam   

camZoomOut :: Camera -> Camera
camZoomOut cam = over camZoom (*0.9) cam

camMove :: Camera -> Vector -> Camera      
camMove cam v = over camPos (^-^ v ^/ (view camZoom cam)) cam
    
defCam :: WorldConsts -> Int -> IO Camera
defCam w fps = do
        screen <- SDL.setVideoMode vx' vy' 0 [SDL.SWSurface]--, SDL.Fullscreen]
        frameRate <- Framerate.new
        Framerate.init frameRate
        Framerate.set frameRate fps
        return $ Camera (wx/2, wy/2) scale (vx', vy') screen frameRate
    where (wx, wy) = view worlSize w
          (vx', vy') = view worlWindowSize w
          (vx, vy) = (fromIntegral vx', fromIntegral vy')
          scale = if wx/wy > vx/vy then vx/wx else vy/wy

toScreen :: Camera -> Double -> Double
toScreen cam x = x * (view camZoom cam)
toScreenV :: Camera -> Vector -> Vector
toScreenV cam v = (toScreen cam x', toScreen cam y') ^+^ 0.5 *^ (tmap fromIntegral (view camSize cam))
    where (x', y') = v ^-^ (view camPos cam)


drawCam :: Camera -> (SDL.Surface -> a) -> a
drawCam cam f = f (view camSurf cam)
--------------------------------------------------------------------------------
withPrepareRendering :: WorldConsts -> Int -> (Camera -> IO a) -> IO a
withPrepareRendering wc fps f = SDL.withInit [SDL.InitEverything] (defCam wc fps >>= f)

renderLogic :: Monoid e =>
    WorldConsts ->
    Wire s e IO (Camera, [Bola], Int) ()
renderLogic wc = proc (cam, bolas, speed) -> do
        rec elapsed <- delay 0 -< (elapsed+1) `mod` speed
        if elapsed==0
            then mkGen_' (uncurry $ renderFrame wc) -< (cam, bolas)
            else returnA -< ()

renderFrame :: WorldConsts -> Camera -> [Bola] -> IO ()
renderFrame wc cam bolas= do
        let screen = view camSurf cam
        SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0 0 0 >>=
            SDL.fillRect screen Nothing
        renderBackground wc cam
        mapM_ (renderBola cam) bolas
        SDL.flip screen
        Framerate.delay (view camFps cam)
            
renderBola :: Camera -> Bola -> IO ()
renderBola cam b = M.when (shouldDraw) $ M.void $ do
        if rr>=5 && view bolMass b >= 10 then do
            draw SDL.filledCircle rr borderColor
            draw SDL.aaCircle rr borderColor
            draw SDL.filledCircle (rr - border) color
            draw SDL.aaCircle (rr - border) color
        else if rr>1 then do
            draw SDL.filledCircle rr color
            draw SDL.aaCircle rr color
        else 
            draw SDL.pixel color
    where (x, y) = tmap round $ toScreenV cam (view bolPos b)
          r = toScreen cam (bolRadio b)
          rr = round r
          (w, h) = tmap fromIntegral $ view camSize cam
          surf = view camSurf cam
          draw f = f surf x y
          border = round $ toScreen cam 0.5
          color = view bolColor b
          borderColor = let (r, g, b) = getRgb color
                            darker c = round $ 0.9 * fromIntegral c
                        in rgb (darker r) (darker g) (darker b)
          isInRange lo x hi = lo<=x && x<= hi
          shouldDraw = r>=0.4 && isInRange (-rr) x (w+rr) && isInRange (-rr) y (h+rr)

renderBackground :: WorldConsts -> Camera -> IO ()
renderBackground wc cam = 
        if separation>4 then do
            SDL.fillRect surf (Just $ SDL.Rect x1 y1 (w+1) (h+1)) backColor
            drawLines (\y -> SDL.hLine surf (fromIntegral x1) (fromIntegral x2) (round y) lineColor)
                (correct y1') (fromIntegral $ min vy y2) separation
            drawLines (\x -> SDL.vLine surf (round x) (fromIntegral y1) (fromIntegral y2) lineColor)
                (correct x1') (fromIntegral $ min vx x2) separation
        else M.void $
            SDL.fillRect surf (Just $ SDL.Rect x1 y1 (w+1) (h+1)) lineColor
    where surf = view camSurf cam
          lineColor = SDL.Pixel 0xe3ebefff
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

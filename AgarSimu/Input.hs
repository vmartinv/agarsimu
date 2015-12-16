{-# LANGUAGE Arrows #-}
-- Module:     AgarSimu.Input
-- Copyright:  (c) 2015 Martin Villagra
-- License:    BSD3
-- Maintainer: Martin Villagra <mvillagra0@gmail.com>

module AgarSimu.Input
    ( -- * Wire
      inputLogic
    )
    where

import Prelude hiding ((.), id, until)
import qualified Graphics.UI.SDL as SDL
import Control.Wire
import AgarSimu.Render
import AgarSimu.Utils

inputLogic :: (Monoid s, Monoid e) => Camera -> Wire s e IO a (Camera, Int)
inputLogic cam = proc _ -> do
    evs <- readEvents -< ()
    addMonad quitHandler -< evs
    addMonad (mouseCam cam &&& speedHandler) -< evs

quitHandler :: Monoid e => WireP s e [SDL.Event] ()
quitHandler = pure () . when (not.(any (isKeyDown SDL.SDLK_q)))

speedHandler :: WireP s e [SDL.Event] Int
speedHandler = foldlWire (const upd) 1 . (id &&& id)
    where upd x ev | isKeyDown SDL.SDLK_KP_PLUS ev = modi con x
          upd x ev | isKeyDown SDL.SDLK_KP_MINUS ev = max 1 (modi (1/con) x)
          upd x _ = x
          con = 1.6 :: Double
          modi c = round.(c*).fromIntegral  
 
leftClickHandler :: WireP s e [SDL.Event] Bool
leftClickHandler = foldlWire (const upd) False . (id&&&id)
    where upd _ (SDL.MouseButtonDown _ _ SDL.ButtonLeft) = True
          upd _ (SDL.MouseButtonUp _ _ SDL.ButtonLeft) = False
          upd b _ = b
          
-- ~ rightClickHandler :: WireP s e [SDL.Event] (Event Bola)
-- ~ rightClickHandler =  . become (any isClick)
    -- ~ where isClick (SDL.MouseButtonDown _ _ SDL.ButtonRight) = True
          -- ~ isClick _ = False

mouseCam :: Camera -> WireP s e [SDL.Event] Camera
mouseCam init = foldlWire upd init . (leftClickHandler &&& id)
    where upd _ cam (SDL.MouseButtonDown _ _ SDL.ButtonWheelUp) = camZoomIn cam
          upd _ cam (SDL.MouseButtonDown _ _ SDL.ButtonWheelDown) = camZoomOut cam
          upd True cam (SDL.MouseMotion _ _ x y) = camMove cam (fromIntegral x, fromIntegral y)
          upd _ cam _ = cam

isKeyDown :: SDL.SDLKey -> SDL.Event -> Bool
isKeyDown x (SDL.KeyDown (SDL.Keysym y _ _)) = x==y
isKeyDown _ _ = False

readEvents :: Monoid e => Wire s e IO a [SDL.Event]
readEvents = mkGen_' $ const (acum [])
    where acum evs = do ev <- SDL.pollEvent
                        -- ~ print ev
                        case ev of
                            SDL.NoEvent -> return evs
                            _ -> acum (ev:evs)

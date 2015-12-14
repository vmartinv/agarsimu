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

inputLogic :: (Monoid s) => Camera -> Wire s () IO a (Camera, Int)
inputLogic cam = proc _ -> do
    evs <- readEvents -< ()
    quit <- addMonad quitControl -< evs
    case quit of
        True -> inhibit () -< ()
        False -> addMonad (mouseCam cam &&& speedControl) -< evs


quitControl :: WireP s e [SDL.Event] Bool
quitControl = foldlWire (const upd) False . (id &&& id)
    where upd _ (SDL.KeyDown (SDL.Keysym SDL.SDLK_q _ _)) = True
          upd b _ = b
          
speedControl :: WireP s e [SDL.Event] Int
speedControl = foldlWire (const upd) 1 . (id &&& id)
    where upd x (SDL.KeyDown (SDL.Keysym SDL.SDLK_KP_PLUS _ _)) = modi con x
          upd x (SDL.KeyDown (SDL.Keysym SDL.SDLK_KP_MINUS _ _)) = max 1 (modi (1/con) x)
          upd x _ = x
          con = 1.6 :: Double
          modi c = round.(c*).fromIntegral  
 
leftClickState :: WireP s e [SDL.Event] Bool
leftClickState = foldlWire (const upd) False . (id&&&id)
    where upd _ (SDL.MouseButtonDown _ _ SDL.ButtonLeft) = True
          upd _ (SDL.MouseButtonUp _ _ SDL.ButtonLeft) = False
          upd b _ = b

mouseCam :: Camera -> WireP s e [SDL.Event] Camera
mouseCam init = foldlWire upd init . (leftClickState &&& id)
    where upd _ cam (SDL.MouseButtonDown _ _ SDL.ButtonWheelUp) = camZoomIn cam
          upd _ cam (SDL.MouseButtonDown _ _ SDL.ButtonWheelDown) = camZoomOut cam
          upd True cam (SDL.MouseMotion _ _ x y) = camMove cam (fromIntegral x, fromIntegral y)
          upd _ cam _ = cam

readEvents :: Monoid e => Wire s e IO a [SDL.Event]
readEvents = mkGen_' $ const (acum [])
    where acum evs = do ev <- SDL.pollEvent
                        -- ~ print ev
                        case ev of
                            SDL.NoEvent -> return evs
                            _ -> acum (ev:evs)

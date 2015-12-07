{-# LANGUAGE Arrows #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import Prelude hiding ((.), id, mapM_, any, concatMap, concat)
import qualified Prelude
import Control.Monad (replicateM, void)
import Data.Bits
import Data.Word (Word8)
import Control.Lens hiding (at, perform, wrapped)
import Control.Wire hiding (until)
import Data.Foldable
import Data.Monoid
import Data.Either.Combinators
import Control.Monad.Fix (MonadFix)
import Linear hiding ((*!))
import qualified Data.Set as Set
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Framerate as Framerate
import qualified Graphics.UI.SDL.Primitives as SDL
import qualified Graphics.UI.SDL.TTF as SDLTTF
import FRP.Netwire.Move
import Control.Monad.IO.Class

rgbColor :: Word8 -> Word8 -> Word8 -> SDL.Pixel
rgbColor r g b = SDL.Pixel (shiftL (fromIntegral r) 24 .|.
                            shiftL (fromIntegral g) 16 .|.
                            shiftL (fromIntegral b) 8  .|.
                            255)

type Font = SDLTTF.Font 
--------------------------------------------------------------------------------
data Bounds = Circle (V2 Double) Double

renderBounds :: SDL.Surface -> SDL.Pixel -> Bounds -> IO ()
renderBounds surf color (Circle (V2 x y) r) = void $
  SDL.circle surf (round x) (round y) (round r) color
  
--------------------------------------------------------------------------------
class Physical p where --modela objetos con bordes
    bounds :: p -> Bounds

class Positioned p where --modela objetos posicionados en el juego
    position :: Functor f => (V2 Double -> f (V2 Double)) -> p -> f p
  
--------------------------------------------------------------------------------
data Bola = Bola { bolPos :: V2 Double
                 , bolMass :: Double
                 -- ~ , bolVelocity :: V2 Double
                 }
                    
instance Physical Bola where
    bounds Bola{..} = Circle bolPos bolMass
    
instance Positioned Bola where
    position f x =  f (bolPos x) <&> \b -> x { bolPos = b }

renderBola :: SDL.Surface -> SDL.Pixel -> Bola -> IO ()
renderBola surf color bola = renderBounds surf color (bounds bola)
  
--------------------------------------------------------------------------------
data Player = Player { plaName :: String
                     , plaBolas :: [Bola]
                     , plaColor :: SDL.Pixel
                     }
                     
renderPlayer :: SDL.Surface -> Font -> Player -> (V2 Double -> Bool) -> IO ()
renderPlayer surf _ Player{..} shouldraw =  mapM_ (renderBola surf plaColor) plaBolas

--------------------------------------------------------------------------------
data Camara = Camara { camPos :: V2 Double
                     , camZoom :: Double
                     }

instance Positioned Camara where
    position f x =  f (camPos x) <&> \b -> x { camPos = b }

isInside :: Camara -> Positioned -> Bool
isInside Camara{..} p = distance camPos p <= camZoom

--------------------------------------------------------------------------------
data Frame = Frame { fPlayers :: [Player]
                   , fCamera :: Camara
                   , fBola :: Bola
                   }

renderFrame :: SDL.Surface -> Font -> Frame -> IO ()
renderFrame screen font Frame{..} = do
    void $ SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0 0 0 >>=
        SDL.fillRect screen Nothing
    mapM_ (renderPlayer screen font (isInside.fCamera)) fPlayers
    renderBola screen (rgbColor 255 255 255) fBola

    --scoreS <-
    --  SDLTTF.renderTextSolid font ("SCORE: " ++ show fScore)
    --    (SDL.Color 255 255 255)

    --SDL.blitSurface scoreS Nothing screen (Just $ SDL.Rect 20 20 100 50)

testFrame :: Frame
testFrame = Frame [] (Camara (V2 0 0) 5) (Bola (V2 3 3) 2)

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

--------------------------------------------------------------------------------
main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
    screen <- SDL.setVideoMode 200 200 0 [SDL.SWSurface]--, SDL.Fullscreen]

    SDLTTF.init
    font <- SDLTTF.openFont "Amatic-Bold.ttf" 10
    
    frameRate <- Framerate.new
    Framerate.init frameRate
    Framerate.set frameRate 60

    void $ gameloop Set.empty screen font clockSession_ stepGame testFrame frameRate
    
gameloop :: Set.Set SDL.Keysym -> SDL.Surface -> Font -> Session IO s
     -> Wire s () IO Frame Frame -> Frame -> Framerate.FPSManager -> IO ()
     
gameloop keysDown screen font s w frame frameRate = void $ do
    keysDown' <- parseEvents keysDown
    (ds, s') <- stepSession s
    (r, w') <- stepWire w ds (Right frame)
    case r of
        Left _ -> putStrLn "Game inhibited!"
        Right frame' -> do
            renderFrame screen font frame'
            SDL.flip screen
            Framerate.delay frameRate
            gameloop keysDown' screen font s' w' frame' frameRate

stepGame :: Monad m => Wire s () m Frame Frame
stepGame = pure testFrame

--Wire s () Identity GameSt (V2 Double)



challenge2 :: (Monad m, HasTime t s) => Wire s () m (Set.Set SDL.Keysym) Double
challenge2 = integral 0 . challenge2_velocity

challenge2_velocity :: (Monad m, Monoid e) => Wire s e m (Set.Set SDL.Keysym) Double
challenge2_velocity  =  pure (-20) . when (keyDown SDL.SDLK_LEFT)
                    <|> pure 20 . when (keyDown SDL.SDLK_RIGHT)
                    <|> pure 0

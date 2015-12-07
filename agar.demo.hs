{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Prelude hiding ((.), id, mapM_)
import qualified Prelude
import Control.Monad (replicateM, void)
import Data.Bits
import Data.Either
import Data.Word (Word8)
import Control.Lens hiding (at, perform, wrapped)
import Control.Wire hiding (until)
import Data.Foldable
import Data.Monoid
import Linear hiding ((*!))
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDL

rgbColor :: Word8 -> Word8 -> Word8 -> SDL.Pixel
rgbColor r g b = SDL.Pixel (shiftL (fromIntegral r) 24 .|.
                            shiftL (fromIntegral g) 16 .|.
                            shiftL (fromIntegral b) 8  .|.
                            255)

--------------------------------------------------------------------------------
data Bounds = Circle (V2 Double) Double

renderBounds :: SDL.Surface -> SDL.Pixel -> Bounds -> IO ()
renderBounds surf color (Circle (V2 x y) r) = void $
  SDL.filledCircle surf (round x) (round y) (round r) color
  
--------------------------------------------------------------------------------
class Physical p where
    bounds :: p -> Bounds
  
--------------------------------------------------------------------------------
data Bola = Bola { bolPos :: V2 Double
                 , bolMass :: Double
                 , bolColor :: SDL.Pixel
                 } deriving Show
                    
instance Physical Bola where
    bounds Bola{..} = Circle bolPos bolMass

renderBola :: SDL.Surface -> Bola -> IO ()
renderBola surf bola@Bola{..} = renderBounds surf bolColor (bounds bola)

--------------------------------------------------------------------------------
data Frame = Frame { fBolas :: [Bola]
                   } deriving Show

renderFrame :: SDL.Surface -> Frame -> IO ()
renderFrame screen Frame{..} = do
    void $ SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0 0 0 >>=
        SDL.fillRect screen Nothing
    mapM_ (renderBola screen) fBolas

testBola :: Integer -> Bola
testBola n = Bola (V2 20 (20+(fromInteger n)*30)) 10 (rgbColor 255 ((fromIntegral n)*30) 255)
testFrame :: Integer -> Frame
testFrame n = Frame [testBola i | i <- [1..n]]

data Subwire s = Subwire (Frame -> Environment) (AI s) (V2 Double -> Frame -> Frame)
loadAis :: HasTime t s => Int -> [Wire s () Identity Frame (Frame -> Frame)]
loadAis n =  map stepSubwire [ Subwire (\f->[]) derechayesperar (\v f -> Frame (replaceNth (i-1) (update v ((getBolas f) !! (i-1) )) (getBolas f) )) | i <- [1..n]]

--------------------------------------------------------------------------------
main :: IO ()
main = do
    SDL.withInit [SDL.InitEverything] $ do
        screen <- SDL.setVideoMode 200 200 0 [SDL.SWSurface]--, SDL.Fullscreen]
        runGame screen clockSession_ (addMonad (stepGame (loadAis 5))) (testFrame 5)
    
runGame screen session wire frame = void $ do
    (ds, session') <- stepSession session
    (r, wire') <- stepWire wire ds (Right frame)
    case r of
        Left _ -> putStrLn "Game inhibited!"
        Right frame' -> do
            renderFrame screen frame'
            SDL.flip screen
            SDL.delay (1000 `div` 60)
            runGame screen session' wire' frame'
         
addMonad :: (HasTime t s, Monad m) => Wire s e Identity a b -> Wire s e m a b
addMonad = mapWire (return.runIdentity)
        
stepGame :: HasTime t s => [Wire s () Identity Frame (Frame -> Frame)] -> Wire s () Identity Frame Frame
stepGame subs = mkGen $ \dt frame -> do
        res <- sequence $ fmap (\w -> stepWire w dt (Right frame)) subs
        let frame'= foldl (\f fun -> fun f) frame (fmap (deshinibit.fst) res)
        return (Right frame', stepGame (fmap snd res))
    where deshinibit (Left _) = id
          deshinibit (Right v) = v


stepSubwire :: HasTime t s => Subwire s -> Wire s () Identity Frame (Frame -> Frame)
-- equivalente a: arr (\v -> outp v) <<< ((ai <<< arr inp)) 
stepSubwire (Subwire getenv ai makemod) = proc frame -> do 
    vec <- ai -< getenv frame
    returnA -< makemod vec
        
update :: V2 Double -> Bola -> Bola
update v Bola{..} = Bola (bolPos+v) bolMass bolColor
getBolas :: Frame -> [Bola]
getBolas Frame{..} = fBolas

replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs
replaceNth n newVal [] = []




 
type Environment = [V2 Double]
type AI s = forall t. HasTime t s => Wire s () Identity Environment (V2 Double)

derecha :: AI s
derecha = pure (V2 1 0)

derechayesperar :: AI s
derechayesperar =  for 2 . derecha

-- ~ type AI s = forall t. HasTime t s => Wire s () Identity Environment (V2 Double)
-- ~ conv :: forall t. HasTime t s => Wire s () (State a) Environment (V2 Double) ->  Wire s () Identity Environment (V2 Double)
-- ~ conv w = mkGen $ \dt x -> do
            -- ~ <- stepWire w dt (Right x) 
        -- ~ res <- (\w -> stepWire w dt (Right frame)) subs
        -- ~ let frame'= foldl (\f fun -> fun f) frame (fmap (deshinibit.fst) res)
        -- ~ return (Right frame', stepGame (fmap snd res))
    -- ~ where deshinibit (Left _) = id
          -- ~ deshinibit (Right v) = v

-- ~ superai estado =

multicast :: (Monad m, Monoid s) => [Wire s e m a b] -> Wire s e m [a] [b]
multicast wires = mkGen $ \dt inputs -> do
        let stepper w x = stepWire w dt (Right x)
        res <- sequence $ zipWith stepper wires inputs
        let (outputs, wires') = unzip $ filter (isRight.fst) res
        return (Right (rights outputs), multicast wires')

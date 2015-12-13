{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module:     AgarSimu.PreFab
-- Copyright:  (c) 2015 Martin Villagra
-- License:    BSD3
-- Maintainer: Martin Villagra <mvillagra0@gmail.com>

module AgarSimu.PublicEntities
    ( -- * Base
      rgb,
      getRgb,
      randomColor,
      Vector,
      
      -- * World
      WorldConsts(..),
      mkWorldConsts,
      worlSize, worlWindowSize, worlSpeed,
      
      -- * Bola
      Bola(..),
      bolColor, bolPos, bolMass,
      getRadio,
      distBolas,
      randomBola,
      
      -- * Base Types
      Time,
      Environment,
      RandomWire,
      AI,
      Scene    
    )
    where

import Control.Lens hiding (at, perform, wrapped)
import Data.Maybe
import Control.Wire hiding ((.))
import Control.Monad.Random
import Data.Word (Word8, Word32)
import Data.AffineSpace (distance)
import qualified Graphics.UI.SDL as SDL (Pixel(..))
import AgarSimu.Utils

rgb :: Word8 -> Word8 -> Word8 -> SDL.Pixel
rgb r g b = let fi = fromIntegral
            in SDL.Pixel (fi r *2^24 + fi g*2^16 + fi b*2^8 + 0xff)

randomColor :: MonadRandom m => m SDL.Pixel
randomColor = getRandomR (0, length colors - 1) >>= return.(colors !!)
    where colors = map (SDL.Pixel.(+0xff).(2^8*))
            [0x07ffb0, 0x07ffb0, 0xe407ff, 0x9cff07, 0x5a07ff, 0x11ff07,
             0xff9307, 0xfeff07, 0xff071d, 0x07ffea, 0x17ff07, 0x8807ff,
             0xffbc07, 0xffd307, 0x7607ff, 0xd907ff, 0x1a07ff, 0x07a2ff,
             0x2507ff, 0x07ff5a]

getRgb :: SDL.Pixel -> (Word8, Word8, Word8)
getRgb (SDL.Pixel p) = (md $ p `div` 2^24, md $ p `div` 2^16, md $ p `div` 2^8)
    where md b = fromIntegral (b `mod` 2^8)

type Vector = (Double, Double)

--------------------------------------------------------------------------------
data WorldConsts = WorldConsts { _worlSize :: Vector
                               , _worlWindowSize :: (Int, Int)
                               , _worlSpeed :: Int
                               } deriving Show
$(makeLenses ''WorldConsts)

mkWorldConsts :: Maybe Vector -> Maybe (Int, Int) -> Maybe Int -> WorldConsts
mkWorldConsts v ws t = WorldConsts (v//(100, 100)) (ws//(400, 400)) (t//1)
    where (//) = flip fromMaybe

--------------------------------------------------------------------------------
data Bola = Bola { _bolColor :: SDL.Pixel
                 , _bolPos :: Vector
                 , _bolMass :: Double
                 } deriving Show
$(makeLenses ''Bola)

getRadio :: Bola -> Double
getRadio b = sqrt $ (view bolMass b)/pi

distBolas :: Bola -> Bola -> Double
distBolas p q = distance (view bolPos p) (view bolPos q)

randomBola :: MonadRandom m => (Double, Double) -> Double -> m Bola
randomBola (wx, wy) m = do col <- randomColor
                           x <- getRandomR (0, wx)
                           y <- getRandomR (0, wy)
                           return $ Bola col (x, y) m
--------------------------------------------------------------------------------
type Time = NominalDiffTime
type Environment = (Vector, Bola, [Bola])
type RandomWire = Wire (Timed Time ()) () (Rand StdGen)
type AI = RandomWire Environment Vector
type Scene = [(AI, Bola)]

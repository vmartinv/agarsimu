{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module:     AgarSimu.PreFab
-- Copyright:  (c) 2015 Martin Villagra
-- License:    BSD3
-- Maintainer: Martin Villagra <mvillagra0@gmail.com>

module AgarSimu.PublicEntities
    ( -- * Base
      rgbColor,
      getColor,
      Vector,
      
      -- * World
      WorldConsts(..),
      mkWorldConsts,
      worlSize, worlWindowSize, worlSpeed, worlStep,
      
      -- * Bola
      Bola(..),
      bolName, bolColor, bolPos, bolMass,
      getRadio,
      distBolas,
      
      -- * Base Types
      Environment,
      RandomWire,
      AI,
      Scene    
    )
    where

import Control.Lens hiding (at, perform, wrapped)
import Data.Maybe
import Control.Wire
import Control.Monad.Random
import Data.Word (Word8, Word32)
import Data.AffineSpace (distance)
import qualified Graphics.UI.SDL as SDL (Pixel(..))
import AgarSimu.Utils

rgbColor :: Word8 -> Word8 -> Word8 -> SDL.Pixel
rgbColor r g b = let fi = fromIntegral
                  in SDL.Pixel (fi r *2^24 + fi g*2^16 + fi b*2^8 + 255)

getColor :: SDL.Pixel -> (Word8, Word8, Word8)
getColor (SDL.Pixel p) = (md $ p `div` 2^24, md $ p `div` 2^16, md $ p `div` 2^8)
    where md b = fromIntegral (b `mod` (2^8))

type Vector = (Double, Double)

--------------------------------------------------------------------------------
data WorldConsts = WorldConsts { _worlSize :: Vector
                               , _worlWindowSize :: (Int, Int)
                               , _worlSpeed :: Int
                               , _worlStep :: NominalDiffTime
                               } deriving Show
$(makeLenses ''WorldConsts)

mkWorldConsts :: Maybe Vector -> Maybe (Int, Int) -> Maybe Int -> Maybe NominalDiffTime -> WorldConsts
mkWorldConsts v ws t s = WorldConsts (v//(100, 100)) (ws//(400, 400)) (t//1) (s//0.1)
    where (//) = flip fromMaybe

--------------------------------------------------------------------------------
data Bola = Bola { _bolName :: String
                 , _bolColor :: SDL.Pixel
                 , _bolPos :: Vector
                 , _bolMass :: Double
                 } deriving Show
$(makeLenses ''Bola)

getRadio :: Bola -> Double
getRadio b = sqrt $ (view bolMass b)/pi

distBolas :: Bola -> Bola -> Double
distBolas p q = distance (view bolPos p) (view bolPos q)

--------------------------------------------------------------------------------
type Environment = (WorldConsts, Bola, [Bola])
type RandomWire = Wire (Timed NominalDiffTime ()) () (Rand StdGen)
type AI = RandomWire Environment Vector
type Scene = [(AI, Bola)]

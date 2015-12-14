{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module:     AgarSimu.Entities
-- Copyright:  (c) 2015 Martin Villagra
-- License:    BSD3
-- Maintainer: Martin Villagra <mvillagra0@gmail.com>

module AgarSimu.Entities
    ( -- * Bola
      mkBolaVec,
      collideBola,
    )
    where

import Control.Lens hiding (at, perform, wrapped)
import Control.Monad (void, when)
import Data.Maybe
import Data.VectorSpace ((^+^), (^-^), magnitude, (*^), (^/))
import Data.AffineSpace (distance)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDL
import AgarSimu.PublicEntities
 
mkBolaVec :: Bola -> Vector -> Vector
mkBolaVec b v = speedConstant *^ normalized ^/ r
    where r = getRadio b
          normalized = let m = magnitude v
                       in if m>1 then v^/m else v
          speedConstant = 50

collideBola :: [Bola] -> Bola -> Maybe Double
collideBola others me = if any (eats me) others
                        then Nothing
                        else let eaten = map (view bolMass) $ filter (flip eats me) others
                             in Just $ foldl (+) 0 eaten
        where a `eats` b = let s = getRadio b + getRadio a
                               prop = view bolMass b / view bolMass a
                           in prop > 1.1 && distBolas a b - getRadio a < 0.9*getRadio b

mkEnvs :: [a] -> [[a]]
mkEnvs xs = mkEnvs' [] xs
  where mkEnvs' izq [] = []
        mkEnvs' izq (x:der) = (izq++der):mkEnvs' (x:izq) der
        

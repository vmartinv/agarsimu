{-# LANGUAGE Arrows #-}
module Main where

import Prelude hiding ((.), id)
import Control.Monad.Random
import Control.Arrow
import AgarSimu
import Data.List
import Data.VectorSpace
import Control.Lens hiding (at, perform, wrapped)

main :: IO ()
main = runSimulation (versus greedy3 greedy3)

builder = do setWorldSize (100, 100)
             setWindowSize (600, 600) 
             bolas <- newBolas 30 (15, 15)
             addBolas (randomAI 0.5) bolas


randomAI :: Time -> AI
randomAI t = for t . hold . now . randomDir --> randomAI t

comibles :: (Bola, [Bola]) -> [Bola]
comibles (yo, otros) = filter ( `eats` yo) otros

meComen :: (Bola, [Bola]) -> [Bola]
meComen (yo, otros) = filter (yo `eats`) otros

closest :: (Bola, [Bola]) -> Bola
closest (yo, otros) = minimumBy byDist otros
    where byDist a b = compare (distBolas a yo) (distBolas b yo)
    
bigger :: [Bola] -> Bola
bigger otros = maximumBy byMass otros
    where byMass a b = compare (view bolMass a) (view bolMass b)

chase :: Bola -> Bola -> Vector
chase yo otro = (uncurry (^-^)) $ view (bolPos `alongside` bolPos) (otro, yo)


greedy1 :: RandomWire Environment Vector
greedy1 = mkSF_ greedy
    where greedy (wc, yo, otros) = let c = comibles (yo, otros)
                                   in case c of
                                        [] -> (0, 0)
                                        xs -> chase yo (closest (yo, xs))
                                        
greedy2 :: RandomWire Environment Vector
greedy2 = mkSF_ greedy
    where greedy (wc, yo, otros) = let c = comibles (yo, otros)
                                   in case c of
                                        [] -> (0, 0)
                                        xs -> chase yo (bigger xs)

greedy3 :: RandomWire Environment Vector
greedy3 = mkSF_ greedy
    where greedy (wc, yo, otros) = let c = meComen (yo, filter (\x->distBolas yo x<2+bolRadio yo+bolRadio x && (view bolColor yo)/=(view bolColor x)) otros)
                                       c' = meComen (yo, filter (\x->distBolas yo x<4+bolRadio yo+bolRadio x && (view bolColor yo)==(view bolColor x)) otros)
                                   in case c' of
                                        [] -> case c of
                                                [] -> greedy' (wc, yo, otros)
                                                xs -> (-1) *^ (chase yo (bigger xs))
                                        ys -> chase yo (bigger ys)
          greedy' (wc, yo, otros) = let c = comibles (yo, otros)
                                    in case c of
                                        [] -> (0, 0)
                                        xs -> chase yo (bigger xs)

vibrar :: AI
vibrar = for 1 . pure (0, 1) --> for 1 . pure (0, -1) --> vibrar

derecha :: AI
derecha = pure (0, 0)

derechayesperar :: AI
derechayesperar =  for 1 . derecha --> for 0.5 . pure (0, 0)
                    --> derechayesperar
   
super :: AI
super = for 10 . pure (1, 1) --> stop



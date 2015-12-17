module Main where

import Prelude hiding ((.), id)
import Control.Lens hiding (at, perform, wrapped)
import Data.VectorSpace
import AgarSimu

main :: IO ()
main = do runSimulation (versus greedy greedy2)
          runSimulation builder

builder = do setWorldSize (100, 100)
             setWindowSize (600, 600) 
             bolas <- newBolas 30 (15, 15)
             addBolas randomAI bolas

greedy2 :: RandomWire Environment Vector
greedy2 = liftAI $ \wc yo otros -> 
    case comibles yo otros of
            [] -> (0, 0)
            xs -> chase yo (biggest xs)

greedy3 :: RandomWire Environment Vector
greedy3 = liftAI $ \wc yo otros -> 
    case meComen yo (filter (\x->isClose yo x && not (sameColor yo x)) otros) of
        [] -> case meComen yo (filter (\x->isClose yo x && (sameColor yo x)) otros) of
                  [] -> case comibles yo otros of
                            [] -> (0, 0)
                            xs -> chase yo (biggest xs)
                  xs -> chase yo (biggest xs)
        ys -> (-1) *^ chase yo (biggest ys)
    where isClose yo x = distBolas yo x<2+bolRadio yo+bolRadio x
          sameColor yo x = (view bolColor yo)==(view bolColor x)



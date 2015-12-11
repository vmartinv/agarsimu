module Main where

import AgarSimu
import Prelude hiding ((.), id)
import Control.Arrow

main :: IO ()
main = runSimulation wc testScene
    where wc = mkWorldConsts Nothing Nothing Nothing

testScene :: Scene
testScene = [(random, Bola (show i)  (rgbColor 255 ((fromIntegral i)*90) 40) (20, ((fromInteger i)*30)) 200) | i <- [0..4]]
         ++ [(vibrar, Bola (show i)  (rgbColor 40 ((fromIntegral i)*80) 255) (80, (20+(fromInteger (i-2))*50)) 300)| i <- [2..3]]


random :: AI
random = for 0.2 . hold . now . randomAI --> random

vibrar :: AI
vibrar = for 0.2 . pure (0, 1) --> for 0.2 . pure (0, -1) --> vibrar

derecha :: AI
derecha = pure (0, 0)

derechayesperar :: AI
derechayesperar =  for 1 . derecha --> for 0.5 . pure (0, 0)
                    --> derechayesperar
   
super :: AI
super = (for 1 . pure (1, 1)) --> pure (0.01, 0.01)



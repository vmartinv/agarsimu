module Main where

import AgarSimu
import Prelude hiding ((.), id)
import Control.Arrow

main :: IO ()
main = runSimulation wc testScene
    where wc = mkWorldConsts (Just (30, 30)) Nothing Nothing Nothing

testScene :: Scene
testScene = [(vibrar, Bola "Pablos"  (rgbColor 255 ((fromIntegral i)*90) 40) (5, ((fromInteger i)*5)) 10) | i <- [0..4]]
         ++ [(derechayesperar, Bola "Martins"  (rgbColor 40 ((fromIntegral i)*80) 255) (15, (10+(fromInteger (i-2))*10)) 30)| i <- [2..3]]


random :: AI
random = for 1.5 . hold . now . randomDir --> random

vibrar :: AI
vibrar = for 0.2 . pure (0, 1) --> for 0.2 . pure (0, -1) --> vibrar

derecha :: AI
derecha = pure (0, 0)

derechayesperar :: AI
derechayesperar =  for 1 . derecha --> for 0.5 . pure (0, 0)
                    --> derechayesperar
   
super :: AI
super = (for 1 . pure (1, 1)) --> pure (0.01, 0.01)



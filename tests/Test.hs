module Main where

import Prelude hiding ((.), id)
import Control.Monad.Random
import Control.Arrow
import AgarSimu

main :: IO ()
main = do ts <- testScene
          runSimulation wc ts
    where wc = WorldConsts (100, 100) (600, 600) 

testScene :: MonadRandom m => m Scene
testScene = manySc (100, 100) (10, 12) 30
-- ~ do
        -- ~ rc <- randomColor
        -- ~ rc2 <- randomColor
        -- ~ return []
        -- ~ return $ [(randomAI 0.2, Bola (rgb 255 ((fromIntegral i)*90) 40) (10, ((fromInteger i)*10)) 10) | i <- [0..4]]
         -- ~ ++ [(randomAI 1, Bola (rgb 40 ((fromIntegral i)*80) 255) (35, (10+(fromInteger (i-2))*30)) 30)| i <- [2..3]]

manySc :: MonadRandom m => (Double, Double) -> (Double, Double) -> Int -> m [(AI, Bola)]
manySc _ _ 0 = return []
manySc tam rng n = do m <- getRandomR rng
                      b <- randomBola tam m
                      xs <- manySc tam rng (n-1)
                      return $ (randomAI 0.2, b):xs
  
randomAI :: Time -> AI
randomAI t = for t . hold . now . randomDir --> randomAI t

vibrar :: AI
vibrar = for 1 . pure (0, 1) --> for 1 . pure (0, -1) --> vibrar

derecha :: AI
derecha = pure (0, 0)

derechayesperar :: AI
derechayesperar =  for 1 . derecha --> for 0.5 . pure (0, 0)
                    --> derechayesperar
   
super :: AI
super = for 10 . pure (1, 1) --> stop



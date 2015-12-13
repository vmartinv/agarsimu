module Main where

import AgarSimu
import Prelude hiding ((.), id)
import Control.Arrow
import Control.Monad.Random

main :: IO ()
main = do ts <- testScene
          runSimulation wc ts
    where wc = mkWorldConsts (Just (50, 50)) Nothing (Just 1)

testScene :: MonadRandom m => m Scene
testScene = many1 (50, 50) 30
-- ~ do
        -- ~ rc <- randomColor
        -- ~ rc2 <- randomColor
        -- ~ return []
        -- ~ return $ [(randomAI 0.2, Bola (rgb 255 ((fromIntegral i)*90) 40) (10, ((fromInteger i)*10)) 10) | i <- [0..4]]
         -- ~ ++ [(randomAI 1, Bola (rgb 40 ((fromIntegral i)*80) 255) (35, (10+(fromInteger (i-2))*30)) 30)| i <- [2..3]]

many1 :: MonadRandom m => (Double, Double) -> Int -> m [(AI, Bola)]
many1 _ 0 = return []
many1 tam n = do b <- randomBola tam 15
                 xs <- many1 tam (n-1)
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



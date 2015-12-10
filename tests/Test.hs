module Main where
import AgarSimu
import Prelude hiding ((.), id)
import Control.Wire.Unsafe.Event

main :: IO ()
main = runSimulation defWorldConsts testScene

testScene :: Scene
testScene = [(random, Player (show i) [Bola (20, ((fromInteger i)*30)) 200] (rgbColor 255 ((fromIntegral i)*90) 40)) | i <- [0..4]]
         ++ [(random, Player (show i) [Bola (40, (20+(fromInteger (i-2))*50)) 300] (rgbColor 40 ((fromIntegral i)*80) 255)) | i <- [2..3]]



random :: AI
random = (for 0.2 . holdFirst . getRandomRAI ((-0.5,-0.5),(0.5,0.5)))
            --> random
vibrar :: AI
vibrar = for 0.2 . pure (0, 1) --> for 0.2 . pure (0, -1) --> vibrar

derecha :: AI
derecha = pure (0, 0)

derechayesperar :: AI
derechayesperar =  for 0.5 . derecha  --> for 0.5 . pure (0, 0)
                    --> derechayesperar
   
super :: AI
super = (for 1 . pure (1, 1)) --> pure (0.01, 0.01)



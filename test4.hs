 module Main where

import Control.Wire
import Prelude hiding ((.), id)
import Control.Monad.Identity (Identity)
import Text.Printf

testApp :: Wire () Identity a Timed
testApp = timeFrom 10

main :: IO ()
main = loop testApp clockSession
    where
    loop w' session' = do
        (mx, w, session) <- stepSession w' session' ()
        case mx of
          Left ex -> putStrLn ("Inhibited: " ++ show ex)
          Right x -> putStrLn ("Produced: " ++ show x)
        loop w session

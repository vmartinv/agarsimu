module Main where

import Prelude hiding ((.), id)-- To use (.) in the scope of Categories instead
import Control.Wire
import FRP.Netwire
import Control.Monad.IO.Class


isKeyDown :: (Monoid e) => Char -> Wire s e IO a e
isKeyDown k =
  mkGen_ $ \_ -> do
    c <- getChar
    case c of
      k   -> putStrLn (show c)
      _   -> putStrLn "nooo"    
    return $ case c of
      k   -> Right mempty
      _   -> Left  mempty

speed :: Monoid e => Wire s e IO a Float
speed  =  pure ( 0.0) . isKeyDown ('A') . isKeyDown ('D')
      <|> pure (-0.5) . isKeyDown ('A')
      <|> pure ( 0.5) . isKeyDown ('D')
      <|> pure ( 0.0)

pos :: HasTime t s => Wire s () IO a Float
pos = integral 0 . speed

main :: IO ()
main = testWireM liftIO clockSession_ pos

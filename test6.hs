module Main where

import Prelude hiding ((.), id)-- To use (.) in the scope of Categories instead
import Control.Wire
import FRP.Netwire
import Control.Monad.IO.Class

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW
import Data.IORef

isKeyDown :: (Enum k, Monoid e) => k -> Wire s e IO a e
isKeyDown k =
  mkGen_ $ \_ -> do
    s <- getKey k
    return $ case s of
      Press   -> Right mempty
      Release -> Left  mempty

speed :: Monoid e => Wire s e IO a Float
speed  =  pure ( 0.0) . isKeyDown (CharKey 'A') . isKeyDown (CharKey 'D')
      <|> pure (-0.5) . isKeyDown (CharKey 'A')
      <|> pure ( 0.5) . isKeyDown (CharKey 'D')
      <|> pure ( 0.0)

pos :: HasTime t s => Wire s () IO a Float
pos = integral 0 . speed


s :: Float
s = 0.05

y :: Float
y = 0.0

renderPoint :: (Float, Float) -> IO ()
renderPoint (x, y) = vertex $ Vertex2 (realToFrac x :: GLfloat)
                                      (realToFrac y :: GLfloat)

generatePoints :: Float -> Float -> Float -> [(Float, Float)]
generatePoints x y s =
  [ (x - s, y - s)
  , (x + s, y - s)
  , (x + s, y + s)
  , (x - s, y + s)
  ] 

runNetwork :: (HasTime t s) => IORef Bool -> Session IO s -> Wire s e IO a Float -> IO ()
runNetwork closedRef session wire = do
  pollEvents
  closed <- readIORef closedRef
  if closed
    then return ()
    else do
      (st , session') <- stepSession session
      (wt', wire'   ) <- stepWire wire st $ Right undefined

      case wt' of
        Left  _ -> return ()
        Right x -> do
          clear [ColorBuffer]
          renderPrimitive Quads $
            mapM_ renderPoint $ generatePoints x y s
          swapBuffers

          runNetwork closedRef session' wire'
          
main :: IO ()
main = do
  initialize
  openWindow (Size 640 480) [DisplayRGBBits 8 8 8, DisplayAlphaBits 8, DisplayDepthBits 24] Window

  closedRef <- newIORef False
  windowCloseCallback $= do
    writeIORef closedRef True
    return True

  runNetwork closedRef clockSession_ pos

  closeWindow

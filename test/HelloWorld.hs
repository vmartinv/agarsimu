module Main where

import Prelude hiding ((.), id)
import Control.Lens hiding (at, perform, wrapped)
import Data.VectorSpace
import AgarSimu

main :: IO ()
main = runSimulation builder

builder :: Builder ()
builder = do setWorldSize (100, 100)   --Dimensiones del mapa
             setWindowSize (600, 600)  --Dimensiones de la pantalla
                                       --(en pixels)
--newBola crea una bola con una masa de 15 a 30,
--la posicion y el color son aleatorios
             bola1 <- newBola (15, 30) 

--addBola agrega una bola junto con su AI a la escena
             addBola derecha bola1

--Otras formas de crear bolas
             let bola2 = Bola (rgb 0 0 0) (50, 90) (10)
             bola3 <- set bolColor (rgb 0 255 0) <$> newBola (15, 30)

             addBolas randomAI [bola2, bola3]
--Nota: randomAI esta definida en AgarSimu.PreFab, alli hay
--      mas AI de ejemplo y funciones utiles.

--derecha siempre devuelve el vector (1, 0)
derecha :: AI
derecha = pure (1, 0)

--En PublicEntities.hs se defininen los tipos Bola, Environmnent, etc.

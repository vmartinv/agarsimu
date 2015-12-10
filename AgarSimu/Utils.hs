{-# LANGUAGE Arrows #-}
module AgarSimu.Utils where
            
import Prelude hiding ((.), id, until)
import Data.Either
import Control.Monad.Fix (MonadFix)
import Control.Wire
import Control.Wire.Unsafe.Event
import Control.Monad.Random

addBaseCase :: (MonadFix m, Monad m) => a -> Wire s e m a a -> Wire s e m a' a
addBaseCase init w = proc _ -> do
        rec input <- w . delay init -< input
        returnA -< input
        
-- ~ loop' :: (MonadFix m, Monad m) => (b, d) -> Wire s e m (b, d) (c, d) -> Wire s e m b c
-- ~ loop' init w = proc b -> do
        -- ~ rec input <- w . delay init -< (b, snd input)
        -- ~ returnA -< fst input

multicast :: (Monad m, Monoid s) => [Wire s e m a b] -> Wire s e m [a] [b]
multicast wires = mkGen $ \dt inputs -> do
        let stepper w x = stepWire w dt (Right x)
        res <- sequence $ zipWith stepper wires inputs
        let (outputs, wires') = unzip $ filter (isRight.fst) res
        return (Right (rights outputs), multicast wires')
        
mkGen_' :: Monad m => (a -> m b) -> Wire s e m a b
mkGen_' f = let f' x = fmap Right (f x)
            in mkGen_ f'

addMonad :: Monad m => WireP s e a b -> Wire s e m a b
addMonad = mapWire $ return.runIdentity

delRandom' :: Wire s e (Rand StdGen) a b -> Wire s e IO a b
delRandom' = mapWire $ \mr -> do
        g <- getStdGen
        let (y, g') = runRand mr g
        setStdGen g'
        return y
-- ~ type AI = forall g . RandomGen g => Wire (Timed NominalDiffTime ()) () (Rand g) Environment Vector

delRandom :: (Monad m, Monoid s) => g -> Wire s e (Rand g) a b -> Wire s e m a b
delRandom gen w = mkGen $ \dt x -> do
            let ((y, w'), gen') = runRand (stepWire w dt (Right x)) gen
            return (y, delRandom gen' w')

-- ~ holdDynamic :: (Monad m, Monoid s) => Either e b -> Wire s e m a (Event b) -> Wire s e m a b
-- ~ holdDynamic init w = mkGen $ \dt x -> do
        -- ~ (y, w') <- stepWire w dt (Right x)
        -- ~ let y' = case y of
                -- ~ Right (Event smt) -> Right smt
                -- ~ Right NoEvent     -> init
                -- ~ Left smt          -> Left smt
        -- ~ return (y', holdDynamic y' w')
        
useLast :: (Monad m, Monoid s) => Wire s e m a b -> Wire s e m [a] b
useLast w = mkGen $ \dt (x:xs) -> do
            let base = stepWire w dt (Right x)
            let stepper m z = do (_, w') <- m 
                                 stepWire w' mempty (Right z)
            (y, w'') <- foldl stepper base xs
            return (y, useLast w'')


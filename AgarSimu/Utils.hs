{-# LANGUAGE Arrows #-}
module AgarSimu.Utils where
            
import Prelude hiding ((.), id, until)
import Data.Either
import Control.Monad.Fix (MonadFix)
import Control.Wire

addFeedback :: (MonadFix m, Monad m) => a -> Wire s e m a a -> Wire s e m a' a
addFeedback init w = proc _ -> do
        rec input <- w . delay init -< input
        returnA -< input

multicast :: (Monad m, Monoid s) => [Wire s e m a b] -> Wire s e m [a] [b]
multicast wires = mkGen $ \dt inputs -> do
        let stepper w x = stepWire w dt (Right x)
        res <- sequence $ zipWith stepper wires inputs
        let (outputs, wires') = unzip $ filter (isRight.fst) res
        return (Right (rights outputs), multicast wires')
        
mkGen_' :: Monad m => (a -> m b) -> Wire s e m a b
mkGen_' f = let f' x = fmap Right (f x)
            in mkGen_ f'

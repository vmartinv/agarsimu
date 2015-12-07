import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)  
import Control.Monad.State (MonadState, StateT, get, put, runStateT, withStateT)
import Data.Maybe
import Data.Either

type Time = Float
type Dist = Float
type Vec = (Dist, Dist)

type Bola = (Vec, Dist)
type Player = (String, [Bola])
data GameSt = GameSt {time::Time, myself::Player, others::[Player]}

empty2 :: GameSt
empty2 = GameSt {time = 0,
                myself = ("martin", [((0, 0), 1)]),
                others = []}

newtype Behaviour s a = B {runBehaviour::s -> a}
instance Monad (Behaviour s) where
    return = B . const
    m >>= f = B (\s -> runBehaviour (f (runBehaviour m s)) s)
    
-- ~ instance Monoid a => Monoid (Behaviour s a) where
    -- ~ mempty = return mempty
    -- ~ mappend f g = B (\s -> mappend (runBehaviour f s) (runBehaviour g s))
    
-- Para calmar al GHC
instance Functor (Behaviour s) where
    fmap = liftM
 
instance Applicative (Behaviour s) where
    pure   = return
    (<*>)  = ap 
    
type AI m = StateT m (Behaviour GameSt)

newtype AI2 a = AI2 (GameSt -> (Vec, AI2 a))

right :: AI () Vec
right = return (1, 0)

-- ~ conv :: AI ma a -> (ma -> mb) -> (mb -> ma) -> AI mb a
-- ~ conv ai = 

-- ~ combine :: AI ma a -> AI mb a -> StateT mb (AI ma a)
-- ~ combine f g = do x <- f
                 -- ~ y <- g
                 -- ~ s <- get
                 -- ~ if isLeft s
                    -- ~ then f
                    -- ~ else g

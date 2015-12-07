import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)  
import Control.Monad.State  
import Data.Maybe

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
    
type AI a = Behaviour GameSt a
type AIWMem m a = Behaviour GameSt (State m a)
--newtype Action s = Behaviour s (Maybe Vec)

right :: AI Vec
right = return (1, 0)

stateless :: m -> AIWMem m a -> AI a
stateless init ai = ai >>= (\st -> return (fst (runState st init)))
--stateless init ai = ai >>= (\st -> return (0, 0))

-- newtype Action s a = A {runAction::s -> Maybe a}
-- instance Monad (Action s) where
    -- return = A . const . Just
    -- m >>= f = A (\s -> ((runAction m s) >>= (\a -> runAction (f a) s)))
    
    
addstate :: AI a -> AIWMem m a
addstate ai = ai >>= return . return


type Action a = AI (Maybe a) 
type ActionWMem m a = AIWMem m (Maybe a) 
--instance Monad (Action s) where
    --return = B . const . Just
    --return x = B (\s -> Just x)
    --m >>= f = B (\s -> ((runBehaviour m s) >>= (\a -> runBehaviour (f a) s)))
    
combine3 :: ActionWMem ma a -> ActionWMem mb a -> ActionWMem (Either ma mb) a
combine3 f g = do x <- conv f
                  y <- conv g
                  return (do m <- get
                             if isLeft m
                                then maybe (put Right >> return y) (const (return y)) x)
                                else return y
        where conv ac = ac>=(\st -> <$> st
do x <- f
                  y <- g
                  return (do s <- get
                             case s of
                               True -> return y
                               False -> maybe (put True >> return y) (const (return y)) x)
                 
                 
-- ~ combine :: Action a -> Action a -> ActionWMem Bool a
-- ~ combine f g = do x <- f
                 -- ~ y <- g
                 -- ~ return (do s <- get
                            -- ~ case s of
                                -- ~ True -> return y
                                -- ~ False -> maybe (put True >> return y) (const (return y)) x)
                 
-- ~ (<|>) :: Action a -> Action a -> Action a
-- ~ f <|> g = stateless False (combine f g)

conv :: Action a -> a -> AI a
conv ac e = do x <- ac
               return (maybe e id x)

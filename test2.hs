{-# LANGUAGE TemplateHaskell, TypeOperators #-}
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)  
import Data.Maybe
import Control.Category
import Data.Label
import Prelude hiding ((.), id)


type Time = Float
type Dist = Float
type Vec = (Dist, Dist)

type Bola = (Vec, Dist)
type Player = (String, [Bola])
getPos :: Player -> Vec
getPos = fst . head . snd
data GameSt = GameSt {_time::Time, _myself::Player, _others::[Player]} deriving Show

$(mkLabels[''GameSt])

emptySt :: GameSt
emptySt = GameSt {_time = 0,
                _myself = ("martin", [((0, 0), 1)]),
                _others = []}

newtype AI m a = AI {runAI :: GameSt -> m -> (a, m)}


instance Monad (AI m) where
    return x = AI (\_-> \m ->(x, m))
    ai >>= f = AI (\s -> \m ->
                let (a, m') = runAI ai s m
                in runAI (f a) s m')

-- Para calmar al GHC
instance Functor (AI s) where
    fmap = liftM
 
instance Applicative (AI s) where
    pure   = return
    (<*>)  = ap 


memoryless :: m -> AI m a -> AI () a
memoryless init ai = AI (\s -> \_ -> (fst (runAI ai s init), ()))
--mentira!

memoryfull :: AI () a -> AI m a
memoryfull ai = AI (\s -> \m -> (fst (runAI ai s ()), m))

getMem :: AI m m
getMem = AI (\s -> \m -> (m, m))
putMem :: m -> AI m ()
putMem m = AI (\s -> \_ -> ((), m))
getSt :: AI m GameSt
getSt = AI (\s -> \m -> (s, m))

type Action m a = AI m (Maybe a)

combine :: Action () a -> Action () a -> Action Bool a
combine f g = do x <- memoryfull f
                 y <- memoryfull g
                 s <- getMem
                 case s of
                    True -> return y
                    False -> maybe (putMem True >> return y) (const (return y)) x

(<|>) :: Action () a -> Action () a -> Action () a
f <|> g = memoryless False (combine f g)

right :: AI () Vec
right = return (0, 0)

wait :: Time -> Action () Vec
wait t = memoryless 0 (do elapsed <- getMem
                          if elapsed>t
                            then return Nothing
                            else (return.return) (0, 0))

diff :: Vec -> Vec -> Vec
diff (u,v) (p,q) = (u-p, v-q)

suma :: Vec -> Vec -> Vec
suma (u,v) (p,q) = (p+u, q+v)

norma :: Vec -> Dist
norma (a, b) = sqrt (a*a+b*b)

go :: Vec -> Action () Vec
go dst = do p <- getPos <$> (get myself <$> getSt)
            if norma (diff dst p) > 0.001
               then (return.return) (diff dst p)
               else return Nothing

conv :: a -> Action m a -> AI m a
conv def ac = do r <- ac
                 return (maybe def id r)

unit :: Vec -> Vec
unit p@(u, v) = case norma p of
                0 -> (0, 0)
                n -> (u/n, v/n)
              
tick :: GameSt -> GameSt
tick s = let t = get time s in set time (t+0.1) s
simulate :: GameSt -> AI () Vec -> GameSt
simulate s ai = let p = unit (fst (runAI ai s ()))
                in tick (set myself ("martin", [(suma p (getPos (get myself s)),1)]) s)


simu :: GameSt -> AI () Vec -> IO ()
simu s ai = do putStrLn (show s')
               if get time s'>=100
                then return ()
                else simu s' ai
            where s' = simulate s ai
-- ~ AI (\s -> \m -> 
                  -- ~ let (a, m') =  runAI ac m s
                  -- ~ in (maybe def fromJust a, m'))

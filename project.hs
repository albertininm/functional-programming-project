import System.IO
import Data.Char

type Env = [(String, Int)]

newtype M a = StOut (Env -> (a, Env, String))

unStOut (StOut f) = f

-- Defining M as instance of Functor, Applicative and Monad

instance Functor M where
  --fmap :: (a->b) -> M a -> M b
  fmap f m = StOut (\s -> let (a, st, str) = unStOut m s in (f a, st, str))

instance Applicative M where
  -- pure :: a -> M a
  pure a = StOut (\s -> (a, s, []))

  -- <*> :: M (a -> b) -> M a -> M b
  mf <*> ma = StOut (\s -> 
    let (a, st, str) = unStOut mf s in 
      let (a', st', str') = unStOut ma st in (a a', st', str'))

instance Monad M where
  -- (return) :: a -> Ma
  return x = pure x

  -- (>>=) :: M a -> (a -> M b) -> M b
  ma >>= f = StOut (\s -> let (a, s1, str1) = (unStOut ma) s 
                              (b, s2, str2) = unStOut (f a) s
                              in (b, s2, str1++str2))

-- Declaring AST

data Exp = Constant Int 
  | Variable String
  | Minus Exp Exp
  | Greater Exp Exp
  | Times Exp Exp
  deriving Show

data Com = Assign String Exp
  | Seq Com Com
  | Cond Exp Com Com
  | While Exp Com
  | Declare String Exp Com
  | Print Exp
  deriving Show

position :: String -> Env -> Int
position name env = positionAux name env 1

positionAux :: String -> Env -> Int -> Int
positionAux _ [] _ = -1
positionAux name ((n, v):ns) counter = if name == n
                                then counter
                                else positionAux name ns (counter+1)

fetch :: Int -> Env -> Int
fetch _ [] = -1
fetch n ((k, v):ss) = if n == 1 then v
                 else fetch (n-1) ss

getFrom :: String -> Env -> M Int
getFrom v env = StOut (\s -> ((fetch (position v env) env), s,[]))

eval1 :: Exp -> Env -> M Int
eval1 exp env = case exp of
  Constant n -> return n
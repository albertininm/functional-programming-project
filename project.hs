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
  | Plus Exp Exp
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

write :: String -> Int -> Env -> M ()
write var val env = do
  StOut (\s -> let updatedEnv = writeValue var val env in (val, updatedEnv, ""))
  return ()

writeValue :: String -> Int -> Env -> Env
writeValue var val [] = [(var, val)]
writeValue var val ((name, value):ss) | (name == var) = ((var, val):ss)
                                      | otherwise = [(name, value)] ++ (writeValue var val ss)

eval1 :: Exp -> Env -> M Int
eval1 exp env = case exp of
  Constant n -> return n
  Variable x -> getFrom x env
  Minus exp1 exp2 -> do {
    val1 <- (eval1 exp1 env);
    val2 <- (eval1 exp2 env);
    return (val1 - val2)
  }
  Plus exp1 exp2 -> do {
    val1 <- (eval1 exp1 env);
    val2 <- (eval1 exp2 env);
    return (val1 + val2)
  }
  Greater exp1 exp2 -> do {
    val1 <- (eval1 exp1 env);
    val2 <- (eval1 exp2 env);
    return (max val1 val2)
  }
  Times exp1 exp2 -> do {
      val1 <- (eval1 exp1 env);
      val2 <- (eval1 exp2 env);
      return (val1 * val2)
  }

exec :: Com -> Env -> M ()
exec stmt env = case stmt of
  Assign var exp -> do {
    res <- eval1 exp env;
    write var res env;
    return ()
  }
  Seq cmd1 cmd2 -> do {
    x <- exec cmd1 env;
    y <- exec cmd2 env;
    return ()
  }
module ParseWhile where
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import System.IO
import Data.Char


data Exp = ExpC Op Exp Exp | Var String | Number Integer deriving Show
data Op = Greater | Less | Equals | Plus | Minus | Times | Divide deriving Show

data Com = Assign String Exp
  | Declare String Exp Com
  | Seq [Com]
  | IfElse Exp Com Com
  | While Exp Com
  | Printe Exp
  deriving Show

  

languageDef =
   emptyDef { Token.commentStart    = "{-"
            , Token.commentEnd      = "-}"
            , Token.commentLine     = "--"
            , Token.identStart      = letter
            , Token.identLetter     = alphaNum
            , Token.reservedNames   = [ "if"
                                      , "then"
                                      , "else"
                                      , "while"
                                      , "do"
                                      , "declare"
                                      , "in"
                                      , "printe"

                                      ]
            , Token.reservedOpNames = ["+", "-", "*", "/", "=", ":="
                                      , "<", ">"
                                      ]
            }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
braces     = Token.braces     lexer -- parses surrounding parenthesis:
                                    --   braces p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   braces p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them

integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace

initParser :: Parser Com
initParser = whiteSpace >> sequenceOfCom

comParser :: Parser Com
comParser = braces sequenceOfCom
            <|> statement

sequenceOfCom :: Parser Com
sequenceOfCom =
  do
  list <- (sepBy1 statement semi)
  return $ if length list == 1 then head list else Seq list

statement :: Parser Com
statement = assignStmt
  <|> ifElseStmt
  <|> whileStmt
  <|> declareStmt
  <|> printeStmt

assignStmt :: Parser Com
assignStmt =
  do
  var  <- identifier
  reservedOp ":="
  expr <- coreExpression
  return $ Assign var expr

ifElseStmt :: Parser Com
ifElseStmt = 
  do
  reserved "if"
  expr <- coreExpression
  reserved "then"
  seqCom1 <- comParser
  reserved "else"
  seqCom2 <- comParser
  return $ IfElse expr seqCom1 seqCom2

whileStmt :: Parser Com
whileStmt =
  do
  reserved "while"
  expr <- coreExpression
  reserved "do"
  seqCom <- comParser
  return $ While expr seqCom

declareStmt :: Parser Com
declareStmt =
  do
  reserved "declare"
  str <- identifier
  reserved "="
  expr <- coreExpression
  reserved "in"
  seqCom <- comParser
  return $ Declare str expr seqCom

printeStmt :: Parser Com
printeStmt =
  do
  reserved "printe"
  expr <- coreExpression
  return $ Printe expr


coreExpression :: Parser Exp
coreExpression = buildExpressionParser operators expression

operators = [[Infix (reservedOp ">" >> return (ExpC Greater)) AssocLeft],
  [Infix (reservedOp "<" >> return (ExpC Less)) AssocLeft],
  [Infix (reservedOp "=" >> return (ExpC Equals)) AssocLeft],
  [Infix (reservedOp "+" >> return (ExpC Plus)) AssocLeft],
  [Infix (reservedOp "-" >> return (ExpC Minus)) AssocLeft],
  [Infix (reservedOp "*" >> return (ExpC Times)) AssocLeft],
  [Infix (reservedOp "/" >> return (ExpC Divide)) AssocLeft]]

expression :: Parser Exp
expression = parens coreExpression 
  <|>liftM Var identifier
  <|> liftM Number integer

parseString :: String -> Com
parseString str =
  case parse initParser "" str of
    Left e  -> error $ show e
    Right r -> r
 
parseFile :: String -> IO Com
parseFile file =
  do program  <- readFile file
     case parse initParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r
-- ast <- parseFile "<filename>"





type Env = [(String, Integer)]

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


position :: String -> Env -> Integer
position name env = positionAux name env 1

positionAux :: String -> Env -> Integer -> Integer
positionAux _ [] _ = -1
positionAux name ((n, v):ns) counter = if name == n
                                then counter
                                else positionAux name ns (counter+1)

fetch :: Integer -> Env -> Integer
fetch _ [] = -1
fetch n ((k, v):ss) = if n == 1 then v
                 else fetch (n-1) ss

getFrom :: String -> Env -> M Integer
getFrom v env = StOut (\s -> ((fetch (position v env) env), s,[]))

write :: String -> Integer -> Env -> M ()
write var val env = do
  StOut (\s -> let updatedEnv = writeValue var val env in (val, updatedEnv, ""))
  return ()

writeValue :: String -> Integer -> Env -> Env
writeValue var val [] = [(var, val)]
writeValue var val ((name, value):ss) | (name == var) = ((var, val):ss)
                                      | otherwise = [(name, value)] ++ (writeValue var val ss)

eval1 :: Exp -> Env -> M Integer
eval1 exp env = case exp of
  Number n -> return n
  Var x -> getFrom x env
  ExpC Minus exp1 exp2 -> do {
    val1 <- (eval1 exp1 env);
    val2 <- (eval1 exp2 env);
    return (val1 - val2)
  }
  ExpC Plus exp1 exp2 -> do {
    val1 <- (eval1 exp1 env);
    val2 <- (eval1 exp2 env);
    return (val1 + val2)
  }
  ExpC Greater exp1 exp2 -> do {
    val1 <- (eval1 exp1 env);
    val2 <- (eval1 exp2 env);
    return (if val1 > val2 then 1 else 0)
  }
  ExpC Less exp1 exp2 -> do {
    val1 <- (eval1 exp1 env);
    val2 <- (eval1 exp2 env);
    return (if val1 < val2 then 1 else 0)
  }
  ExpC Equals exp1 exp2 -> do {
    val1 <- (eval1 exp1 env);
    val2 <- (eval1 exp2 env);
    return (if val1 == val2 then 1 else 0)
  }
  ExpC Times exp1 exp2 -> do {
      val1 <- (eval1 exp1 env);
      val2 <- (eval1 exp2 env);
      return (val1 * val2)
  }
  ExpC Divide exp1 exp2 -> do {
      val1 <- (eval1 exp1 env);
      val2 <- (eval1 exp2 env);
      return (quot val1 val2)
  }

exec :: Com -> Env -> M ()
exec stmt env = case stmt of
  Assign var exp -> do {
    res <- eval1 exp env;
    write var res env;
    return ()
  }
  Seq (x:y:zs) -> do {
    x <- exec x env;
    y <- exec y env;
    return ()
  }
  Printe exp -> do {
    res <- eval1 exp env;
    output res;
  }
  Declare var exp cmd -> do {
    val <- eval1 exp env;
    write var val env;
    x <- exec cmd env;
    return ()
  }
  IfElse exp cmd1 cmd2 -> do {
    val <- eval1 exp env;
    if val /= 0
      then exec cmd1 env;
      else exec cmd2 env;
  }
  While exp cmd -> do {
    val <- eval1 exp env;
    if val == 1
      then exec cmd env;
      else return ()
  }


output :: Show a => a -> M()
output v = StOut (\n -> ((), n, show v))

interp a = unStOut (exec a []) []
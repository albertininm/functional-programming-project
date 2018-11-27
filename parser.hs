module ParseWhile where
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


data Exp = ExpC Op Exp Exp | Var String | Number Integer deriving Show
data Op = Greater | Less | Equals | Plus | Minus | Times | Divide deriving Show

data Com = Assign String Exp
  | Declare String Exp Com
  | Seq Com Com
  | IfElse Exp Com Com
  | While Exp Com
  | Print Exp
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
                                      , "print"

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
  do list <- (sepBy1 statement semi)
     return $ Seq (head list) (head (tail list))


statement :: Parser Com
statement = assignStmt

assignStmt :: Parser Com
assignStmt =
  do var  <- identifier
     reservedOp ":="
     expr <- coreExpression
     return $ Assign var expr

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
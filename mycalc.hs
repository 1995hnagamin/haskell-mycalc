import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Expr
import Text.Parsec.Language (haskellDef)
import Control.Monad.Identity
import Data.List

-- AST

data Expr = Number Integer
          | Variable String
          | Lambda String Expr
          | App Expr Expr
          | Neg Expr
          | Plus Expr Expr
          | Minus Expr Expr
          | Times Expr Expr
          | Div Expr Expr

instance Show (Expr) where
    show (Number x) = show x
    show (Variable v) = v
    show (Lambda x m) = "\\" ++ x ++ "." ++ show m
    show (App m n@(App _ _)) = (show m) ++ " (" ++ (show n) ++ ")"
    show (App m@(Lambda _ _) n) = "(" ++ (show m) ++ ") " ++ (show n)
    show (App m n) = (show m) ++ " " ++ (show n)
    show (Neg (Number x)) = "-" ++ (show x)
    show (Neg x) = "(- " ++ (show x) ++ ")"
    show (Plus x y) = "(+ " ++ (show x) ++ " " ++ (show y) ++ ")"
    show (Minus x y) = "(- " ++ (show x) ++ " " ++ (show y) ++ ")"
    show (Times x y) = "(* " ++ (show x) ++ " " ++ (show y) ++ ")"
    show (Div x y) = "(/ " ++ (show x) ++ " " ++ (show y) ++ ")"

divExpr :: Expr -> Expr -> Expr
divExpr (Number x) (Number y) = Number $ x `div` y

-- lexer

lexer :: P.TokenParser st
lexer = P.makeTokenParser haskellDef

parens :: Parser a -> Parser a
parens = P.parens lexer

identifier :: Parser String
identifier = P.identifier lexer

natural :: Parser Integer
natural = P.natural lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

-- parser

term :: Parser Expr
term = liftM Number natural
   <|> liftM Variable identifier
   <|> parens parseExpr

parseApp :: Parser Expr
parseApp = do
    exprs <- many1 term
    return $ foldl1 App exprs

table :: [[Operator String () Identity Expr]]
table = [
    [opPrefix "-" Neg],
    [opInfix "*" Times AssocLeft, opInfix "/" Div   AssocLeft],
    [opInfix "+" Plus  AssocLeft, opInfix "-" Minus AssocLeft]]
    where
        opInfix name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
        opPrefix name fun = Prefix (do{ reservedOp name; return fun })

parseArith :: Parser Expr
parseArith = buildExpressionParser table parseApp

parseLambdaExpr :: Parser Expr
parseLambdaExpr = do
    reservedOp "\\"
    vars <- many1 identifier
    reservedOp "->"
    body <- parseExpr
    return $ foldr Lambda body vars

parseExpr :: Parser Expr
parseExpr = parseLambdaExpr <|> parseArith

-- eval

fv :: Expr -> [String]
fv (Number n) = []
fv (Variable v) = [v]
fv (Lambda x m) = (fv m) \\ [x]
fv (App m n) = union (fv m) (fv n)
fv (Neg x) = fv x
fv (Plus x y) = union (fv x) (fv y)
fv (Minus x y) = union (fv x) (fv y)
fv (Times x y) = union (fv x) (fv y)
fv (Div x y) = union (fv x) (fv y)

gensym :: [String] -> String -> String -> String
gensym fvs x y = 
    head . filter (\v -> ((not $ elem v fvs) && v /= x)) . 
        map ((y++).show) $ [1..]

subst :: Expr -> String -> Expr -> Expr
subst x@(Number _) _ _ = x
subst (Variable y) x n = if y == x then n else Variable y
subst (Lambda y m) x n
    | y == x    = (Lambda y m)
    | otherwise = Lambda z (subst (subst m y (Variable z)) x n)
        where
            z = gensym (fv (App m n)) x y
subst (App m1 m2) x n = App (subst m1 x n) (subst m2 x n)
subst (Neg m) x n = Neg (subst m x n)
subst (Plus m1 m2) x n = Plus (subst m1 x n) (subst m2 x n)
subst (Minus m1 m2) x n = Minus (subst m1 x n) (subst m2 x n)
subst (Times m1 m2) x n = Times (subst m1 x n) (subst m2 x n)
subst (Div m1 m2) x n = Div (subst m1 x n) (subst m2 x n)

eval :: Expr -> Expr

eval v@(Variable x)        = v
eval (Lambda x m)          = Lambda x $ eval m
eval (App m@(App m1 m2) n) = eval (App (eval m) $ n)
eval (App (Lambda x m) n)  = eval $ subst m x $ eval n
eval (App m n) = App m n

eval n@(Number x)= n 
eval (Neg m)     = negEval (eval m)
eval (Plus m n)  = opEval "+" (eval m) (eval n)
eval (Minus m n) = opEval "-" (eval m) (eval n)
eval (Times m n) = opEval "*" (eval m) (eval n)
eval (Div m n)   = opEval "/" (eval m) (eval n)

negEval (Number a) = Number (-a)
negEval x = Neg x

opEval "+" (Number a) (Number b) = Number $ a + b
opEval "+" x y = Plus x y
opEval "-" (Number a) (Number b) = Number $ a - b
opEval "-" x y = Minus x y
opEval "*" (Number a) (Number b) = Number $ a * b
opEval "*" x y = Times x y
opEval "/" (Number a) (Number b) = Number $ div a b
opEval "/" x y = Div x y

-- exec

main :: IO()
main = do
    str <- getLine :: IO String
    case parse parseExpr "calc" str of 
        Left err -> putStrLn . show $ err
        Right x  -> do
            putStrLn . show $ x
            putStrLn . show . eval $ x

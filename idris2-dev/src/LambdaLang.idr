module LambdaLang

import Data.List
import Prelude.Types

import Text.Lexer
import public Text.Parser.Core
import public Text.Parser


-- lexer

data Token = Num Integer
           | Sym String
           | LParen
           | RParen
           | Comment String
           | EndInput

Eq Token where
  (Num i) == (Num j) = i == j
  (Sym a) == (Sym b) = a == b
  LParen == LParen = True
  RParen == RParen = True
  (Comment _) == (Comment _) = True
  EndInput == EndInput = True
  _ == _ = False

Show Token where
  show (Num x) = show x
  show (Sym x) = x
  show LParen = "("
  show RParen = ")"
  show (Comment x) = ";" ++ x ++ "\n"
  show EndInput = ""

toInt' : String -> Integer
toInt' = cast

lispSymbolChar : Lexer
lispSymbolChar = alphaNum <|> oneOf "+-*/_#!@'`&%=;:,.<>"

lispSymbol : Lexer
lispSymbol = lispSymbolChar <+> (many lispSymbolChar <|> digit)

lispComment : Lexer
lispComment = is ';' <+> many (isNot '\n')

expressionTokens : TokenMap Token
expressionTokens = [
    (digits, \x => Num (toInt' x)),
    (lispSymbol, \x => Sym x),
    (is '(' ,\x => LParen),
    (is ')' ,\x => RParen),
    (spaces, Comment),
    (lispComment, Comment)
]

stripWhitespace : (List (WithBounds Token), (Int, (Int, String))) -> (List (WithBounds Token), (Int, (Int, String)))
stripWhitespace (x, y) = ((filter notComment x), y) where
    notComment : (WithBounds Token) -> Bool
    notComment (MkBounded (Comment _) _ _) = False
    notComment _ = True


--parser

data Ast = Number Integer
         | Symbol String
         | Lambda String Ast
         | Apply Ast Ast

Interpolation Ast where
  interpolate (Number i) = show i
  interpolate (Symbol str) = str
  interpolate (Lambda p b) = "(lambda (\{p}) \{b})"
  interpolate (Apply f a) = "(\{f} \{a})"

Show Ast where
  show = interpolate

Rule : Type -> Type
Rule ty = Grammar () Token True ty

parseNum : Rule Integer
parseNum = terminal "<integer literal>" 
                    (\x => case x of
                              (Num i) => Just i
                              _ => Nothing)

parseSym : Rule String
parseSym = terminal "<symbol>" 
                    (\x => case x of
                              (Sym s) => Just s
                              _ => Nothing)

consume : Token -> Grammar () Token True ()
consume tok  = terminal "<symbol>" (\x => if x == tok then Just () else Nothing)

parseExp : Rule Ast

parseLam : Rule Ast
parseLam = do consume LParen
              consume (Sym "lambda")
              consume LParen
              p <- parseSym
              consume RParen
              b <- parseExp
              consume RParen
              pure (Lambda p b)

parseApp : Rule Ast
parseApp = do consume LParen
              f <- parseExp
              a <- parseExp
              consume RParen
              pure (Apply f a)

parseExp = map Number parseNum
       <|> map Symbol parseSym
       <|> parseLam
       <|> parseApp



-- interpreter

data Value = I Integer
           | F (Value -> Maybe Value)

showVal : Value -> String
showVal (I i) = show i
showVal (F f) = "->"


Env : Type
Env = List (String, Value)

envLookup : String -> Env -> Maybe Value
envLookup _ [] = Nothing
envLookup s ((k, v) :: es) = if k == s then Just v else envLookup s es

envExtend : String -> Value -> Env -> Env
envExtend k v es = (k, v) :: es


eval : Ast -> Env -> Maybe Value
eval (Number i) _ = Just (I i)
eval (Symbol s) env = envLookup s env
eval (Lambda p b) env = Just (F (\x => (eval b (envExtend p x env))))
eval (Apply f a) env = do p <- eval f env
                          v <- eval a env
                          case p of
                            (F g) => g v
                            _ => Nothing

defaultEnv = [
  ("Z", I 0),
  ("S", F (\x => case x of 
                   (I i) => Just (I (i + 1))
                   _ => Nothing))
]

main : IO ()
main = do putStr "alg>"
          x <- getLine 
          case (parse parseExp (fst (stripWhitespace (lex expressionTokens x)))) of
            (Left errs) => putStrLn ("Error: " ++ (show errs))
            (Right (exp, _)) => putStrLn (show (map showVal (eval exp defaultEnv)))

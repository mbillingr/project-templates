module LambdaLang

import Data.List
import Prelude.Types
import System.REPL

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
lispSymbolChar = alphaNum <|> oneOf "+-*/_#!?@'`&%=;:,.<>"

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
         | Let String Ast Ast
         | Define String Ast

Interpolation Ast where
  interpolate (Number i) = show i
  interpolate (Symbol str) = str
  interpolate (Lambda p b) = "(lambda (\{p}) \{b})"
  interpolate (Apply f a) = "(\{f} \{a})"
  interpolate (Let v a b) = "(let ((\{v} \{a})) \{b})"
  interpolate (Define v x) = "(define \{v} \{x})"

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

parseLet = do consume LParen
              consume (Sym "let")
              consume LParen
              consume LParen
              v <- parseSym
              a <- parseExp
              consume RParen
              consume RParen
              b <- parseExp
              consume RParen
              pure (Apply (Lambda v b) a)

parseDef = do consume LParen
              consume (Sym "define")
              key <- parseSym
              val <- parseExp
              consume RParen
              pure (Define key val)

parseExp = map Number parseNum
       <|> map Symbol parseSym
       <|> parseLam
       <|> parseApp
       <|> parseLet
       <|> parseDef



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


mutual
  apply : Ast -> Ast -> Env -> Maybe Value
  apply f a env = do p <- fst (eval f env)
                     v <- fst (eval a env)
                     case p of
                       (F g) => g v
                       _ => Nothing

  evalDefine : String -> Ast -> List (String, Value) -> (Maybe Value, List (String, Value))
  evalDefine v x env = case (eval x env) of
                         (Nothing, _) => (Nothing, env)
                         ((Just y), _) => (Nothing, (envExtend v y env))

  eval : Ast -> Env -> (Maybe Value, Env)
  eval (Number i) env = (Just (I i), env)
  eval (Symbol s) env = (envLookup s env, env)
  eval (Lambda p b) env = (Just (F (\x => (fst (eval b (envExtend p x env))))), env)
  eval (Apply f a) env = ((apply f a env), env)
  eval (Let _ _ _) env = (Nothing, env)
  eval (Define v x) env = evalDefine v x env

defaultEnv = [
  ("Z", I 0),
  ("S", F (\x => case x of 
                   (I i) => Just (I (i + 1))
                   _ => Nothing)),
  ("Z?", (F (\x => Just (F (\zz => Just (F (\nz => case x of
                                                        (I 0) => Just zz
                                                        _ => Just nz)))))))
]

parse_and_eval : Env -> String -> Maybe (String, Env)
parse_and_eval env src = case (parse parseExp (fst (stripWhitespace (lex expressionTokens src)))) of
                          (Left errs) => Just ("Error: " ++ (show errs) ++ "\n", env)
                          (Right (exp, _)) => let (res, env) = (eval exp env) 
                                              in Just (show (map showVal res) ++ "\n", env)
                       

main : IO ()
main = do replWith defaultEnv "alg>" parse_and_eval

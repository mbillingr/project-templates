module SomeLang

import Data.List
import Prelude.Types

import Text.Lexer
import public Text.Parser.Core
import public Text.Parser


public export
data ExpressionToken = Number Integer
         | Operator String
         | OParen
         | CParen
         | Comment String
         | EndInput

export
Show ExpressionToken where
  show (Number x) = "number " ++ show x
  show (Operator x) = "operator " ++ x
  show OParen = "("
  show CParen = ")"
  show (Comment x) = "--" ++ x
  show EndInput = "end of input"

-- I don't think the l/c/t params are correct for MkBounded
--export
--Show (WithBounds ExpressionToken) where
--  show (MkBounded l c t) = "line=" ++ show l ++ " col=" ++ show c ++ "tok=" ++ show t


export
opChars : String
opChars = "+-*"

operator : Lexer
operator = some (oneOf opChars)

toInt' : String -> Integer
toInt' = cast

expressionTokens : TokenMap ExpressionToken
expressionTokens = [
    (digits, \x => Number (toInt' x)),
    (operator, \x => Operator x),
    (is '(' ,\x => OParen),
    (is ')' ,\x => CParen),
    (spaces, Comment)
]

-- end of lexer

-- start of parser

public export
Rule : Type -> Type
Rule ty = Grammar () ExpressionToken True ty

export
intLiteral : Rule Integer
intLiteral = terminal "<integer literal>" 
                      (\x => case x of
                                Number i => Just i
                                _ => Nothing)

export
openParen : Rule ()
openParen = terminal "(" (\x => case x of
                                  OParen => Just ()
                                  _ => Nothing)

export
closeParen : Rule ()
closeParen = terminal ")" (\x => case x of
                                   CParen => Just ()
                                   _ => Nothing)

export
paren : Rule a -> Rule a
paren exp = openParen *> exp <* closeParen


op : String -> Rule Integer
op s = terminal "op" (\x => case x of 
                              (Operator s1) => if s == s1 then Just 0 else Nothing
                              _ => Nothing)

addInt : Integer -> Integer -> Integer
addInt a b = a + b

export
add : {c1,c2,c3 : Bool} -> Grammar () tok c1 Integer -> Grammar () tok c2 Integer -> Grammar () tok c3 Integer -> Grammar () tok ((c1 || c2) || c3) Integer
add x y z = map addInt (x *> y) <*> z

subInt : Integer -> Integer -> Integer
subInt a b = a - b

export
sub : {c1,c2,c3 : Bool} -> Grammar () tok c1 Integer -> Grammar () tok c2 Integer -> Grammar () tok c3 Integer -> Grammar () tok ((c1 || c2) || c3) Integer
sub x y z = map subInt (x *> y) <*> z

mulInt : Integer -> Integer -> Integer
mulInt a b = a * b

export
mul : {c1,c2,c3 : Bool} -> Grammar () tok c1 Integer -> Grammar () tok c2 Integer -> Grammar () tok c3 Integer -> Grammar () tok ((c1 || c2) || c3) Integer
mul x y z = map mulInt (x *> y) <*> z

expr : Rule Integer
--expr = intLiteral <|>
--       do openParen
--          r <- expr
--          closeParen
--          pure r

factor : Rule Integer
factor = intLiteral <|> do
              openParen
              r <- expr
              closeParen
              pure r

term : Rule Integer
term = map mulInt factor <*> ((op "*")*> factor)
   <|> factor

expr = map addInt term <*> ((op "+") *> term)
   <|> map subInt term <*> ((op "-") *> term)
   <|> term


processWhitespace : (List (WithBounds ExpressionToken), (Int, (Int, String))) -> (List (WithBounds ExpressionToken), (Int, (Int, String)))
processWhitespace (x, y) = ((filter notComment x), y) where
    notComment : (WithBounds ExpressionToken) -> Bool
    notComment (MkBounded (Comment _) _ _) = False
    notComment _ = True

calc : String -> Either (List1 (ParsingError ExpressionToken)) (Integer, List (WithBounds ExpressionToken))
calc s = (parse expr (fst (processWhitespace (lex expressionTokens s))))

lft : (List1 (ParsingError ExpressionToken)) -> IO ()
lft (x ::: _) = putStrLn ("error: " ++ show x)

rht : (Integer, List (WithBounds ExpressionToken)) -> IO ()
rht i = putStrLn ("right " ++ (show i))

main : IO ()
main = do putStr "alg>"
          x <- getLine 
          either lft rht (calc x)

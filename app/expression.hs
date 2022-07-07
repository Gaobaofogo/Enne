{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Expression where

import Lexer
import Token
import Memory
import Text.Parsec

import Control.Monad.IO.Class
import System.IO.Unsafe

expression :: ParsecT [Token] MemoryList IO(Token)
expression = try bin_expression  <|> una_expression

una_expression :: ParsecT [Token] MemoryList IO(Token)
una_expression = literal_values <|> literal_from_name

literal_values :: ParsecT [Token] MemoryList IO(Token)
literal_values =  do
                    intToken <|> floatToken <|> stringToken

literal_from_name :: ParsecT [Token] MemoryList IO Token -- TODO
literal_from_name = do
  a <- idToken
  s <- getState
  let result = symtable_search a s
  if snd result then
    return $ (get_value_cell . fst) result
  else fail "Variável não encontrada"

-- literal_from_array:: ParsecT [Token] MemoryList IO(Token)
-- literal_from_array =  do
--                     a <- idToken
--                     b <- positionSequence
--                     s1 <- getState
--                     return (fromTypeX ( fst (symtableArraySearch s1 (getIndexes b []) (getVariableName a) "" ))) 

bin_expression :: ParsecT [Token] MemoryList IO(Token)
bin_expression = do
                   n1 <- intToken <|> floatToken <|> stringToken <|> literal_from_name
                   eval_remaining n1

eval_remaining :: Token -> ParsecT [Token] MemoryList IO(Token)
eval_remaining n1 = do
                      op <- addToken <|> subToken <|> multToken
                      n2 <- intToken <|> floatToken <|> stringToken <|> literal_from_name
                      eval_remaining (eval n1 op n2)
                    <|> return n1

-- Checando se os tipos na atribuição são compatíveis
areTypesCompatible :: (Token,Token) -> Bool
areTypesCompatible (String s1, String s2)     = True
areTypesCompatible (Int x1, Int x2)           = True
areTypesCompatible (Float y1, Float y2)       = True
areTypesCompatible (Float y1, Int x1)         = True
areTypesCompatible (Int x1, Float y1)         = True
areTypesCompatible (_,_)                      = False

convertTypeToValue :: Token -> Token
convertTypeToValue (Type "string") = String ""
convertTypeToValue (Type "int") = Int 0
convertTypeToValue (Type "float") = Float 0.0


eval :: Token -> Token -> Token -> Token
eval (Int x)    Add   (Int y)   = Int (x + y)
eval (Int x)    Sub   (Int y)   = Int (x - y)
eval (Int x)    Mult  (Int y)   = Int (x * y)
eval (Float x)  Add   (Float y) = Float (x + y)
eval (Float x)  Sub   (Float y) = Float (x - y)
eval (Float x)  Mult  (Float y) = Float (x * y)
eval (Float x)  Add   (Int y)   = Float (x + fromIntegral y)
eval (Float x)  Sub   (Int y)   = Float (x - fromIntegral y)
eval (Float x)  Mult  (Int y)   = Float (x * fromIntegral y)
eval (Int x)    Add   (Float y) = Float (fromIntegral x + y)
eval (Int x)    Sub   (Float y) = Float (fromIntegral x - y)
eval (Int x)    Mult  (Float y) = Float (fromIntegral x * y)
eval (String x) Add   (String y)= String (x ++ y)

-- boolean expressions

logicExpression :: ParsecT [Token] MemoryList IO([Token])
logicExpression = do
    a <- floatToken <|> intToken <|> expression <|> booleanToken
    b <- greaterToken <|> lowerToken <|> equalToToken
    c <- floatToken <|> intToken <|> expression <|> booleanToken
    result <- logic_remaining (logicComparative a b c)
    return result

logic_remaining :: Bool -> ParsecT [Token] MemoryList IO([Token])
logic_remaining bool = (do
    a <- logicalOpToken
    b <- floatToken <|> intToken <|> expression <|> booleanToken
    c <- greaterToken <|> lowerToken <|> equalToToken
    d <- floatToken <|> intToken <|> expression <|> booleanToken
    result <- logic_remaining  (logicOperation bool a (logicComparative b c d))
    return (result)) <|> (return [boolToToken bool])

boolToToken :: Bool -> Token
boolToToken True = (Boolean "true")
boolToToken False = (Boolean "false")

tokenToBool :: Token -> Bool
tokenToBool (Boolean "true") = True
tokenToBool (Boolean "false") = False

logicOperation :: Bool -> Token -> Bool -> Bool
logicOperation a (LogicalOp "&&") b = a && b
logicOperation a (LogicalOp "||") b = a || b

logicComparative :: Token -> Token -> Token -> Bool
logicComparative (Int x) Greater (Int y) = x > y
logicComparative (Float x) Greater (Float y) = x > y
logicComparative (Int x) Greater (Float y) = fromIntegral x > y
logicComparative (Float x) Greater (Int y) = x > fromIntegral y
-- logicComparative (Int x) (ComparativeOp ">=") (Int y) = x >= y
-- logicComparative (Float x) (ComparativeOp ">=") (Float y) = x >= y
-- logicComparative (Int x) (ComparativeOp ">=") (Float y) = fromIntegral x >= y
-- logicComparative (Float x) (ComparativeOp ">=") (Int y) = x >= fromIntegral y
logicComparative (Int x) Lower (Int y) = x < y
logicComparative (Float x) Lower (Float y) = x < y
logicComparative (Int x) Lower (Float y) = fromIntegral x < y
logicComparative (Float x) Lower (Int y) = x < fromIntegral y
-- logicComparative (Int x) (ComparativeOp "<=") (Int y) = x <= y
-- logicComparative (Float x) (ComparativeOp "<=") (Float y) = x <= y
-- logicComparative (Int x) (ComparativeOp "<=") (Float y) = fromIntegral x <= y
-- logicComparative (Float x) (ComparativeOp "<=") (Int y) = x <= fromIntegral y
logicComparative (Int x) EqualTo (Int y) = x == y
logicComparative (Float x) EqualTo (Float y) = x == y
logicComparative (Int x) EqualTo (Float y) = fromIntegral x == y
logicComparative (Float x) EqualTo (Int y) = x == fromIntegral y
-- logicComparative (Boolean x) EqualTo (Boolean y) = stringToBool x == stringToBool y
-- logicComparative (Boolean x) (ComparativeOp "!=") (Boolean y) = stringToBool x /= stringToBool y
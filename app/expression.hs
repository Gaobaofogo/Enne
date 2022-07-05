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

literal_from_name :: ParsecT [Token] MemoryList IO(Token) -- TODO
literal_from_name = do
  a <- idToken
  return a

-- literal_from_name :: ParsecT [Token] MemoryList IO(Token) -- TODO
-- literal_from_name =  do
--                     a <- idToken
--                     s1 <- getState
--                     return (fromTypeX ( fst (symtableSearch s1 (getVariableName a) "" )))

-- literal_from_array:: ParsecT [Token] MemoryList IO(Token)
-- literal_from_array =  do
--                     a <- idToken
--                     b <- positionSequence
--                     s1 <- getState
--                     return (fromTypeX ( fst (symtableArraySearch s1 (getIndexes b []) (getVariableName a) "" ))) 

bin_expression :: ParsecT [Token] MemoryList IO(Token)
bin_expression = do
                   n1 <- intToken <|> floatToken <|> stringToken
                   eval_remaining n1

eval_remaining :: Token -> ParsecT [Token] MemoryList IO(Token)
eval_remaining n1 = do
                      op <- addToken <|> subToken <|> multToken
                      n2 <- intToken <|> floatToken <|> stringToken
                      eval_remaining (eval n1 op n2)
                    <|> return n1

-- Checando se os tipos na atribuição são compatíveis
areTypesCompatible :: (Token,Token) -> Bool
areTypesCompatible (Type "string", Type "string")     = True
areTypesCompatible (Type "int", Type "int")           = True
areTypesCompatible (Type "float", Type "float")       = True
areTypesCompatible (Type "float", Type "int")         = True
areTypesCompatible (Type "int", Type "float")         = True

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
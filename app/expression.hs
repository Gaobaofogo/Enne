{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Expression where

import Lexer
import Token
import Memory
import Matrix
import Text.Parsec

import Control.Monad.IO.Class
import System.IO.Unsafe

expression :: ParsecT [Token] MemoryList IO(Token)
expression = try bin_expression  <|> una_expression

una_expression :: ParsecT [Token] MemoryList IO(Token)
una_expression = literal_values <|> literal_from_name

literal_values :: ParsecT [Token] MemoryList IO(Token)
literal_values =  do
                    intToken <|> floatToken <|> stringToken <|> literal_from_array

literal_from_name :: ParsecT [Token] MemoryList IO Token
literal_from_name = do
  a <- idToken
  s <- getState
  let result = symtable_search a s
  if snd result then
    return $ case fst result of
                MemoryCell id1 value -> value
                MemoryArray id2 t2 d2 arr2 -> Matrix t2 d2 arr2
  else fail "Variável não encontrada"
  -- if snd result then
  --   return $ (get_value_cell . fst) result
  -- else fail "Variável não encontrada"

literal_from_array :: ParsecT [Token] MemoryList IO Token
literal_from_array = do
  idT <- idToken
  s <- getState

  case symtable_search_array idT s of
    Left err -> fail "err"
    Right (MemoryArray id typeArr dim arr) -> return $ Matrix typeArr dim arr

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

bracketSequence :: ParsecT [Token] MemoryList IO([Token], [Token])
bracketSequence = do
  first <- bracketWithNumber
  next <- remainingBracketSequence
  let intSequence = fst first ++ fst next
  let tokenSequence = snd first ++ snd next
  return (intSequence, tokenSequence)

remainingBracketSequence :: ParsecT [Token] MemoryList IO([Token], [Token])
remainingBracketSequence = (do bracketSequence) <|> return ([], [])

bracketWithNumber :: ParsecT [Token] MemoryList IO([Token], [Token])
bracketWithNumber = do
  lB <- leftSquareBracketToken
  iT <- intToken
  rB <- rightSquareBracketToken

  return ([iT], [lB, iT, rB])

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

convertInputToType :: [Char] -> Token -> Token
convertInputToType x (Type "string") = String x
convertInputToType x (Type "float")  = Float (read x::Double)
convertInputToType x (Type "int")    = Int (read x::Int)

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
eval (Matrix t1 dim1 arr1) Add (Matrix t2 dim2 arr2) = Matrix t1 dim1 (array_sum arr1 arr2)

{-
Aqui eu posso colocar no eval mais uma linha e retornar um 
"token" de matrix, mas não faz sentido ser um token de matrix.
Como eu vou saber que eu tô fazendo soma de matrizes? Porra.
Tá, talvez eu possa colocar um token novo de matrix com as
dimensoes e os valores. O que vai dar? Eu vou ter dois
literal_from_array e literal_from_array_index.
O primeiro vai devolver o valor da matrix acima descrito
e o segundo eu vou pegar o valor no índice igual o timbux.
-}

-- boolean expressions

logicExpression :: ParsecT [Token] MemoryList IO([Token])
logicExpression = do
    a <- floatToken <|> intToken <|> expression <|> booleanToken
    b <- greaterToken <|> greaterOrEqualToken <|> lowerToken <|> lowerOrEqualToken <|> equalToToken <|> notEqualToToken
    c <- floatToken <|> intToken <|> expression <|> booleanToken
    result <- logic_remaining (logicComparative a b c)
    return result

logic_remaining :: Bool -> ParsecT [Token] MemoryList IO([Token])
logic_remaining bool = (do
    a <- logicalOpToken
    b <- floatToken <|> intToken <|> expression <|> booleanToken
    c <- greaterToken <|> greaterOrEqualToken <|> lowerToken <|> lowerOrEqualToken <|> equalToToken <|> notEqualToToken
    d <- floatToken <|> intToken <|> expression <|> booleanToken
    result <- logic_remaining  (logicOperation bool a (logicComparative b c d))
    return (result)) <|> (return [boolToToken bool])

boolToToken :: Bool -> Token
boolToToken True = (Boolean "true")
boolToToken False = (Boolean "false")

tokenToBool :: Token -> Bool
tokenToBool (Boolean "true") = True
tokenToBool (Boolean "false") = False

stringToBool :: String -> Bool
stringToBool "true" = True
stringToBool "false" = False

logicOperation :: Bool -> Token -> Bool -> Bool
logicOperation a (LogicalOp "&&") b = a && b
logicOperation a (LogicalOp "||") b = a || b

logicComparative :: Token -> Token -> Token -> Bool
logicComparative (Int x) Greater (Int y) = x > y
logicComparative (Float x) Greater (Float y) = x > y
logicComparative (Int x) Greater (Float y) = fromIntegral x > y
logicComparative (Float x) Greater (Int y) = x > fromIntegral y
logicComparative (Int x) GreaterOrEqual (Int y) = x >= y
logicComparative (Float x) GreaterOrEqual (Float y) = x >= y
logicComparative (Int x) GreaterOrEqual (Float y) = fromIntegral x >= y
logicComparative (Float x) GreaterOrEqual (Int y) = x >= fromIntegral y
logicComparative (Int x) Lower (Int y) = x < y
logicComparative (Float x) Lower (Float y) = x < y
logicComparative (Int x) Lower (Float y) = fromIntegral x < y
logicComparative (Float x) Lower (Int y) = x < fromIntegral y
logicComparative (Int x) LowerOrEqual (Int y) = x <= y
logicComparative (Float x) LowerOrEqual (Float y) = x <= y
logicComparative (Int x) LowerOrEqual (Float y) = fromIntegral x <= y
logicComparative (Float x) LowerOrEqual (Int y) = x <= fromIntegral y
logicComparative (Int x) EqualTo (Int y) = x == y
logicComparative (Float x) EqualTo (Float y) = x == y
logicComparative (Int x) EqualTo (Float y) = fromIntegral x == y
logicComparative (Float x) EqualTo (Int y) = x == fromIntegral y
logicComparative (Int x) NotEqualTo (Int y) = x /= y
logicComparative (Float x) NotEqualTo (Float y) = x /= y
logicComparative (Int x) NotEqualTo (Float y) = fromIntegral x /= y
logicComparative (Float x) NotEqualTo (Int y) = x /= fromIntegral y
-- logicComparative (Boolean x) EqualTo (Boolean y) = stringToBool x == stringToBool y
-- logicComparative (Boolean x) NotEqualTo (Boolean y) = stringToBool x /= stringToBool y
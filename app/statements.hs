{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Statement where

import Lexer
import Token
import Memory
import Text.Parsec
import Expression
import System.IO
import System.IO.Unsafe
import Control.Monad.IO.Class

-- while/for
statements :: ParsecT [Token] [(Token,Token)] IO [Token]
statements = do
        first <- attribution <|> ifStatement <|> whileStatement <|> funcStatement
        next  <- remaining_stmts
        return (first ++ next) <|> return []

remaining_stmts :: ParsecT [Token] [(Token,Token)] IO [Token]
remaining_stmts = (do statements) <|> return []

attribution :: ParsecT [Token] [(Token,Token)] IO[Token]
attribution = do
  a <- typeToken
  b <- idToken
  c <- assignToken
  d <- expression
  if areTypesCompatible(a, d) then
    updateState(symtable_insert (b, d))
  else fail "Os tipos não são compatíveis"
  -- Como posso melhorar esses erros?

  s <- getState
  liftIO (print s)
  e <- semiColonToken
  return [a, b, c, d, e]

ifStatement :: ParsecT [Token] [(Token,Token)] IO[Token]
ifStatement = do
  ifT <- ifToken
  lp <- leftParentesisToken
  le <- logicExpression
  rp <- rightParentesisToken
  bS <- blockStatement
  eS <- elseStatement <|> return []

  return ([ifT, lp] ++ le ++ [rp] ++ bS ++ eS)

elseStatement :: ParsecT [Token] [(Token,Token)] IO[Token]
elseStatement = do
  eT <- elseToken
  bS <- blockStatement

  return (eT : bS)

whileStatement :: ParsecT [Token] [(Token,Token)] IO[Token]
whileStatement = do
  wT <- whileToken
  lP <- leftParentesisToken
--   -- Precisa colocar aqui pra ler uma expressão booleana
  rP <- rightParentesisToken
  bS <- blockStatement

  return ([wT, lP, rP] ++ bS)

funcStatement :: ParsecT [Token] [(Token,Token)] IO[Token]
funcStatement = do
  fS <- funcToken
  iT <- idToken
  lP <- leftParentesisToken
  fA <- funcArgumentsStatement
  rP <- rightParentesisToken
  cT <- colonToken
  tT <- typeToken
  bS <- blockStatement

  return ([fS, iT, lP] ++ fA ++ [rP, cT, tT] ++ bS)

funcArgumentsStatement :: ParsecT [Token] [(Token,Token)] IO[Token]
funcArgumentsStatement = do
  first <- singleArgumentStatement <|> return []
  next <- remainingFuncArgumentsStatement
  return (first ++ next) <|> return []

remainingFuncArgumentsStatement :: ParsecT [Token] [(Token,Token)] IO[Token]
remainingFuncArgumentsStatement = ( do
  cmT <- commaToken
  faS <- funcArgumentsStatement
  return (cmT:faS)
  ) <|> return []

singleArgumentStatement :: ParsecT [Token] [(Token,Token)] IO[Token]
singleArgumentStatement = do
  tT <- typeToken
  iT <- idToken

  return [tT, iT]

blockStatement :: ParsecT [Token] [(Token,Token)] IO[Token]
blockStatement = do
  lb <- leftBlockToken
  stmts <- statements <|> return []
  rb <- rightBlockToken

  return ([lb] ++ stmts ++ [rb])
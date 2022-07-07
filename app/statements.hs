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
statements :: ParsecT [Token] MemoryList IO [Token]
statements = do
  first <- attribution <|> ifStatement <|> whileStatement <|> funcStatement
  next  <- remaining_stmts
  return (first ++ next) <|> return []

remaining_stmts :: ParsecT [Token] MemoryList IO [Token]
remaining_stmts = (do statements) <|> return []

attribution :: ParsecT [Token] MemoryList IO[Token]
attribution = do
  tT <- typeToken
  idT <- idToken
  aT <- assignToken
  e <- expression
  sT <- semiColonToken

  if areTypesCompatible (convertTypeToValue tT, e) then
    updateState $ symtable_insert $ MemoryCell idT e
  else fail "Tipos não são compatíveis"

  s <- getState
  liftIO (print s)

  return [tT, idT, aT, e, sT]

ifStatement :: ParsecT [Token] MemoryList IO[Token]
ifStatement = do
  ifT <- ifToken
  lp <- leftParentesisToken
  le <- logicExpression
  rp <- rightParentesisToken
  bS <- blockStatement
  eS <- elseStatement <|> return []

  return ([ifT, lp] ++ le ++ [rp] ++ bS ++ eS)

elseStatement :: ParsecT [Token] MemoryList IO[Token]
elseStatement = do
  eT <- elseToken
  bS <- blockStatement

  return (eT : bS)

whileStatement :: ParsecT [Token] MemoryList IO[Token]
whileStatement = do
  wT <- whileToken
  lP <- leftParentesisToken
--   -- Precisa colocar aqui pra ler uma expressão booleana
  rP <- rightParentesisToken
  bS <- blockStatement

  return ([wT, lP, rP] ++ bS)

funcStatement :: ParsecT [Token] MemoryList IO[Token]
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

funcArgumentsStatement :: ParsecT [Token] MemoryList IO[Token]
funcArgumentsStatement = do
  first <- singleArgumentStatement <|> return []
  next <- remainingFuncArgumentsStatement
  return (first ++ next) <|> return []

remainingFuncArgumentsStatement :: ParsecT [Token] MemoryList IO[Token]
remainingFuncArgumentsStatement = ( do
  cmT <- commaToken
  faS <- funcArgumentsStatement
  return (cmT:faS)
  ) <|> return []

singleArgumentStatement :: ParsecT [Token] MemoryList IO[Token]
singleArgumentStatement = do
  tT <- typeToken
  iT <- idToken

  return [tT, iT]

blockStatement :: ParsecT [Token] MemoryList IO[Token]
blockStatement = do
  lb <- leftBlockToken
  stmts <- statements <|> return []
  rb <- rightBlockToken

  return ([lb] ++ stmts ++ [rb])
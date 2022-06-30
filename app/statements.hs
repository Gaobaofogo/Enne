{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Statement where

import Lexer
import Token
import Text.Parsec
import Expression
import System.IO
import System.IO.Unsafe


statements :: ParsecT [Token] [(Token,Token)] IO [Token]
statements = do
        first <- attribution <|> ifStatement
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
  e <- semiColonToken
  return [a, b, c, d, e]

ifStatement :: ParsecT [Token] [(Token,Token)] IO[Token]
ifStatement = do
  ifT <- ifToken
  lp <- leftParentesisToken
  -- Precisa colocar aqui pra ler uma expressão booleana
  rp <- rightParentesisToken
  lb <- leftBlockToken
  stmts <- statements <|> return []
  rb <- rightBlockToken
  eS <- elseStatement <|> return []

  return ([ifT, lp, rp, lb] ++ stmts ++ [rb] ++ eS)

elseStatement :: ParsecT [Token] [(Token,Token)] IO[Token]
elseStatement = do
  eT <- elseToken
  lb <- leftBlockToken
  stmts <- statements <|> return []
  rb <- rightBlockToken

  return ([eT, lb] ++ stmts ++ [rb])
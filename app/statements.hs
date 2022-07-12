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
  first <- attributionSemiColon <|> ifStatement <|> whileStatement <|> funcStatement <|> forStatement <|> printStatement
  next  <- remaining_stmts
  return (first ++ next) <|> return []

remaining_stmts :: ParsecT [Token] MemoryList IO [Token]
remaining_stmts = (do statements) <|> return []

attributionSemiColon :: ParsecT [Token] MemoryList IO[Token]
attributionSemiColon = do
  aT <- attribution
  sC <- semiColonToken

  return $ aT ++[sC]

attribution :: ParsecT [Token] MemoryList IO[Token]
attribution = do attributionDeclaration <|> reattribution

attributionDeclaration :: ParsecT [Token] MemoryList IO[Token]
attributionDeclaration = do
  tT <- typeToken
  idT <- idToken
  aT <- assignToken
  e <- expression <|> readStatement

  actualState <- getState
  if areTypesCompatible (convertTypeToValue tT, e) then
    case symtable_insert (MemoryCell idT e) actualState of
      Left errorMsg -> fail errorMsg
      Right newState -> updateState (const newState)
  else fail "Tipos não são compatíveis"

  s <- getState
  liftIO (print s)

  return [tT, idT, aT, e]

reattribution :: ParsecT [Token] MemoryList IO[Token]
reattribution = do
  idT <- idToken
  aT <- assignToken
  e <- expression <|> readStatement

  actualState <- getState
  let var = symtable_search idT actualState
  let cell_var = fst var
  let value_cell = get_value_cell cell_var
  if areTypesCompatible (value_cell, e) && snd var then
    updateState $ const $ symtable_update (MemoryCell idT e) actualState
  else fail "Tipos não são compatíveis"

  s <- getState
  liftIO (print s)

  return [idT, aT, e]

printStatement :: ParsecT [Token] MemoryList IO[Token]
printStatement = do
  pT <- printToken
  lp <- leftParentesisToken
  eX <- expression
  rp <- rightParentesisToken
  sT <- semiColonToken

  liftIO $ putStrLn $ get_data_from_token eX

  return [pT, lp, eX, rp, sT]

readStatement ::  ParsecT [Token] MemoryList IO(Token)
readStatement = do
  rT <- readToken
  lP <- leftParentesisToken
  tT <- typeToken
  rP <- rightParentesisToken

  input <- liftIO getLine
  let x = convertInputToType input tT

  return $ convertInputToType input tT
  --return [rT, lP, tT, rP, sT]

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
  le <- logicExpression
  rP <- rightParentesisToken
  bS <- blockStatement

  return ([wT, lP] ++ le ++ [rP] ++ bS)

forStatement :: ParsecT [Token] MemoryList IO[Token]
forStatement = do
  fT <- forToken
  lP <- leftParentesisToken
  iS <- attribution
  iST <- semiColonToken
  le <- logicExpression
  sST <- semiColonToken
  sS <- attribution
  rP <- rightParentesisToken
  bS <- blockStatement

  return ([fT, lP] ++ iS ++ [iST] ++ le ++ [sST] ++ sS ++ [rP] ++ bS)

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
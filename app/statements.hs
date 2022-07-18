{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Statement where


import Lexer
import Token
import Memory
import Matrix
import Text.Parsec
import Expression
import System.IO
import System.IO.Unsafe
import Control.Monad.IO.Class
import Control.Monad

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
attribution = try attributionDeclaration <|> try reattribution  <|> arrayAttribution <|> arrayDeclaration

attributionDeclaration :: ParsecT [Token] MemoryList IO[Token]
attributionDeclaration = do
  tT <- typeToken
  idT <- idToken
  aT <- assignToken
  e <- expression <|> readStatement

  actualState <- getState
  Control.Monad.when (canOperate actualState) $
    if areTypesCompatible (convertTypeToValue tT, e) then
      case symtable_insert (MemoryCell idT e) actualState of
        Left errorMsg -> fail errorMsg
        Right newState -> updateState (const newState)
    else fail "Tipos não são compatíveis"

  s <- getState

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

  Control.Monad.when (canOperate actualState) $
    if areTypesCompatible (value_cell, e) && snd var then
      updateState $ const $ symtable_update (MemoryCell idT e) actualState
    else fail "Tipos não são compatíveis"

  s <- getState
  -- liftIO (print s)

  return [idT, aT, e]

arrayDeclaration :: ParsecT [Token] MemoryList IO[Token]
arrayDeclaration = do
  tT <- typeToken
  idT <- idToken
  bS <- bracketSequence
  aT <- assignToken <|> return Null
  e <- expression <|> return Null

  -- Estou assumindo que a expressão é uma matrix tbm. COmo eu faço?????
  actualState <- getState
  let newArray = asdf e idT tT $ fst bS
  case symtable_insert newArray actualState of
    Left errorMsg -> fail errorMsg
    Right newState -> updateState (const newState)
  
  s <- getState
  liftIO (print s)

  return $ [tT, idT] ++ snd bS

asdf :: Token -> Token -> Token -> [Token] -> MemoryCell
asdf Null idT tT bS             = declareMemoryArray idT tT $ tokensToInts bS
asdf (Matrix t dim arr) idT _ _ = MemoryArray idT t dim arr

arrayAttribution ::ParsecT [Token] MemoryList IO[Token]
arrayAttribution = do
  idT <- idToken
  bS <- bracketSequence
  aT <- assignToken
  e <- expression <|> readStatement

  -- TODO: Fazer a verificação se o array tá dentro do alcance do número
  s <- getState
  let arrayFound = symtable_search idT s
  if snd arrayFound then
    updateState $ symtable_update $ update_array_at_index (fst arrayFound) (fst bS) e
  else fail "Array não existe"

  return $ [idT] ++ snd bS ++ [aT, e]

printStatement :: ParsecT [Token] MemoryList IO[Token]
printStatement = do
  pT <- printToken
  lp <- leftParentesisToken
  eX <- expression
  rp <- rightParentesisToken
  sT <- semiColonToken

  s <- getState
  if canOperate s then
    liftIO $ putStrLn $ get_data_from_token eX
  else updateState (symtableUpdateFlag 0)

  return [pT, lp, eX, rp, sT]

readStatement ::  ParsecT [Token] MemoryList IO(Token)
readStatement = do
  rT <- readToken
  lP <- leftParentesisToken
  tT <- typeToken
  rP <- rightParentesisToken

  actualState <- getState
  if canOperate actualState then
    do
      input <- liftIO getLine
      return $ convertInputToType input tT
  else return $ convertTypeToValue tT
  
  -- return [rT, lP, tT, rP]

ifStatement :: ParsecT [Token] MemoryList IO[Token]
ifStatement = do
  ifT <- ifToken
  lp <- leftParentesisToken
  le <- logicExpression
  rp <- rightParentesisToken


  s1 <- getState
  let flag = head s1

  if canOperate s1 && tokenToBool (le!!0)
    then updateState ( symtableUpdateFlag 1 )
  else updateState ( symtableUpdateFlag 0)

  bS <- blockStatement
  eS <- elseStatement <|> return []

  updateState $ symtableUpdateFlag $ get_int_from_token $ get_value_cell flag

  return ([ifT, lp] ++ le ++ [rp] ++ bS ++ eS)

elseStatement :: ParsecT [Token] MemoryList IO[Token]
elseStatement = do
  s1 <- getState
  if canOperate s1
    then updateState ( symtableUpdateFlag 0 )
  else updateState ( symtableUpdateFlag 1)
  eT <- elseToken
  bS <- blockStatement
  if canOperate s1
    then updateState ( symtableUpdateFlag 1 )
  else updateState ( symtableUpdateFlag 0)

  return (eT : bS)

whileStatement :: ParsecT [Token] MemoryList IO[Token]
whileStatement = do
  z <- getInput
  wT <- whileToken
  lP <- leftParentesisToken
  le <- logicExpression
  rP <- rightParentesisToken

  s1 <- getState
  if tokenToBool (head le)
    then updateState ( symtableUpdateFlag 1 )
  else updateState ( symtableUpdateFlag 0)

  bS <- blockStatement

  y <- getState
  if canOperate y then
    do
      setInput z
      aaaaaa <- whileStatement
      return ([wT, lP] ++ le ++ [rP] ++ bS)
  else
    do
      updateState $ symtableUpdateFlag 1
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
  s1 <- getState
  -- if canOperate s1
  --   then updateState ( symtableUpdateFlag 1 )
  -- else updateState ( symtableUpdateFlag 1 )

  return ([lb] ++ stmts ++ [rb])
module Token where

import Lexer
import Text.Parsec
import Data.Functor.Identity

-- general statements programing
-- update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- needs improvement
update_pos pos _ []      = pos

idToken :: ParsecT [Token] st IO Token
idToken = tokenPrim show update_pos get_token where
    get_token (Id x) = Just (Id x)
    get_token _        = Nothing

-- language types
-- floatToken :: ParsecT [Token] st Data.Functor.Identity.Identity Token
floatToken :: ParsecT [Token] st IO (Token)
floatToken = tokenPrim show update_pos get_token where
    get_token (Float x) = Just (Float x)
    get_token _         = Nothing 

intToken :: ParsecT [Token] st IO (Token)
intToken = tokenPrim show update_pos get_token where
    get_token (Int x) = Just (Int x)
    get_token _       = Nothing

-- boolToken :: ParsecT [Token] u IO Token
-- boolToken = tokenPrim show update_pos get_token where
--     get_token (Bool x) = Just (Bool x)
--     get_token _           = Nothing

addToken :: ParsecT [Token] u IO Token
addToken = tokenPrim show update_pos get_token where
    get_token (Add) = Just (Add)
    get_token _      = Nothing

subToken :: ParsecT [Token] u IO Token
subToken = tokenPrim show update_pos get_token where
    get_token (Sub) = Just (Sub)
    get_token _      = Nothing

multToken :: ParsecT [Token] u IO Token
multToken = tokenPrim show update_pos get_token where
    get_token (Mult) = Just (Mult)
    get_token _      = Nothing

stringToken :: ParsecT [Token] u IO Token
stringToken = tokenPrim show update_pos get_token where
    get_token (String x) = Just (String x)
    get_token _        = Nothing

typeToken :: ParsecT [Token] u IO Token
typeToken = tokenPrim show update_pos get_token where
    get_token (Type x) = Just (Type x)
    get_token _        = Nothing

semiColonToken :: ParsecT [Token] u IO Token
semiColonToken = tokenPrim show update_pos get_token where
    get_token (SemiColon) = Just (SemiColon)
    get_token _        = Nothing

assignToken :: ParsecT [Token] u IO Token
assignToken = tokenPrim show update_pos get_token where
    get_token (Assign) = Just (Assign)
    get_token _        = Nothing
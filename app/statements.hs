module Statement where

import Lexer
import Token
import Text.Parsec
import Expression
import System.IO
import System.IO.Unsafe


stmts :: ParsecT [Token] [Type] IO [Token]
stmts = do
        first <- assign
        next  <- remaining_stmts
        return (first ++ next) <|> (return [])

remaining_stmts :: ParsecT [Token] [Type] IO [Token]
remaining_stmts = (do a <- stmts
                      return a) <|> (return [])
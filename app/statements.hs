module Statement where

import Lexer
import Token
import Text.Parsec
import Expression
import System.IO
import System.IO.Unsafe


statements :: ParsecT [Token] [(Token,Token)] IO [Token]
statements = do
        first <- attribution
        next  <- remaining_stmts
        return (first ++ next) <|> (return [])

remaining_stmts :: ParsecT [Token] [(Token,Token)] IO [Token]
remaining_stmts = (do a <- statements
                      return a) <|> (return [])
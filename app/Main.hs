module Main where

import Lexer
import Expression
import Text.Parsec
import Control.Monad.IO.Class

import System.IO.Unsafe
import Text.Parsec (ParseError)

program :: ParsecT [Token] [(Token,Token)] IO ([Token])
program = do
    b <- attribution
    eof
    return (b++[])

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program [] "Error message" tokens

main :: IO ()
main = case unsafePerformIO (parser (getTokens "../programa.enne")) of
            { Left err -> print err; 
              Right ans -> print ans
            }

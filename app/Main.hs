module Main where

import Lexer
import Expression
import Statement
import Text.Parsec
import Control.Monad.IO.Class
import System.Environment

import System.IO.Unsafe
import Text.Parsec (ParseError)
import System.Directory.Internal.Prelude (getArgs)

program :: ParsecT [Token] [(Token,Token)] IO ([Token])
program = do
    a <- statements
    eof
    return (a++[])

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program [] "Error message" tokens

main :: IO ()
main =
  do {
    file <- getArgs;
    case unsafePerformIO (parser (getTokens (head file))) of
              { Left err -> print err;
                Right ans -> print ans
              }
  }

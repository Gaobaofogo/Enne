{
module Lexer where

import System.IO
import System.IO.Unsafe
}

%wrapper "basic"

$digit = 0-9      -- digits
$alpha = [a-zA-Z] -- alphabetic characters
$assignment = \=
$aritmetic_operators = [\+\-]

-- literal types
@types = int
       | float
       | string
       | bool
@float_number = $digit+ \. $digit+

tokens :-

  $white+                              ;
  "--".*                               ;
  :                                    { \s -> Colon}
  ";"                                  { \s -> SemiColon}
  @types                               { \s -> Type s}
  $assignment                          { \s -> Assign}
  if                                   { \s -> If}
  then                                 { \s -> Then}
  write                                { \s -> Write}
  >                                    { \s -> Greater}
  $aritmetic_operators                 { \s -> AritmeticOperator s}
  $digit+                              { \s -> Int (read s) }
  @float_number                        { \s -> Float (read s)}
  $alpha [$alpha $digit \_ \']*        { \s -> Id s }
  \" $alpha [$alpha $digit ! \_ \']* \"  { \s -> String s}

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
  Colon   |
  SemiColon |
  Assign    | 
  If  |
  Then |
  Write |
  Greater |
  Type String |
  Id String |
  Int Int |
  Float Double |
  String String |
  AritmeticOperator String
  deriving (Eq,Show)

getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do {fh <- openFile fn ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}
}
{
module Lexer where

import System.IO
import System.IO.Unsafe
}

%wrapper "basic"

$digit = 0-9      -- digits
$alpha = [a-zA-Z] -- alphabetic characters
$assignment = \=
$aritmetic_operators = [\+\%\-\*\/]
$parentesis = [\(\)]
$block = [\{\}]

-- literal types
@types = int
       | float
       | string
       | bool
@float_number = $digit+ \. $digit+

tokens :-

  $white+                                ;
  "--".*                                 ;
  :                                      { \s -> Colon}
  ";"                                    { \s -> SemiColon}
  ","                                    { \s -> Comma}
  @types                                 { \s -> Type s}
  func                                   { \s -> Function}
  $assignment                            { \s -> Assign}
  $parentesis                            { \s -> Parentesis s }
  $block                                 { \s -> Block s }
  if                                     { \s -> If}
  else                                   { \s -> Else}
  for                                    { \s -> For}
  while                                  { \s -> While}
  \>                                     { \s -> Greater}
  \<                                     { \s -> Lower}
  \=\=                                   { \s -> EqualTo}
  $aritmetic_operators                   { \s -> AritmeticOperator s}
  $digit+                                { \s -> Int (read s) }
  @float_number                          { \s -> Float (read s)}
  $alpha [$alpha $digit \_ \']*          { \s -> Id s }
  \" $alpha [$alpha $digit ! \_ \']* \"  { \s -> String s}

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
  Colon   |
  SemiColon |
  Comma |
  Assign    | 
  If  |
  Else |
  For |
  While |
  Greater |
  Lower |
  EqualTo |
  Block String |
  Parentesis String |
  Type String |
  Function |
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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Memory where
import Lexer



symtable_insert :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_insert symbol []  = [symbol]
symtable_insert symbol symtable = symtable ++ [symbol]

symtable_search :: Token -> [(Token,Token)] -> [(Token,Token)]
symtable_search symbol [] = fail "Variable not found"
symtable_search (Id id1) ((Id id2, v2):t) =
    if id1 == id2 then [(Id id2, v2)] else symtable_search (Id id1) t

symtable_update :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_update _ [] = fail "variable not found"
symtable_update (Id id1, v1) ((Id id2, v2):t) =
                               if id1 == id2 then (Id id1, v1) : t
                               else (Id id2, v2) : symtable_update (Id id1, v1) t

symtable_remove :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_remove _ [] = fail "variable not found"
symtable_remove (Id id1, v1) ((Id id2, v2):t) =
                               if id1 == id2 then t
                               else (Id id2, v2) : symtable_remove (Id id1, v1) t

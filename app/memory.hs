{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Memory where
import Lexer

{-
https://github.com/Gaobaofogo/Enne/issues/2
(name, value)

Example: (Id "global.name", String "baleia")
-}
data MemoryCell = MemoryCell Token Token
    deriving (Show)
type MemoryList = [MemoryCell]

get_name_cell :: MemoryCell -> String
get_name_cell (MemoryCell (Id x) _) = x

get_value_cell :: MemoryCell -> Token
get_value_cell (MemoryCell _ value) = value

symtable_insert :: MemoryCell -> MemoryList -> Either String MemoryList
symtable_insert symbol []  = Right [symbol]
symtable_insert symbol symtable = if found then Left "Variável já existe" else Right (symtable ++ [symbol]) where
    found = snd $ symtable_search (Id (get_name_cell symbol)) symtable

symtable_search :: Token -> MemoryList -> (MemoryCell, Bool)
symtable_search symbol [] = (MemoryCell (Id "global.erro") (String "Erro"), False)
symtable_search (Id id1) (memory_cell:t) =
    if id1 == id2 then (memory_cell,True) else symtable_search (Id id1) t where
        id2 = get_name_cell memory_cell

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

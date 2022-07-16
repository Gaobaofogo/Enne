{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Memory where
import Lexer
import Token

{-
https://github.com/Gaobaofogo/Enne/issues/2
(name, value)

Example: (Id "global.name", String "baleia")
-}

data MemoryCell = MemoryCell Token Token
                | MemoryArray Token Token [Int] [Token]
                deriving (Show)
type MemoryList = [MemoryCell]

declareMemoryArray :: Token -> Token -> [Int] -> MemoryCell
declareMemoryArray id typeArray dimensions = MemoryArray id typeArray dimensions (dataArray typeArray) where
    dataArray (Type "int")    = replicate (product dimensions) (Int 0)
    dataArray (Type "float")  = replicate (product dimensions) (Float 0.0)
    dataArray (Type "string") = replicate (product dimensions) (String "")

get_name_cell :: MemoryCell -> String
get_name_cell (MemoryCell  (Id x) _)    = x
get_name_cell (MemoryArray (Id x) _ _ _) = x

get_value_cell :: MemoryCell -> Token
get_value_cell (MemoryCell _ value) = value

get_id_array :: MemoryCell -> Token
get_id_array (MemoryArray id _ _ _) = id

get_type_array :: MemoryCell -> Token
get_type_array (MemoryArray _ typeArr _ _) = typeArr

get_dimensions_array :: MemoryCell -> [Int]
get_dimensions_array (MemoryArray _ _ dimensions _) = dimensions

get_data_array :: MemoryCell -> [Token]
get_data_array (MemoryArray _ _ _ data_array) = data_array

update_array_at_index :: MemoryCell -> [Token] -> Token -> MemoryCell
update_array_at_index arrayFound bS e = MemoryArray (get_id_array arrayFound) (get_type_array arrayFound) (get_dimensions_array arrayFound) newArray where
  index    = arrayIndex (get_dimensions_array arrayFound) (tokensToInts bS)
  newArray = arrayReplace (get_data_array arrayFound) index e
  

-- Obs.: O tamanho das duas listas deve ser igual
arrayIndex :: [Int] -> [Int] ->  Int
arrayIndex [] [] = 0
arrayIndex (d:dimensions) (s:selectedBrackets) = (s * product dimensions) + arrayIndex dimensions selectedBrackets

-- A função abaixo não verifica se a posição está dentro do tamanho do array
arrayReplace :: [a] -> Int -> a -> [a]
arrayReplace (x:xs) 0 y   = y : xs
arrayReplace (x:xs) pos y = x : arrayReplace xs (pos - 1) y

symtable_insert :: MemoryCell -> MemoryList -> Either String MemoryList
symtable_insert symbol []  = Right [symbol]
symtable_insert symbol symtable = if found then Left "Variável já existe" else Right (symtable ++ [symbol]) where
    found = snd $ symtable_search (Id (get_name_cell symbol)) symtable

symtable_search :: Token -> MemoryList -> (MemoryCell, Bool)
symtable_search symbol [] = (MemoryCell (Id "global.erro") (String "Erro"), False)
symtable_search (Id id1) (memory_cell:t) =
    if id1 == id2 then (memory_cell,True) else symtable_search (Id id1) t where
        id2 = get_name_cell memory_cell

symtable_update :: MemoryCell -> MemoryList -> MemoryList
symtable_update _ [] = fail "variable not found"
symtable_update (MemoryCell (Id id1) v1) ((MemoryCell (Id id2) v2):t) =
                                if id1 == id2 then (MemoryCell (Id id1) v1) : t
                                else (MemoryCell (Id id2) v2) : symtable_update (MemoryCell (Id id1) v1) t
symtable_update (MemoryArray (Id id1) t1 d1 arr1) ((MemoryArray (Id id2) t2 d2 arr2):t) =
                                if id1 == id2 then (MemoryArray (Id id1) t1 d1 arr1) : t
                                else (MemoryArray (Id id2) t2 d2 arr2) : symtable_update (MemoryArray (Id id1) t1 d1 arr1) t
symtable_update x (y:ys) = y : symtable_update x ys


symtable_remove :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_remove _ [] = fail "variable not found"
symtable_remove (Id id1, v1) ((Id id2, v2):t) =
                               if id1 == id2 then t
                               else (Id id2, v2) : symtable_remove (Id id1, v1) t

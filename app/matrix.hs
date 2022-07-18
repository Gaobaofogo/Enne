{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Matrix where

import Lexer
import Memory
import Token
import GHC.Float (int2Double)


array_sum :: [Token] -> [Token] -> [Token]
array_sum [] []                         = []
array_sum ((Int x):xs) ((Int y):ys)     = Int (x + y)   : array_sum xs ys
array_sum ((Float x):xs) ((Float y):ys) = Float (x + y) : array_sum xs ys
array_sum ((Int x):xs) ((Float y):ys)   = Float (int2Double x + y) : array_sum xs ys
array_sum ((Float x):xs) ((Int y):ys)   = Float (x + int2Double y) : array_sum xs ys

-- array_diff :: MemoryCell -> MemoryCell -> [Token]
-- array_diff (MemoryArray _ _ _ arr1) (MemoryArray _ _ _ arr2) = array_diff_aux arr1 arr2 where
--   array_diff_aux [] []         = []
--   array_diff_aux (x:xs) (y:ys) = x - y : array_diff_aux xs ys

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

-- Verifica se a matriz é vazia. Caso seja, preenche a matriz com valores padroes.
isArrayEmpty :: Token -> Token -> Token -> [Token] -> MemoryCell
isArrayEmpty Null idT tT bS             = declareMemoryArray idT tT $ tokensToInts bS
isArrayEmpty (Matrix t dim arr) idT _ _ = MemoryArray idT t dim arr
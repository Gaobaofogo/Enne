import Data.Matrix

-- Falta colocar um valor para receber um Matrix
data BuiltinTypes = BTI Integer
    | BTC Char
    | BTS String
    | BTF Double
    | BTB Bool
    | BTA [BuiltinTypes] -- Array
    | BTM (Matrix BuiltinTypes) -- Array
    deriving Show
-- Módulo responsável por gerenciar matrizes
module Matrix where

import Data.List

type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Int

-- Retorna as linhas de uma matriz
rows :: Matrix a -> [Row a]
rows m = m

-- Retorna cada coluna como uma lista de linhas (transposição)
cols :: Matrix a -> [Row a]
cols m = transpose m
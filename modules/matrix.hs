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

-- retorna os blocos em forma de linhas
blocks :: Eq a => (Matrix a, Matrix a) -> [Row a]
blocks (vals, pos) = [filterByGroup group tupleValueGroup | group <- groups]
  where
    tupleValueGroup = foldl1 (++) $ zipWith zip vals pos
    groups = nub $ map snd tupleValueGroup
    filterByGroup group list = map fst $ filter ((== group) . snd) list


-- -- retorna os blocos em forma de matrizes
-- blocksAsMatrix :: (Matrix a, Matrix a) -> [Matrix a]




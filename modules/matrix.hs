-- Módulo responsável por gerenciar matrizes
module Matrix where

import Data.List

type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Int

size :: Matrix a -> Int
size m = length (m!!0)

-- Retorna as linhas de uma matriz
rows :: Matrix a -> [Row a]
rows m = m

-- Retorna cada coluna como uma lista de linhas (transposição)
cols :: Matrix a -> [Row a]
cols m = transpose m

-- retorna os blocos em forma de linhas
blocks :: Eq a => Matrix a -> Grid -> [Row a]
blocks vals pos = [filterByGroup group tupleValueGroup | group <- groups]
  where
    tupleValueGroup = foldl1 (++) (zipWith zip vals pos)
    groups = nub (map snd tupleValueGroup)
    filterByGroup group list = map fst $ filter ((== group) . snd) list

valsOfBlock :: Eq a => Matrix a -> Grid -> Int -> Row a
valsOfBlock vals pos id = map fst $ filter ((==id) . snd) tupleValueGroup 
  where
    tupleValueGroup = foldl1 (++) (zipWith zip vals pos)
    

blocksByCols :: Eq a => Matrix a -> Grid -> [Row a]
blocksByCols vals pos = zip (cols vals) (cols pos) >>= (\(v, p) -> blocks [v] [p])

lengthOfBlock :: Eq a => a -> Matrix a -> Int
lengthOfBlock _ [] = 0
lengthOfBlock id pos = sum [count id p | p <- pos]
  where count x xs = length (filter (==x) xs)

colsOfBlocksByCols :: [Row a] -> Int -> [Row a]
colsOfBlocksByCols bs n = chunksOf n (concat bs)

chunksOf :: Int -> [a] -> [Row a]
chunksOf n = takeWhile (not . null) . map (take n) . iterate (drop n)
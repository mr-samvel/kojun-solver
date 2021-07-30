module Main where

import Reader
import Matrix
import Data.List
-----------------------------------------------------------------------------
type Choices = [Value]

-- ---------------------- solucao 2
-- -- expanding choices one square at
-- -- a time, and filtering out any resulting matrices that are blocked
-- -- before considering any further choices.
-- -- solve :: Grid -> [Grid]
-- solve :: Grid -> Grid -> [Grid]
-- solve vals pos = search (prune (choices vals pos) pos) pos


-- -- primeira solução - INEFICIENTE
-- -- cria uma matriz com todas as escolhas possiveis,
-- -- aplica prune n vezes, até nao ser mais possível reduzir o numero de escolhas pra cada celula
-- -- extrai todas as matrizes pra cada escolha
-- -- filtra por essas matrizes até achar uma válida
solve :: Grid -> Grid -> [Grid]
solve vals pos = filter (`valid` pos) (collapse (fix (`prune` pos) (choices vals pos)))

valid :: Grid -> Grid -> Bool
valid vals pos = all validNeighborhood (cols vals) &&
        all validNeighborhood (rows vals) &&
        all nodups (blocks vals pos) &&
        all isDecreasing (blocksByCols vals pos)

-- itera a função (a -> a) no parametro de entrada até a saída ser igual a ele
fix :: Eq a => (a -> a) -> a -> a
fix f x = if x == x' then x else fix f x'
    where x' = f x

-- preenche cada celula sem valor com uma lista de 1..9
choices :: Grid -> Grid -> Matrix Choices
choices vals pos = map (map choice) (zipWith zip vals pos)
    where choice (v, p) = if v == 0 then [1..(lengthOfBlock p pos)] `minus` (valsOfBlock vals pos p) else [v]

-- de uma matriz, elimina das escolhas os valores que já foram utilizados no bloco, coluna e linha
prune :: Matrix Choices -> Grid -> Matrix Choices
prune vals pos = cols $ colsOfBlocksByCols (map reduce (blocksByCols vals pos)) (size vals)

-- de uma linha, reduz as escolhas com base em elementos unitários
-- ex: ["1 2 3 4", "1", "3 4", "3"] -> ["2 4", "1", "4", "3"]
reduce :: Row Choices -> Row Choices
reduce xss = [xs `minus` singles | xs <- xss]
    where singles = concat (filter single xss)

minus :: Choices -> Choices -> Choices
xs `minus` ys = if single xs then xs else xs \\ ys

--retorna true se a lista passada só possui um elemento
single :: [a] -> Bool
single [_] = True
single _ = False

-- search :: Matrix Choices -> Grid -> [Grid]
-- search m pos
--     | blocked m pos = []
--     | all (all single) m = collapse m
--     | otherwise = [g | m' <- expand m, g <- search (prune m' pos) pos]

-- matrizes bloqueadas nunca podem fornecer uma solução
blocked :: Matrix Choices -> Grid -> Bool
blocked m pos = void m || not (safe m pos)

-- verifica se ha alguma celula vazia na matriz
void :: Matrix Choices -> Bool
void m = any (any null) m

-- verifica se todos os campos sao consistentes (nao possuem valores unitarios duplicados)
safe :: Matrix Choices -> Grid -> Bool
safe m pos = all (check validNeighborhood) (cols m) &&
        all (check validNeighborhood) (rows m) &&
        all (check nodups) (blocks m pos) &&
        all (check isDecreasing) (blocksByCols m pos)
            where check f = f . concat . filter single

-- verifica se nao ha valores vizinhos iguais
validNeighborhood :: Eq a => [a] -> Bool
validNeighborhood [] = True
validNeighborhood [a] = True
validNeighborhood (a:b:bs) = if a == b then False else validNeighborhood bs

-- verifica se não há valores unitarios duplicados na linha passada
nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x:xs) = not (elem x xs) && nodups xs

-- verifica se nao ha valores vizinhos iguais
isDecreasing :: Ord a => [a] -> Bool
isDecreasing [] = True
isDecreasing [a] = True
isDecreasing (a:b:bs) = if a < b then False else isDecreasing bs

-- retorna uma lista contendo todas as matrizes de solução possíveis
collapse :: Matrix [a] -> [Matrix a]
collapse m = cp (map cp m)

-- retorna o produto cartesiano das listas de uma lista
-- ex: [[1, 2], [3, 4]] -> [[1, 3], [1, 4], [2, 3], [2, 4]]
cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [y:ys | y <- xs, ys <- cp xss]

--The function expand behaves in the same way as collapse, except that
--it only collapses the first square with more than one choice:
expand :: Matrix Choices -> [Matrix Choices]
expand m = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
    where
        (rows1,row:rows2) = break (any (not . single)) m
        (row1,cs:row2)    = break (not . single) row

------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- validacao kojun:
-- verificar se elemento é igual ao proximo elemento na linha e coluna
-- verificar se há duplicados em um bloco
-- verificar se bloco respeita regra de verticalidade
------------------------------------------------------------------------------

main = do
    (vals, pos) <- readPuzzle "tabuleiro6x6.txt"
    print $ solve vals pos
    

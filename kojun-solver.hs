module Main where

import Reader
import Matrix
import Data.List
-----------------------------------------------------------------------------
type Choices = [Value]

-- ---------------------- solucao 2
-- -- expanding choices one square at
-- -- a time, and filtering out any resulting matrices that are blocked
-- -- before considering any further choices
solve :: Grid -> Grid -> [Grid]
solve vals pos = search (prune (choices vals pos) pos) pos

-- -- primeira solução - INEFICIENTE
-- -- cria uma matriz com todas as escolhas possiveis,
-- -- aplica prune n vezes, até nao ser mais possível reduzir o numero de escolhas pra cada celula
-- -- extrai todas as matrizes pra cada escolha
-- -- filtra por essas matrizes até achar uma válida
-- solve :: Grid -> Grid -> [Grid]
-- solve vals pos = filter (`valid` pos) (collapse (fix (`prune` pos) (choices vals pos)))

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

search :: Matrix Choices -> Grid -> [Grid]
search m pos
    | blocked m pos = []
    | all (all single) m = collapse m
    | otherwise = [g | m' <- expand m, g <- search (prune m' pos) pos]

-- matrizes bloqueadas nunca podem fornecer uma solução
blocked :: Matrix Choices -> Grid -> Bool
blocked m pos = void m || not (safe m pos)

-- verifica se ha alguma celula vazia na matriz
void :: Matrix Choices -> Bool
void m = any (any null) m

-- verifica se todos os campos sao consistentes (nao possuem valores unitarios duplicados)
safe :: Matrix Choices -> Grid -> Bool
safe m pos = all (validNeighborhood) (cols m) &&
        all (validNeighborhood) (rows m) &&
        all (nodups) (blocks m pos) &&
        all (isDecreasing) (blocksByCols m pos)

-- verifica se nao ha valores vizinhos iguais
validNeighborhood :: Eq a => Row [a] -> Bool
validNeighborhood [] = True
validNeighborhood [a] = True
validNeighborhood (a:b:bs) 
    | (length a <= 1) && (length b <= 1) = if a == b then False else validNeighborhood (b:bs)
    | otherwise = validNeighborhood (b:bs)

-- verifica se não há valores unitarios duplicados na linha passada
nodups :: Eq a => Row [a] -> Bool
nodups [] = True
nodups (x:xs) = if (length x <= 1) then not (elem x xs) && nodups xs else nodups xs

-- verifica se nao ha valores vizinhos iguais
isDecreasing :: Ord a => Row [a] -> Bool
isDecreasing [] = True
isDecreasing [a] = True
isDecreasing (a:b:bs) 
    | (length a <= 1) && (length b <= 1) = if a < b then False else isDecreasing bs
    | otherwise = isDecreasing bs

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
    board <- getLine
    (vals, pos) <- readPuzzle board
    --solve vals pos = search (prune (choices vals pos) pos) pos
    -- solve vals pos = filter (`valid` pos) (collapse (fix (`prune` pos) (choices vals pos)))
    -- print $ choices vals pos
    -- print $ lengthOfBlock 2 pos
    -- print $ valsOfBlock vals pos 2
    -- `minus` (valsOfBlock vals pos p)
    --print $ prune (choices vals pos) pos
    --print $ expand (prune (choices vals pos) pos)
    --print $ blocked (prune (choices vals pos) pos) pos
    --print $ search (prune (choices vals pos) pos) pos
    -- let a = [[[2],[1,3],[2]],[[1],[2],[1,3]],[[4],[1,3],[1,3]]]
    -- print $ safe a pos
    -- print $ map (typeOf) a
    -- print $ [g | m' <- expand (prune (choices vals pos) pos), g <- search (prune m' pos) pos]
    -- let a = [m | m <- expand (prune (choices vals pos) pos)]
    -- print $ map (`blocked` pos) a
    -- let a = [[[2],[5],[1],[4],[3],[4],[1],[2]],[[1],[3],[6],[2],[6],[3],[2],[1]],[[3],[1],[5],[1],[5],[2],[3],[5]],[[2],[3],[4],[2],[3],[1],[2],[1]],[[1],[2],[1],[3],[4],[2],[4],[5]],[[3],[1],[2],[1],[5],[4],[1],[3]],[[1],[5],[1],[6],[3],[2],[5],[4]],[[2],[4],[2],[1],[4],[1],[3],[2]]]
    print $ solve vals pos
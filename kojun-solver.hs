module Main where

import Reader
import Matrix

-----------------------------------------------------------------------------
-- EXEMPLO DE RESOLUÇÃO PARA SUDOKU

-- type Choices = [Value]

-- -- primeira solução - INEFICIENTE
-- -- cria uma matriz com todas as escolhas possiveis,
-- -- aplica prune n vezes, até nao ser mais possível reduzir o numero de escolhas pra cada celula
-- -- extrai todas as matrizes pra cada escolha
-- -- filtra por essas matrizes até achar uma válida
-- solve :: Grid -> [Grid]
-- solve = filter valid . collapse . fix prune . choices

-- valid :: Grid -> Bool
-- valid g = all noDuplicates (rows g) &&
--           all noDuplicates (cols g) &&
--           all noDuplicates (boxes g)

-- noDuplicates :: Eq a => [a] -> Bool
-- noDuplicates [] = True
-- noDuplicates (x:xs) = not (elem x xs) && noDuplicates xs

-- -- preenche cada celula sem valor com uma lista de 1..9
-- choices :: Grid -> Matrix Choices
-- choices choices g = map (map choice) g
--     where choice v = if v == 0 then [1..9] else [v]

-- -- retorna uma lista contendo todas as matrizes de solução possíveis
-- collapse :: Matrix [a] -> [Matrix a]
-- collapse m = cp (map cp m)

-- -- retorna o produto cartesiano das listas de uma lista
-- -- ex: [[1, 2], [3, 4]] -> [[1, 3], [1, 4], [2, 3], [2, 4]]
-- cp :: [[a]] -> [[a]]
-- cp [] = [[]]
-- cp (xs:xss) = [y:ys | y <- xs, ys <- cp xss]

-- -- de uma matriz, elimina das escolhas os valores que já foram utilizados no bloco, coluna e linha
-- prune :: Matrix Choices -> Matrix Choices
-- prune = pruneBy boxes . pruneBy cols . pruneBy rows
--     where pruneBy f = f . map reduce . f

-- -- de uma linha, reduz as escolhas com base em elementos unitários
-- -- ex: ["1 2 3 4", "1", "3 4", "3"] -> ["2 4", "1", "4", "3"]
-- reduce :: Row Choices -> Row Choices
-- reduce xss = [xs `minus` singles | xs <- xss]
--     where singles = concat (filter single xss)

-- minus :: Choices -> Choices -> Choices
-- xs `minus` ys = if single xs then xs else xs \\ ys

-- -- itera a função (a -> a) no parametro de entrada até a saída ser igual a ele
-- fix :: Eq a => (a -> a) -> a -> a
-- fix f x = if x == x' then x else fix f x'
--     where x' = f x

-- ---------------------- solucao 2
-- -- expanding choices one square at
-- -- a time, and filtering out any resulting matrices that are blocked
-- -- before considering any further choices.
-- solve2 :: Grid -> [Grid]
-- solve = search . prune . choices

-- -- verifica se ha alguma celula vazia na matriz
-- void :: Matrix Choices -> Bool
-- void m = any (any null) m

-- -- verifica se todos os campos sao consistentes (nao possuem valores unitarios duplicados)
-- safe :: Matrix Choices -> Bool
-- safe m = all consistent (rows m) &&
--          all consistent (cols m) &&
--          all consistent (boxes m)

-- -- verifica se não há valores unitarios duplicados na linha passada
-- consistent :: Row Choices -> Bool
-- consistent = nodups . concat . filter single

-- -- matrizes bloqueadas nunca podem fornecer uma solução
-- blocked :: Matrix Choices -> Bool
-- blockes m = void m || not (safe m)

-- search :: Matrix Choices -> [Grid]
-- search m
--     | blocked m = []
--     | all (all single) m = collapse m
--     | otherwise = [g | m' <- expand m, g <- search (prune m')]

-- --retorna true se a lista passada só possui um elemento
-- single :: [a] -> Bool
-- single [_] = true
-- single _ = false

-- --The function expand behaves in the same way as collapse, except that
-- --it only collapses the first square with more than one choice:
-- expand :: Matrix Choices -> [Matrix Choices]
-- expand m = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
--     where
--         (rows1,row:rows2) = break (any (not . single)) m
--         (row1,cs:row2)    = break (not . single) row

------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- validacao kojun:
-- verificar se elemento é igual ao proximo elemento na linha e coluna
-- verificar se há duplicados em um bloco
-- verificar se bloco respeita regra de verticalidade
------------------------------------------------------------------------------

main = do
    (tabuleiro, posicoes) <- readPuzzle "tabuleiro.txt"
    -- print tabuleiro
    -- print posicoes
    print $ blocks tabuleiro posicoes
    print $ colsByBlocks tabuleiro posicoes

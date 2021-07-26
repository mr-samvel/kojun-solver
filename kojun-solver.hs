-- Módulo principal
module Main where

-- Importa módulos auxiliares
import Reader
import Matrix

-----------------------------------------------------------------------------
-- EXEMPLO DE RESOLUÇÃO PARA SUDOKU

-- type Choices = [Value]

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

-- -- itera a função (a -> a) no parametro de entrada até a saída ser igual a ele
-- fix :: Eq a => (a -> a) -> a -> a
-- fix f x = if x == x' then x else fix f x'
--     where x' = f x


-- -- primeira solução - INEFICIENTE
-- -- cria uma matriz com todas as escolhas possiveis,
-- -- aplica prune n vezes, até nao ser mais possível reduzir o numero de escolhas pra cada celula
-- -- extrai todas as matrizes pra cada escolha
-- -- filtra por essas matrizes até achar uma válida
-- solve :: Grid -> [Grid]
-- solve = filter valid . collapse . fix prune . choices
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
    print $ blocks (tabuleiro, posicoes)

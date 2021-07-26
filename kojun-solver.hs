-- Módulo principal
module Main where

-- Importa módulos auxiliares
import Reader
import Matrix


-- -- ex de validacao de um sudoku
-- valid :: Grid -> Bool
-- valid g = all noDuplicates (rows g) &&
--           all noDuplicates (cols g) &&
--           all noDuplicates (boxes g)

noDuplicates :: Eq a => [a] -> Bool
noDuplicates [] = True
noDuplicates (x:xs) = not (elem x xs) && noDuplicates xs

main = do
    (tabuleiro, posicoes) <- readPuzzle "tabuleiro.txt"
    print tabuleiro
    print posicoes

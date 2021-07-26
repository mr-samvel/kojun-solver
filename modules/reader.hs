-- Modulo responsavel por ler e mapear o documento tabuleiro.txt
module Reader where

import Matrix

-- A partir da String passada, cria uma lista divindo-a onde a condição passada for verdadeira
-- exemplo: splitWhen (==",") "Minha,string" -> retorna ["Minha", "string"]
splitWhen :: (Char -> Bool) -> String -> [String]
splitWhen condition str = case dropWhile condition str of
                    "" -> []
                    str' -> splitted: splitWhen condition str''
                        where (splitted, str'') = break condition str'

readPuzzle :: FilePath -> IO (Grid, Grid)
readPuzzle p = do
    puz <- readFile p
    return (lineProcessing (lines puz))

-- processamento lento, pensar em melhoria!
lineProcessing :: [[Char]] -> (Grid, Grid)
lineProcessing xs = ([splitElementsOfBoardStr (words x) 0 | x <- xs], [splitElementsOfBoardStr (words x) 1 | x <- xs])

splitElementsOfBoardStr :: [String] -> Int -> [Int]
splitElementsOfBoardStr [] _ = []
splitElementsOfBoardStr (element : list) pos = [value] ++ splitElementsOfBoardStr list pos
    where value = read ((splitWhen (==',') element)!!pos) :: Int
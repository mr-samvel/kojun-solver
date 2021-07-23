-- Modulo responsavel por ler e mapear o documento tabuleiro.txt
module Reader where

-- A partir da String passada, cria uma lista divindo-a onde a condição passada for verdadeira
splitWhen :: (Char -> Bool) -> String -> [String]
splitWhen condition str = case dropWhile condition str of
                    "" -> []
                    str' -> splitted: splitWhen condition str''
                        where (splitted, str'') = break condition str'

-- Lê o arquivo indicado e retorna uma lista de strings separadas por quebra de linha
readBoardFile :: FilePath -> IO [String]
readBoardFile = fmap lines . readFile

-- Retorna uma lista de listas cujos elementos são as strings recebidas separadas pelo char espaço
splitStrBoardToMatrix :: [String] -> [[String]]
splitStrBoardToMatrix [] = []
splitStrBoardToMatrix (a:b) = [words a] ++ splitStrBoardToMatrix b

-- Recebe strings "A,B", e retorna uma lista contendo os elementos em Int da posição indicada
-- exemplo: splitElementsOfBoardStr ["1,3", "2,3"] 0 -> retorna [1, 2]
splitElementsOfBoardStr :: [String] -> Int -> [Int]
splitElementsOfBoardStr [] _ = []
splitElementsOfBoardStr (element : list) pos = [value] ++ splitElementsOfBoardStr list pos
    where value = read ((splitWhen (==',') element)!!pos) :: Int

-- 
getMatrixOfPos :: [[String]] -> Int -> [[Int]]
getMatrixOfPos [] _ = []
getMatrixOfPos (list : lists) pos = [splitElementsOfBoardStr list pos] ++ getMatrixOfPos lists pos

-- 
mapBoardFile :: [[String]] -> ([[Int]], [[Int]])
mapBoardFile stringBoard  = (valuesMatrix, positionsMatrix)
    where { valuesMatrix = getMatrixOfPos stringBoard 0 ;
        positionsMatrix = getMatrixOfPos stringBoard 1 }

-- 
loadBoard :: [String] -> ([[Int]], [[Int]])
loadBoard readFile = mapBoardFile (splitStrBoardToMatrix readFile)
-- Modulo responsavel por ler e mapear o documento tabuleiro.txt
module Reader where

-- Lê o arquivo indicado e retorna uma lista de strings separadas por quebra de linha
readBoardFile :: FilePath -> IO [String]
readBoardFile = fmap lines . readFile

-- A partir da String passada, cria uma lista divindo-a onde a condição passada for verdadeira
-- exemplo: splitWhen " " "Minha string" -> retorna ["Minha", "string"]
splitWhen :: (Char -> Bool) -> String -> [String]
splitWhen condition str = case dropWhile condition str of
                    "" -> []
                    str' -> splitted: splitWhen condition str''
                        where (splitted, str'') = break condition str'

-- Recebe uma lista de strings "A,B C,D" e retorna matriz de strings
-- exemplo: splitStrBoardToMatrix ["1,2 3,4", "5,6 7,8"] 0 -> retorna [["1,2", "3,4"], ["5,6", "7,8"]]------------------getMatrixFromList
splitStrBoardToMatrix :: [String] -> [[String]]
splitStrBoardToMatrix [] = []
splitStrBoardToMatrix (a:b) = [words a] ++ splitStrBoardToMatrix b

-- Recebe uma lista de strings "A,B", e retorna uma lista contendo os elementos em Int da posição indicada
-- exemplo: splitElementsOfBoardStr ["1,2", "3,4"] 0 -> retorna [1, 3]----------------------------------------getIntList--------------strToIntList
splitElementsOfBoardStr :: [String] -> Int -> [Int]
splitElementsOfBoardStr [] _ = []
splitElementsOfBoardStr (element : list) pos = [value] ++ splitElementsOfBoardStr list pos
    where value = read ((splitWhen (==',') element)!!pos) :: Int

-- Recebe uma matriz de strings "A,B", e retorna uma matriz contendo os elementos em Int da posição indicada
-- exemplo: getMatrixOfPos [["1,2", "3,4"], ["5,6", "7,8"]] 0 -> retorna [[1, 3], [5, 7]]---------------------getIntMatrix-------------strToIntMatrix
getMatrixOfPos :: [[String]] -> Int -> [[Int]]
getMatrixOfPos [] _ = []
getMatrixOfPos (list : lists) pos = [splitElementsOfBoardStr list pos] ++ getMatrixOfPos lists pos

-- Recebe uma matriz de strings "A,B", e retorna uma tupla contendo duas matrizes de Int
-- exemplo: mapBoardFile [["1,2", "3,4"], ["5,6", "7,8"]] 0 -> retorna ([[1, 3], [5, 7]], [[2, 4], [6, 8]])
mapBoardFile :: [[String]] -> ([[Int]], [[Int]])
mapBoardFile stringBoard  = (valuesMatrix, positionsMatrix)
    where { valuesMatrix = getMatrixOfPos stringBoard 0 ;
        positionsMatrix = getMatrixOfPos stringBoard 1 }

-- Recebe uma lista de strings "A,B C,D", e retorna uma tupla contendo duas matrizes de Int
-- exemplo: loadBoard ["1,2 3,4", "5,6 7,8"] 0 -> retorna ([[1, 3], [5, 7]], [[2, 4], [6, 8]])
loadBoard :: [String] -> ([[Int]], [[Int]])
loadBoard readFile = mapBoardFile (splitStrBoardToMatrix readFile)
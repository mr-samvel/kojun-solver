-- Módulo principal
module Main where

-- Importa módulos auxiliares
import Reader

-- Main
main = do
    tabuleiroStr <- readBoardFile "tabuleiro.txt" 
    let tabuleiro = loadBoard tabuleiroStr
    print tabuleiro

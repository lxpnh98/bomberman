{- |
Module: Main
Description: Módulo descrito em Haskell que
            implementa um bot para o jogo Bomberman.
Copyright: Alexandre Mendonça Pinho <a82441@alunos.uminho.pt>;
           Joel Filipe Esteves Gama <a82202@alunos.uminho.pt>;
-}
module Main where

import Data.Char (isDigit)
import System.Environment
import Text.Read
import Data.Maybe
import Tarefa6_li1g168 (bot)

-- | Função main.
main :: IO ()
main = do
    a <- getArgs
    let player = readMaybe (a !! 0)
    let ticks = readMaybe (a !! 1)
    w <- getContents
    if isJust player && isJust ticks
        then putStr $ show $ bot (lines w) (fromJust player) (fromJust ticks)
        else putStrLn "Parâmetros inválidos"

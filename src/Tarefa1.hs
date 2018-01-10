{- |
Module: Main
Description: Módulo descrito em Haskell que
             cria os mapas do jogo "Bomberman".
Copyrigth: Alexandre Mendonça Pinho <a82441@alunos.uminho.pt>;
           Joel Filipe Esteves Gama <a82202@alunos.uminho.pt>;
 -}
module Main where

import System.Environment
import Text.Read
import Data.Maybe

import System.Random
import Data.List

import Bomberman (mapa)

-- | Função main.
main :: IO ()
main = do a <- getArgs
          let s = readMaybe (a !! 0)
          let l = readMaybe (a !! 1)
          if length a == 2 && isJust s && isJust l && fromJust s >= 5 && odd (fromJust s)
             then putStr $ unlines $ mapa (fromJust s) (fromJust l)
             else putStrLn "Parametros invalidos"

{- |
Module: Bomberman
Description: Módulo descrito em Haskell que
             cria os mapas do jogo "Bomberman".
Copyrigth: Alexandre Mendonça Pinho <a82441@alunos.uminho.pt>;
           Joel Filipe Esteves Gama <a82202@alunos.uminho.pt>;
 -}
module Bomberman where

import System.Random
import Data.List

-- | Lista de coordenadas das posições do mapa que ficarão obrigatoriamente
-- vazias. Números negativos significa que se conta a posição a partir do
-- fim da linha ou coluna.
lista_vazios = [(x,y) | x <- [(-2),(-1),1,2], y <- [(-2),(-1),1,2], (abs x /=2) || (abs y /= 2)]

-- | Função que, dada a dimensão e semente, deverá devolver uma lista de
-- /Strings/ com os elementos do mapa.
mapa :: Int -> Int -> [String]
mapa dimensao seed = (replacePowerUps mapa1) ++ (mkBombs mapa1) ++ (getFlames mapa1)
    where lista_aleatorios = randomRs (0, 99) (mkStdGen seed)
          mapa1 = mkMap 0 dimensao lista_aleatorios

-- | Função que, dada a linha, a dimensão e a lista de números aleatórios,
-- devolve uma lista de /Strings/ que representam as linhas do mapa.
mkMap :: Int -> Int -> [Int] -> [String]
mkMap linha dim lst_rand
    | linha>=dim     = []
    | linha==0       = mkLine 0 linha dim lst_rand : mkMap (linha+1) dim lst_rand
    | linha==1       = mkLine 0 linha dim lst_rand : mkMap (linha+1) dim (drop (dim-6) lst_rand)
    | linha==2       = mkLine 0 linha dim lst_rand : mkMap (linha+1) dim (drop (dim-4-(div (dim-3) 2)) lst_rand)
    | linha==dim-1   = mkLine 0 linha dim lst_rand : mkMap (linha+1) dim lst_rand
    | linha==dim-2   = mkLine 0 linha dim lst_rand : mkMap (linha+1) dim (drop (dim-6) lst_rand)
    | linha==dim-3   = mkLine 0 linha dim lst_rand : mkMap (linha+1) dim (drop (dim-4-(div (dim-3) 2)) lst_rand)
    | mod linha 2==0 = mkLine 0 linha dim lst_rand : mkMap (linha+1) dim (drop (dim-2-(div (dim-3) 2)) lst_rand)
    | mod linha 2/=0 = mkLine 0 linha dim lst_rand : mkMap (linha+1) dim (drop (dim-2) lst_rand)

-- | Função que, dada a coluna, a linha, a dimensão e a lista de números
-- aleatórios, devolde uma /String/ correspondente à linha do mapa.
mkLine :: Int -> Int -> Int -> [Int] -> String
mkLine c linha dim rand
    | c>=dim                                     = []
    | linha == 0 || linha == (dim-1)             = '#' : mkLine (c+1) linha dim rand
    | c == 0 || c == (dim-1)                     = '#' : mkLine (c+1) linha dim rand
    | mod linha 2 == 0 && mod c 2 == 0           = '#' : mkLine (c+1) linha dim rand
    | elem (linha, c) (normDim dim lista_vazios) = ' ' : mkLine (c+1) linha dim rand
    | otherwise                                  = mkBlock (rand !! 0) : mkLine (c+1) linha dim (tail rand)

-- | Função que dado um número aleatório o converte num tijolo, espaço ou Power
-- Up.
mkBlock :: Int -> Char
mkBlock x | x<=1      = '+'
          | x<=3      = '!'
          | x<=39     = '?'
          | otherwise = ' '

-- | Função que adiciona a dimensao aos números negativos para poder comparar
-- as coordenadas.
--
-- >>> normDim 7 [(1,1),(1,(-1)),((-1),(-1))]
--               [(1,1),(1,5),(5,5)]
normDim :: Int -> [(Int, Int)] -> [(Int, Int)]
normDim _ []           = []
normDim dimensao (h:t) = (f,s):(normDim dimensao t)
    where f = if fst h < 0
                  then fst h + dimensao - 1
                  else fst h
          s = if snd h < 0
                  then snd h + dimensao - 1
                  else snd h

-- | Função que constroi uma lista de /Strings/ em que cada um representa um
-- Power Up Bomb no mapa.
mkBombs :: [String] -> [String]
mkBombs (h:t) = getLBombs 0 (h:t)

-- | Função que, dado uma linha e uma lista de /Strings/, devolve uma lista com
-- as linhas das Bombs.
getLBombs :: Int -> [String] -> [String]
getLBombs _ []    = []
getLBombs l (h:t) = (getCBombs l 0 h) ++ (getLBombs (l+1) t)

-- | Função que, dado uma linha, uma coluna e uma /String/, devolve uma lista
-- com as colunas das Bombs.
getCBombs :: Int -> Int -> String -> [String]
getCBombs _ _ [] = []
getCBombs l c (h:t)
    | h=='+' = ("+ " ++ show c ++ " " ++ show l):getCBombs l (c+1) t
    | otherwise = getCBombs l (c+1) t

-- | Função que constroi uma lista de /Strings/ em que cada um representa um
-- Power Up Flame no mapa.
getFlames :: [String] -> [String]
getFlames (h:t) = getLFlames 0 (h:t)

-- | Função que, dado uma linha e uma lista de /Strings/, devolve uma lista com
-- as linhas dos Flames.
getLFlames :: Int -> [String] -> [String]
getLFlames _ []    = []
getLFlames l (h:t) = (getCFlames l 0 h) ++ (getLFlames (l+1) t)

-- | Função que, dado uma linha, uma coluna e uma /String/, devolve uma lista
-- com as colunas dos Flames.
getCFlames :: Int -> Int -> String -> [String]
getCFlames _ _ [] = []
getCFlames l c (h:t)
    | h=='!' = ("! " ++ show c ++ " " ++ show l):getCFlames l (c+1) t
    | otherwise = getCFlames l (c+1) t

-- | Função que procura os caracteres correspondentes aos Power Ups e os
-- subtitui por tijolos.
replacePowerUps :: [String] -> [String]
replacePowerUps (h:t) = replaceLPowerUps 0 (h:t)

-- | Função que, dado um número e uma lista de /Strings/, devolve uma lista com
-- as linhas dos Power Ups.
replaceLPowerUps :: Int -> [String] -> [String]
replaceLPowerUps _ []    = []
replaceLPowerUps l (h:t) = (replaceCPowerUps l 0 h):(replaceLPowerUps (l+1) t)

-- | Função que, dado uma linha, uma coluna e uma /String/, devolve uma
-- /String/ das colunas dos Power Ups.
replaceCPowerUps :: Int -> Int -> String -> String
replaceCPowerUps _ _ [] = []
replaceCPowerUps l c (h:t)
    | h=='!'    = '?':replaceCPowerUps l (c+1) t
    | h=='+'    = '?':replaceCPowerUps l (c+1) t
    | otherwise = h:replaceCPowerUps l (c+1) t

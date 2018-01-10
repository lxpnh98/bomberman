{-|
Module: Main
Description: Módulo descrito em Haskell que
             define os comandos executados
             pelos jogadores no jogo "Bomberman"
Copyrigth: Alexandre Mendonça Pinho <a82441@alunos.uminho.pt>;
           Joel Filipe Esteves Gama <a82202@alunos.uminho.pt>;
 -}
module Main where

import Data.Char
import System.Environment

-- | Tipo que representa posições no mapa.
type Pos = (Int,Int)

-- | Função que, dado o estado actual do jogo no formato acima descrito, o
-- identificador de um jogador e o comando, deverá devolver o novo estado do
-- jogo.
move :: [String] -> Int -> Char -> [String]
move m x cmd
    | cmd == 'D' = cmdD m x
    | cmd == 'U' = cmdU m x
    | cmd == 'L' = cmdL m x
    | cmd == 'R' = cmdR m x
    | cmd == 'B' = cmdB m x

-- | Função auxiliar que define o comando 'U' (ir para cima).
cmdU :: [String] -> Int -> [String]
cmdU (h:t) x = if getPos (h:t) npos == ' '
               then setPlayer m x npos pw
               else (h:t)
                   where npos = (fst (getPlayerPos (h:t) x), (snd (getPlayerPos (h:t) x)) - 1)
                         pw = addPWInPos (h:t) (getPlayerPWs (h:t) x) npos
                         m = (removePW (h:t) npos)

-- | Função auxiliar que define o comando 'D' (ir para baixo).
cmdD :: [String] -> Int -> [String]
cmdD (h:t) x = if getPos (h:t) npos == ' '
               then setPlayer m x npos pw
               else (h:t)
                   where npos = (fst (getPlayerPos (h:t) x), (snd (getPlayerPos (h:t) x)) + 1)
                         pw = addPWInPos (h:t) (getPlayerPWs (h:t) x) npos
                         m = (removePW (h:t) npos)

-- | Função auxiliar que define o comando 'L' (ir para a esquerda).
cmdL :: [String] -> Int -> [String]
cmdL (h:t) x = if getPos (h:t) npos == ' '
               then setPlayer m x npos pw
               else (h:t)
                   where npos = (fst (getPlayerPos (h:t) x) - 1, (snd (getPlayerPos (h:t) x)))
                         pw = addPWInPos (h:t) (getPlayerPWs (h:t) x) npos
                         m = (removePW (h:t) npos)

-- | Função auxiliar que define o comando 'R' (ir para a direita).
cmdR :: [String] -> Int -> [String]
cmdR (h:t) x = if getPos (h:t) npos == ' '
               then setPlayer m x npos pw
               else (h:t)
                   where npos = (fst (getPlayerPos (h:t) x) + 1, (snd (getPlayerPos (h:t) x)))
                         pw = addPWInPos (h:t) (getPlayerPWs (h:t) x) npos
                         m = (removePW (h:t) npos)

-- | Função auxiliar que define o comando 'B' (colocar bomba).
cmdB :: [String] -> Int -> [String]
cmdB m x = if isBomb m pos || (maxBombs m x) <= (countBombs m x)
           then m
           else setBomb m x
               where pos = getPlayerPos m x

-- | Função que verifica se existe uma bomba numa dada posição.
--
-- >>> isBomb ["#######","#     #","# # # #",
--             "# ??  #","# #?# #","#     #",
--             "#######","+ 3 3","+ 3 4",
--             "* 3 5 0 1 10","0 3 5","1 5 5"] (3,5) = True
--
-- >>> isBomb ["#######","#     #","# # # #","# ??  #",
--             "# #?# #","#     #","#######","+ 3 3",
--             "+ 3 4","* 3 5 0 1 10","0 3 5","1 5 5"] (5,5) = False
isBomb :: [String] -> Pos -> Bool
isBomb [] _ = False
isBomb (h:t) (c,l) = if (h !! 0 == '*') && (read ((words h) !! 1) == c) && (read ((words h) !! 2) == l)
                   then True
                   else isBomb t (c,l)

-- | Função que posiciona uma bomba numa dada posição se ela já não existir.
--
-- >>> setBomb ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4","0 3 5","1 5 5"] 0
--             ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4","* 3 5 0 1 10","0 3 5","1 5 5"]
setBomb :: [String] -> Int -> [String]
setBomb (h:t) x
    | h !! 0 == '*' && (read ((words h) !! 2) > l) = b:h:t
    | h !! 0 == '*' && (read ((words h) !! 2) == l) && (read ((words h) !! 1) > c) = b:h:t
    | isDigit (h !! 0) = b:h:t
    | null t = h:b:t
    | otherwise = h:setBomb t x
        where b = ("* " ++ show c ++ " " ++ show l ++ " " ++ show x ++ " " ++ radius ++ " 10")
              (c,l) = getPlayerPos (h:t) x
              radius = show (1 + (length (filter (=='!') (getPlayerPWs (h:t) x))))

-- | Função que retorna o número máximo de bombas que um jogador pode colocar.
--
-- >>> maxBombs ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4","0 3 5 ++","1 5 5"] 0
--              3
maxBombs :: [String] -> Int -> Int
maxBombs (h:t) x =
    if (h !! 0) == intToDigit x
    then 1 + (length (filter (=='+') (getPlayerPWs (h:t) x)))
    else maxBombs t x

-- | Função que retorna o número de bombas que um jogador tem colocadas no mapa.
--
-- >>> countBombs ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4","* 3 5 0 1 10","0 3 5","1 5 5"] 0
--                1
countBombs :: [String] -> Int -> Int
countBombs [] _ = 0
countBombs (h:t) x =
    if (h !! 0) == '*' && (words h) !! 3 == show x
    then 1 + countBombs t x
    else countBombs t x

-- | Função que adiciona, se existir, o Power Up da posição ao jogador.
--
-- >>> addPWInPos ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4","0 3 3"] "" (3,3)
--                "+"
addPWInPos :: [String] -> String -> Pos -> String
addPWInPos [] s _ = s
addPWInPos (h:t) s (c,l)
    | h !! 0 == '+' && (read ((words h) !! 1) == c) && (read ((words h) !! 2) == l) = '+' : s
    | h !! 0 == '!' && (read ((words h) !! 1) == c) && (read ((words h) !! 2) == l) = s ++ "!"
    | otherwise = addPWInPos t s (c,l)

-- | Função que retorna uma String com os Power Ups do jogador x.
--
-- >>> getPlayerPWs ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4","* 3 5 0 1 10","0 3 5 +","1 5 5"]
--                  "+"
getPlayerPWs :: [String] -> Int -> String
getPlayerPWs (h:t) x = if h !! 0 == intToDigit x
                       then if length (words h) == 3
                           then ""
                           else (words h) !! 3
                       else getPlayerPWs t x

-- | Função que altera a posição e quantidade de Power Ups de um jogador.
--
-- >>> setPlayer ["0 3 5"] 0 (4,5) "+!"
--               ["0 4 5 +!"]
setPlayer :: [String] -> Int -> Pos -> String -> [String]
setPlayer (h:t) x (c,l) s = if (h !! 0 /= intToDigit x)
                            then h : setPlayer t x (c,l) s
                            else (show x ++ " " ++ show c ++ " " ++ show l ++ " " ++ s) : t

-- | Função que retorna o conteúdo de uma célula.
--
-- >>> getPos ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4","* 3 5 0 1 10","0 3 5 +","1 5 5"] (3,3)
--            "+"
getPos :: [String] -> Pos -> Char
getPos m (c,l) = (m !! l) !! c

-- | Função que retorna a possição de um jogador.
--
-- >>> getPlayerPos ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4","* 3 5 0 1 10","0 3 5 +","1 5 5"] 0
--                  (3,5)
getPlayerPos :: [String] -> Int -> Pos
getPlayerPos (h:t) x
    | h !! 0 == intToDigit x = (read (w !! 1), read (w !! 2))
    | otherwise = getPlayerPos t x
        where w = words h

-- | Função que, dado o estado atual do jogo e uma posição, revome, se existir, o Power Up nessa posição.
--
-- >>> removePW ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4","* 3 5 0 1 10","0 3 5 +","1 5 5"] (3,3)
--              ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 4","* 3 5 0 1 10","0 3 5 +","1 5 5"]
removePW :: [String] -> Pos -> [String]
removePW [] _ = []
removePW (h:t) (c,l)
    | (h !! 0 == '+' || h !! 0 == '!') && (read ((words h) !! 1) == c) && (read ((words h) !! 2) == l) = t
    | otherwise = h:(removePW t (c,l))

-- | Função main.
main :: IO ()
main = do a <- getArgs
          let p = a !! 0
          let c = a !! 1
          w <- getContents
          if length a == 2 && length p == 1 && isDigit (head p) && length c == 1 && head c `elem` "UDLRB"
             then putStr $ unlines $ move (lines w) (read p) (head c)
             else putStrLn "Parâmetros inválidos"

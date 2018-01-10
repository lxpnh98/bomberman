{- |
Module: Main
Description: Módulo descrito em Haskell que
             faz o jogo avançar no tempo.
Copyright: Alexandre Mendonça Pinho <a82441@alunos.uminho.pt>;
           Joel Filipe Esteves Gama <a82202@alunos.uminho.pt>;
 -}
module Main where

import Data.Char (isDigit)
import System.Environment
import Text.Read
import Data.Maybe

import Data.List (intersperse,delete)
import Common

-- | Representação de uma bomba (posição da bomba, tamanho da explosão)
type Bomb = (Pos, Int)

-- | Avança o estado do jogo um tick para a frente, explodindo as bombas
-- (incluindo os seus efeitos) e colocando os blocos da espiral nos ticks
-- finais.
avanca :: Map -> Int -> Map
avanca state tick =
    let map2 = if  tick <= calcTicks (calcDim state)
               then spiral state (calcTicks (calcDim state) - tick + 1) 1 (1,1) (1,0)
               else state
        (exploding, map3) = takeExploding map2
        map4 = tickBombs map3
        exploded = explode map4 exploding
        final = removeExploded exploded
    in final

-- | Reduz o contador decrescente de todas as bombas por um tick.
tickBombs :: Map -> Map
tickBombs [] = []
tickBombs (h:t) =
    if (h!!0) == '*'
    then ((concat . intersperse " ") s ++ " " ++ show ((read (tick!!0)) - 1)) : tickBombs t
    else h : tickBombs t
    where (s, tick) = splitAt 5 (words h)

-- | Retira do estado do jogo todos os elementos que foram marcados como
-- retirados.
removeExploded :: Map -> Map
removeExploded [] = []
removeExploded (h:t) = if (h!!0) == '#'
                        then (map (\c ->if c=='X' then ' ' else c) h) : removeExploded t
                        else if (h!!0) == 'X'
                             then removeExploded t
                             else h : removeExploded t

-- | Função que, dado um mapa, retorna um par de uma lista de bombas que
-- explodem no instante atual, e o mapa sem as linhas dessas bombas.
takeExploding :: Map -> ([Bomb], Map)
takeExploding [] = ([],[])
takeExploding (h:t) =
    if (h!!0) == '*' && read (w!!5) == 1
    then (((read (w!!1), (read (w!!2))),(read (w!!4))) : bombs,map)
    else (bombs, h : map)
    where w = words h
          (bombs, map) = takeExploding t

-- | Função que dado um mapa e uma lista de bombas retorna um outro mapa onde
-- se sentem os efeitos das explosões das bombas.
explode :: Map -> [Bomb] -> Map
explode map [] = map
explode map (h:t) = explode (explodeBomb map h 0) t

-- | Função que explode a bomba na sua própria posição, e nas 4 direções em que
-- a bomba explode.
explodeBomb :: Map -> Bomb -> Int -> Map
explodeBomb map (pos@(c,l), size) i
    | i==0 = (explodeBomb (killPlayers map pos) (pos,size) (i+1))
    | i==1 = foldl (explodeBombAux size 1 pos) map [(c,l-i),(c-i,l),(c+i,l),(c,l+i)]

-- | Função que, dado o tamanho da explosão, o tamanho atual, a posição da
-- bomba, um mapa, e a posição atual da explosão, retorna um mapa onde se
-- sentem os efeitos da explosão da bomba apenas na direção atual.
explodeBombAux :: Int -> Int -> Pos -> Map -> Pos -> Map
explodeBombAux size i (x,y) map (c,l)
    | isRock (c,l) map = map
    | isBrick (c,l) map = setPos map_ (c,l) 'X'
    | isInPos (c,l) map ['*'] = triggerBomb map_ (c,l)
    | isInPos (c,l) map ['!','+','X'] = rmPW map_ (c,l)
    | otherwise = if i<size && inMap (calcDim map) (calcNextPos (x,y) (c,l))
                  then explodeBombAux size (i+1) (x,y) map_ (calcNextPos (x,y) (c,l))
                  else map_
    where map_ = killPlayers map (c,l)

-- | Função que dadas duas posições alinhados pelo eixo horizontal ou vertical,
-- retorna a próxima posição, ou seja, aquela imediatamente após a segunda no
-- sentido oposto desta para a primeira.
--
-- >>> calcNextPos (1,1) (4,1) = (5,1)
calcNextPos :: Pos -> Pos -> Pos
calcNextPos (x1,y1) (x2,y2)
    | x2-x1<0 = (x2-1,y2)
    | x2-x1>0 = (x2+1,y2)
    | y2-y1<0 = (x2,y2-1)
    | y2-y1>0 = (x2,y2+1)

-- | Função que dadas a dimensão de um mapa e uma posição, testa se essa posição
-- se encontra dentro desse mapa.
inMap :: Int -> Pos -> Bool
inMap dim (x,y) = x>=0 && x<dim && y>=0 && y<dim

-- | Função que dados um mapa e uma posição, retorna um mapa cuja contador da
-- bomba na dada posiçäo é reduzido a 1, fazendo a bomba explodir no próximo
-- tick.
triggerBomb :: Map -> Pos -> Map
triggerBomb [] _ = []
triggerBomb (h:t) (c,l) = if isInPos (c,l) [h] ['*']
                          then ("* " ++ show c ++ " " ++ show l ++ " " ++ (w !! 3) ++ " " ++ (w !! 4) ++ " 1"):t
                          else h : triggerBomb t (c,l)
                          where w = words h

-- | Função que dados um mapa e uma posição, retorna um outro mapa sem
-- jogadores nessa posição.
killPlayers :: Map -> Pos -> Map
killPlayers [] _ = []
killPlayers (h:t) pos = if isInPos pos [h] ['0','1','2','3']
                        then killPlayers t pos
                        else h : killPlayers t pos

-- | Função /main/.
main :: IO ()
main = do
    a <- getArgs
    let ticks = readMaybe (a !! 0)
    w <- getContents
    if isJust ticks
        then putStr $ unlines $ avanca (lines w) (fromJust ticks)
        else putStrLn "Parâmetros inválidos"

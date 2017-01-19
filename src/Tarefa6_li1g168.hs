{- | 
Module: Tarefa6_li1g168 
Description: Módulo descrito em Haskell que  
            implementa um bot para o jogo Bomberman.
Copyright: Alexandre Mendonça Pinho <a82441@alunos.uminho.pt>; 
           Joel Filipe Esteves Gama <a82202@alunos.uminho.pt>; 
-}
module Tarefa6_li1g168 where

import Data.Char (intToDigit)
import Data.List (minimumBy,nub)

import Common
import Bomberman

-- | Representação de uma bomba.
type Bomb = (Pos,Int,Int)

-- | Função que dado um mapa, o número do jogador a controlar, e o número de
-- ticks até ao final do jogo, retorna um comando para o jogador.
bot :: [String] -> Int -> Int -> Maybe Char
bot mapa player ticks = 
    let playerPos = getPlayerPos mapa player
        movable = nub $ getMovable mapa playerPos playerPos []
        commonUtilityMap = calcCommonUtility mapa player ticks movable
        moveUtilityMap = calcMoveUtility mapa playerPos ticks commonUtilityMap
        bombUtilityMap = calcBombUtility mapa player commonUtilityMap
        cmd = getCmd mapa playerPos ticks moveUtilityMap bombUtilityMap
    in cmd

-- | Função que lista todas as posições que o jogador consegue alcançar.
getMovable :: [String] -> Pos -> Pos -> [Pos] -> [Pos]
getMovable mapa initPos pos@(c,l) checked = 
    if (isMovable mapa pos && not (elem pos checked) && (distanceSquared pos initPos) <= 12) || pos == initPos
    then pos : (getMovable mapa initPos (c-1,l) (pos:checked) ++
                getMovable mapa initPos (c+1,l) (pos:checked) ++
                getMovable mapa initPos (c,l-1) (pos:checked) ++
                getMovable mapa initPos (c,l+1) (pos:checked))
    else []

-- | Função que testa se um jogador pode ocupar uma dada posição 
isMovable :: [String] -> Pos -> Bool
isMovable mapa pos
    | elem (getPos mapa pos) ['#','?'] = False
    | otherwise = True

-- | Função que retorna uma lista de bombas dado um mapa.
getBombs :: [String] -> [Bomb]
getBombs [] = []
getBombs (h:t) = if (h!!0) == '*'
                 then ((read (w!!1),read (w!!2)), read (w!!3), read (w!!4)) : getBombs t
                 else getBombs t
                 where w = words h

-- | Função que dados um mapa, a posição do jogador, o número de ticks que
-- faltam, e as duas listas de utilidade de posições (uma para o jogador se
-- mover, outra para por bombas), determina o comando.
getCmd :: [String] -> Pos -> Int -> [(Pos,Float)] -> [(Pos,Float)] -> Maybe Char
getCmd mapa playerPos ticks moveUtility bombUtility
    | bestMoveValue >= bestBombValue && playerPos == bestMovePos = Nothing
    | bestMoveValue < bestBombValue && playerPos == bestBombPos = Just 'B'
    | bestMoveValue >= bestBombValue = getMove mapa playerPos bestMovePos ticks
    | bestMoveValue < bestBombValue = getMove mapa playerPos bestBombPos ticks
    where (bestMovePos,bestMoveValue) = getBestPos ((0,0),(maximum (map snd moveUtility))) moveUtility
          (bestBombPos,bestBombValue) = getBestPos ((0,0),(maximum (map snd bombUtility))) bombUtility

-- | Função que determina a melhor posição de uma lista de posições e os seus
-- respetivos valores de utilidade.
getBestPos :: (Pos,Float) -> [(Pos,Float)] -> (Pos,Float)
getBestPos x [] = x
getBestPos (maxPos,max) ((pos,x):t) =
    if x>=max
    then getBestPos (pos,x) t
    else getBestPos (maxPos,max) t

-- | Função que determina para onde o jogador se deve mover para chegar a uma
-- dada posição.
getMove :: [String] -> Pos -> Pos -> Int -> Maybe Char
getMove mapa playerPos@(x,y) pos tick
    | x < nextX = Just 'R'
    | x > nextX = Just 'L'
    | y < nextY = Just 'D'
    | y > nextY = Just 'U'
    | otherwise = Nothing
    where bestPath = minimumBy (\x y -> compare (length x) (length y)) $ getPathsTo mapa playerPos pos tick [] -- controi todos os possíveis caminhos para o destino, e seleciona o caminho mais curto (a lista de posições com o menor comprimento)
          (nextX,nextY) = if length bestPath == 1
                          then bestPath !! 0
                          else bestPath !! 1

-- | Função que retorna uma lista de caminhos possíveis de uma posição para
-- outra, representados por uma lista de posições.
getPathsTo :: [String] -> Pos -> Pos -> Int -> [Pos] -> [[Pos]]
getPathsTo mapa init@(x1,y1) end ticks currentPath =
    if init == end
    then [currentPath ++ [end]]
    else if isMovable mapa init && not (elem init currentPath)
         then getPathsTo mapa (x1-1,y1) end ticks (currentPath ++ [init]) ++
              getPathsTo mapa (x1+1,y1) end ticks (currentPath ++ [init]) ++
              getPathsTo mapa (x1,y1-1) end ticks (currentPath ++ [init]) ++
              getPathsTo mapa (x1,y1+1) end ticks (currentPath ++ [init])
         else []

-- | Função que retorna a possição de um jogador.
--
-- >>> getPlayerPos ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4","* 3 5 0 1 10","0 3 5 +","1 5 5"] 0
--                  (3,5) 
getPlayerPos :: [String] -> Int -> Pos
getPlayerPos (h:t) x
    | h !! 0 == intToDigit x = (read (w !! 1), read (w !! 2))
    | otherwise = getPlayerPos t x   
    where w = words h

--------------------------------------------------------------------------------

-- | Fator de desisão de proximidade de bombas.
bDistFactor :: Float
bDistFactor = (-1000)

-- | Fator de desisão de explosão de tijolos.
bEFactor :: Float
bEFactor = 1.5

-- | Fator de desisão de proximidade do centro.
distFromCenterFactor :: Float
distFromCenterFactor = 10

-- | Função que calcula o quadrado da distância entre uma posição e outra.
distanceSquared :: Pos -> Pos -> Float
distanceSquared (c,l) (x,y) = (fromIntegral ((abs (x-c))^2 + (abs (y-l))^2))

-- | Função que calcula o valor de utilidade de uma posição tendo em conta
-- apenas a sua distância à posição central.
distanceFromCenter :: Pos -> Pos -> Int -> Float
distanceFromCenter (c,l) (x,y) ticks =
    if mod (c-1) 4 == 0
    then 100 - distanceSquared (c,l) (x,y)
    else 100 - distanceSquared (c-1,l) (x,y)

-- | Função que calcula os valores de utilidade em comum de todas as posições
-- para que o jogador se consegue mover.
calcCommonUtility :: [String] -> Int -> Int -> [Pos] -> [(Pos,Float)]
calcCommonUtility _ _ _ [] = []
calcCommonUtility mapa player ticks (h:t) =
    (h, bDistFactor * (bombDistance mapa h) +
        distFromCenterFactor * (distanceFromCenter (c,c) h ticks)) : calcCommonUtility mapa player ticks t
    where c = length (mapa!!0) `div` 2

-- | Função que calcula os valores de utilidade para o movimento do jogador
-- para todas as posições para que se pode mover.
calcMoveUtility :: [String] -> Pos -> Int -> [(Pos,Float)] -> [(Pos,Float)]
calcMoveUtility _ _ _ [] = []
calcMoveUtility mapa playerPos ticks ((pos,v):t) = ((pos,v):t)
--    (pos,v) : calcMoveUtility mapa playerPos ticks t

-- | Função que calcula o valor de utilidade de uma dada posição tendo em conta apenas as bombas.
bombDistance :: [String] -> Pos -> Float
bombDistance mapa (x,y) = bombDistanceAux mapa (x,y) bombs
    where bombs = getBombs mapa

-- | Função auxiliar de /bombDistance/.
bombDistanceAux :: [String] -> Pos -> [Bomb] -> Float
bombDistanceAux _ _ [] = 0
bombDistanceAux mapa (c,l) (((x,y),size,ticks):t)
    | c == x && not (isBlocked mapa (c,l) (x,y) (0,signum (y-l))) = (-(fromIntegral (min 0 (abs (y-l)-(size+2))))) + bombDistanceAux mapa (c,l) t -- diretamente prop. ao tamanho da expl.
    | l == y && not (isBlocked mapa (c,l) (x,y) (signum (x-c),0)) = (-(fromIntegral (min 0 (abs (x-c)-(size+2))))) + bombDistanceAux mapa (c,l) t
    | otherwise = 0 + bombDistanceAux mapa (c,l) t

-- | Função que testa se uma explosão está bloqueada.
isBlocked :: [String] -> Pos -> Pos -> (Int,Int) -> Bool
isBlocked mapa (c,l) end (i,j)
    | (c,l) == end = False
    | elem (getPos mapa (c,l)) ['#','?'] = True
    | isInPos (c,l) mapa ['+','!'] = True
    | otherwise = isBlocked mapa (c+i,l+j) end (i,j)

--------------------------------------------------------------------------------

-- | Função que calcula os valores de utilidade para a introdução de bombas
-- pelo jogador para todas as posições para que se pode mover.
calcBombUtility :: [String] -> Int -> [(Pos,Float)] -> [(Pos,Float)]
calcBombUtility _ _ [] = []
calcBombUtility mapa player ((pos,v):t) =
    (pos,v + (bEFactor * bricksExploded mapa player pos)): calcBombUtility mapa player t

-- | Função que calcula o número de tijolos que uma bomba explodiria se posto
-- numa dada posição.
bricksExploded :: [String] -> Int -> Pos -> Float
bricksExploded mapa player (c,l) = 
    fromIntegral (countBricks mapa (c,l) (0,(-1)) size 0 +
                  countBricks mapa (c,l) (0,1) size 0 +
                  countBricks mapa (c,l) ((-1),0) size 0 +
                  countBricks mapa (c,l) (1,0) size 0) 
    where size = getExplosionSize mapa player

-- | Função auxiliar de /bricksExploded/.
countBricks :: [String] -> Pos -> (Int,Int) -> Int -> Int -> Int
countBricks mapa (c,l) (i,j) size x
    | x > size = 0
    | cellContent == '?' = 1
    | cellContent == '#' = 0
    | isInPos (c,l) mapa ['+','!'] = 0
    | otherwise = countBricks mapa (c+i,l+j) (i,j) size (x+1)
    where cellContent = getPos mapa (c,l)

-- | Função que determina o tamanho da explosão de uma bomba posto por um dado jogador.
getExplosionSize :: [String] -> Int -> Int
getExplosionSize (h:t) x = if (h!!0) == intToDigit x
                           then if length (words h) == 4
                                then 1 + (length $ filter (=='!') $ (words h) !! 3)
                                else 1
                           else getExplosionSize t x


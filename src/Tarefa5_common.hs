{- |
Module: Tarefa5_common
Description: Módulo descrito em Haskell que
             define funções e tipos comuns para os módulos da tarefa 5.
Copyright: Alexandre Mendonça Pinho <a82441@alunos.uminho.pt>;
           Joel Filipe Esteves Gama <a82202@alunos.uminho.pt>;
 -}
module Tarefa5_common where

import Graphics.Gloss.Interface.Pure.Game

-- | O tipo de um dado bloco.
data Tile = Rock | Brick | Empty | RemovedTile
    deriving Eq

-- | O movimento de um jogador.
data Motion = StillU | StillD | StillL | StillR | MUp | MDown | MLeft | MRight

-- | Para comparar o movimento de jogadores, as 4 variações de parado (para
-- possibilitar desenha-las) são equivalentes.
instance Eq Motion where
    StillU == StillU = True
    StillD == StillU = True
    StillL == StillU = True
    StillR == StillU = True

    StillU == StillD = True
    StillD == StillD = True
    StillL == StillD = True
    StillR == StillD = True

    StillU == StillL = True
    StillD == StillL = True
    StillL == StillL = True
    StillR == StillL = True

    StillU == StillR = True
    StillD == StillR = True
    StillL == StillR = True
    StillR == StillR = True

    MUp == MUp = True
    MDown == MDown = True
    MLeft == MLeft = True
    MRight == MRight = True

    _ == _ = False

-- | Representação de um botão, com o seu nome, e a transformação que ocorre ao
-- estado quando o botão é ativado.
type Button = (String,(Estado -> Estado))

-- | Estado principal, para saber se se está no menu ou a jogar.
data MainState = Menu [Button] Int | Game

-- | Apenas se compara os contrutores para testar a igualdade de estados
-- principais.
instance Eq MainState where
    Menu _ _ == Menu _ _ = True
    Game == Game = True
    _ == _ = False

-- | Posição
type Pos = (Int,Int)

-- | Tipo de powerup (RemovedPW serve para saber quais aqueles que foram
-- removidos)
data PWType = BombPW | FlamePW | RemovedPW
    deriving Eq

-- | Representação de um powerup.
type PW = (PWType,Pos)

-- | Representação de uma bomba.
type Bomb = (Pos,Int,Int,Int) -- ponto, número do jogador, tamnho da explosão, ticks ate explosao

-- | Tipo de explosão (para depois as desenhar).
data BlastType = CenterBlast |
                 VerticalBlast |
                 HorizontalBlast |
                 UpperBlast |
                 LowerBlast |
                 RightBlast |
                 LeftBlast

-- | Representação de uma explosão
type Blast = (BlastType,Pos)

-- | A representação de um jogador.
type Player = (Int, Pos, (Int,Int), Motion, (Either (Key,Key,Key,Key,Key) ([String] -> Int -> Int -> Maybe Char)))

-- | A representação do estado do jogo.
data Estado = E {mainState :: MainState, pics :: [Picture], tileMap :: [[Tile]], pws :: [PW], bombs :: [Bomb], players :: [Player], blasts :: [Blast] , time :: Float}

-- | Função que verifica se existe uma pedra numa dada posição.
isRock :: Pos -> [[Tile]] -> Bool
isRock (c,l) mapa = ((mapa !! l) !! c) == Rock

-- | Função que verifica se existe um tijolo numa dada posição.
isBrick :: Pos -> [[Tile]] -> Bool
isBrick (c,l) mapa = elem ((mapa !! l) !! c) [Brick,RemovedTile]

-- | Função que verifica se existe um espaço vazio numa dada posição.
isEmpty :: Pos -> [[Tile]] -> Bool
isEmpty (c,l) mapa = ((mapa !! l) !! c) == Empty

{- | 
Module: Tarefa5_draw 
Description: Módulo descrito em Haskell que  
             desenha o estado do jogo Bomberman.
Copyright: Alexandre Mendonça Pinho <a82441@alunos.uminho.pt>; 
           Joel Filipe Esteves Gama <a82202@alunos.uminho.pt>; 
 -}
module Tarefa5_draw where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture

import Tarefa5_config
import Tarefa5_common

-- | Função que desenha o jogo.
drawState :: Estado -> Picture
drawState s = 
    case s of
        (E (Menu buttons x) _ _ _ _ _ _ _) -> Pictures [Scale titleSize titleSize (Translate (fromIntegral ((-(windowWidth `div` 2)) + 30)) (fromIntegral ((windowHeight `div` 2) - 120)) (Text "Bomberman")),Translate 0 (-300) $ drawRects buttons x 0 textSize]
            where titleSize = 0.7
                  textSize = 0.2
        (E Game [bomb,
               playerD,playerU,playerL,playerR,
               player2D,player2U,player2L,player2R,
               player3D,player3U,player3L,player3R,
               player4D,player4U,player4L,player4R,
               bombPW,flamePW,
               brick,empty,rockTL,rockTR,rockBL,rockBR,rockTop,rockBottom,rockLeft,rockRight,rockCenter,
               center,horizontal,vertical,upper,lower,left,right] mapa pws bombs players blasts _) ->
            Translate (fromIntegral x) (fromIntegral y)
                (Pictures [drawMap (length mapa) mapa (0,0) tileSize [brick,empty,rockTL,rockTR,rockBL,rockBR,rockTop,rockBottom,rockLeft,rockRight,rockCenter],                                        
                           drawPWs mapa pws tileSize [bombPW,flamePW],
                           drawBombs bombs tileSize bomb,
                           drawPlayers players tileSize [playerD,playerU,playerL,playerR,player2D,player2U,player2L,player2R,player3D,player3U,player3L,player3R,player4D,player4U,player4L,player4R],
                           drawBlasts blasts tileSize [center,horizontal,vertical,upper,lower,left,right]])
    where tileSize = (windowHeight `div` mapDim)
          (x,y) = ((-windowHeight `div` 2) + (tileSize `div` 2) + ((mod windowHeight mapDim) `div` 2),
                   ((windowHeight `div` 2) - (tileSize `div` 2) - ((mod windowHeight mapDim) `div` 2)))

-- | Função que desenha o menu.
drawRects :: [Button] -> Int -> Int -> Float -> Picture
drawRects [] _ _ _ = Blank
drawRects (h:t) selected current size = 
    Pictures [Translate 0 (fromIntegral ((windowHeight `div` 2) - ((current)*150))) (Pictures [Color rectColor (rectangleSolid 300 50), Color textColor (Translate (-30) (-10) (Scale size size (Text (fst h))))]), drawRects t selected (current+1) size]
    where (rectColor,textColor) = if current == selected then (greyN 0.3,greyN 0.0) else (greyN 0.5,greyN 0.0)

-- | Função que desenha os blocos do mapa.
drawMap :: Int -> [[Tile]] -> Pos -> Int -> [Picture] -> Picture
drawMap _ [] _ _ _ = Blank
drawMap dim (h:t) (c,l) x pics = Pictures [(drawLine dim h (c,l) x pics),(drawMap dim t (c,(l-x)) x pics)]

-- | Funçao que desenha cada linha do mapa.
drawLine :: Int -> [Tile] -> Pos -> Int -> [Picture] -> Picture
drawLine _ [] _ _ _ = Blank
drawLine dim (h:t) (c,l) x pics = Pictures [(Translate  (fromIntegral c) (fromIntegral l) (drawTile dim (fromEnum ((fromIntegral c)/(fromIntegral x)),fromEnum ((fromIntegral (-l))/(fromIntegral x))) h x pics)),(drawLine dim t ((c+x),l) x pics)]

-- | Funçao que desenha um bloco individual.
drawTile :: Int -> Pos -> Tile -> Int -> [Picture] -> Picture
drawTile _ _ Brick x [brick,_,_,_,_,_,_,_,_,_,_] = Scale ((fromIntegral x)/48.0) ((fromIntegral x)/48.0) brick
drawTile _ _ Empty x [_,empty,_,_,_,_,_,_,_,_,_] = Scale ((fromIntegral x)/48.0) ((fromIntegral x)/48.0) empty
drawTile dim (c,l) Rock x [_,_,rockTL,rockTR,rockBL,rockBR,rockTop,rockBottom,rockLeft,rockRight,rockCenter]
    | (c,l) == (0,0)             = Scale ((fromIntegral x)/48.0) ((fromIntegral x)/48.0) rockTL
    | (c,l) == ((dim-1),0)       = Scale ((fromIntegral x)/48.0) ((fromIntegral x)/48.0) rockTR
    | (c,l) == (0,(dim-1))       = Scale ((fromIntegral x)/48.0) ((fromIntegral x)/48.0) rockBL
    | (c,l) == ((dim-1),(dim-1)) = Scale ((fromIntegral x)/48.0) ((fromIntegral x)/48.0) rockBR
    | c == 0                     = Scale ((fromIntegral x)/48.0) ((fromIntegral x)/48.0) rockLeft
    | c == (dim-1)               = Scale ((fromIntegral x)/48.0) ((fromIntegral x)/48.0) rockRight
    | l == 0                     = Scale ((fromIntegral x)/48.0) ((fromIntegral x)/48.0) rockTop
    | l == (dim-1)               = Scale ((fromIntegral x)/48.0) ((fromIntegral x)/48.0) rockBottom
    | otherwise                  = Scale ((fromIntegral x)/48.0) ((fromIntegral x)/48.0) rockCenter

-- | Função que desenha os powerups.
drawPWs :: [[Tile]] -> [PW] -> Int -> [Picture] -> Picture
drawPWs _ [] _ _ = Blank
drawPWs mapa ((pwtype,(c,l)):t) x pics =
    if (not ((isRock (c,l) mapa) || (isBrick (c,l) mapa)))
    then Pictures [(Translate ((fromIntegral c)*(fromIntegral x)) (-(fromIntegral l)*(fromIntegral x)) (drawPW pwtype x pics)),(drawPWs mapa t x pics)]
    else drawPWs mapa t x pics

-- | Função que desenha um powerup individual.
drawPW :: PWType -> Int -> [Picture] -> Picture
drawPW BombPW x [bombPic,_]= Scale ((fromIntegral x)/48.0) ((fromIntegral x)/48.0) bombPic
drawPW FlamePW x [_,flamePic]= Scale ((fromIntegral x)/48.0) ((fromIntegral x)/48.0) flamePic

-- | Função que desenha as bombas.
drawBombs :: [Bomb] -> Int -> Picture -> Picture
drawBombs [] _ _ = Blank
drawBombs (((c,l),_,_,_):t) x pic = Pictures [(Translate ((fromIntegral c)*(fromIntegral x)) (-(fromIntegral l)*(fromIntegral x)) (drawBomb x pic)),(drawBombs t x pic)]

-- | Função que desenha uma bomba individual.
drawBomb :: Int -> Picture -> Picture
drawBomb x bomb = Scale ((fromIntegral x)/48.0) ((fromIntegral x)/48.0) bomb

-- | Função que desenha os jogadores.
drawPlayers :: [Player] -> Int -> [Picture] -> Picture
drawPlayers [] _ _ = Blank
drawPlayers (player@(_,(c,l),_,_,_):t) x pics = Pictures [(Translate ((fromIntegral c)*(fromIntegral x)) (-(fromIntegral l)*(fromIntegral x)) (drawPlayer x player pics)),(drawPlayers t x pics)]

-- | Função que desenha cada jogador, selecionando diferentes imagens para cada
-- um.
drawPlayer :: Int -> Player -> [Picture] -> Picture
drawPlayer x (p,_,_,StillD,_) pics = Scale ((fromIntegral x)/48.0) ((fromIntegral x)/48.0) (pics !! (p*4))
drawPlayer x (p,_,_,MDown,_)  pics = Scale ((fromIntegral x)/48.0) ((fromIntegral x)/48.0) (pics !! (p*4))
drawPlayer x (p,_,_,StillU,_) pics = Scale ((fromIntegral x)/48.0) ((fromIntegral x)/48.0) (pics !! (p*4 + 1))
drawPlayer x (p,_,_,MUp,_)    pics = Scale ((fromIntegral x)/48.0) ((fromIntegral x)/48.0) (pics !! (p*4 + 1))
drawPlayer x (p,_,_,StillL,_) pics = Scale ((fromIntegral x)/48.0) ((fromIntegral x)/48.0) (pics !! (p*4 + 2))
drawPlayer x (p,_,_,MLeft,_)  pics = Scale ((fromIntegral x)/48.0) ((fromIntegral x)/48.0) (pics !! (p*4 + 2))
drawPlayer x (p,_,_,StillR,_) pics = Scale ((fromIntegral x)/48.0) ((fromIntegral x)/48.0) (pics !! (p*4 + 3))
drawPlayer x (p,_,_,MRight,_) pics = Scale ((fromIntegral x)/48.0) ((fromIntegral x)/48.0) (pics !! (p*4 + 3))

-- | Função que desenha as explosões.
drawBlasts :: [Blast] -> Int -> [Picture] -> Picture
drawBlasts [] _ _ = Blank
drawBlasts ((blastType,(c,l)):t) x pics = Pictures [(Translate ((fromIntegral c)*(fromIntegral x)) (-(fromIntegral l)*(fromIntegral x)) (drawBlast blastType x pics)),(drawBlasts t x pics)]

-- | Função que desenha um explosão.
drawBlast :: BlastType -> Int -> [Picture] -> Picture
drawBlast CenterBlast x [center,horizontal,vertical,upper,lower,left,right] = Scale ((fromIntegral x)/48.0) ((fromIntegral x)/48.0) center
drawBlast HorizontalBlast x [center,horizontal,vertical,upper,lower,left,right] = Scale ((fromIntegral x)/48.0) ((fromIntegral x)/48.0) horizontal
drawBlast VerticalBlast x [center,horizontal,vertical,upper,lower,left,right] = Scale ((fromIntegral x)/48.0) ((fromIntegral x)/48.0) vertical
drawBlast UpperBlast x [center,horizontal,vertical,upper,lower,left,right] = Scale ((fromIntegral x)/48.0) ((fromIntegral x)/48.0) upper
drawBlast LowerBlast x [center,horizontal,vertical,upper,lower,left,right] = Scale ((fromIntegral x)/48.0) ((fromIntegral x)/48.0) lower
drawBlast LeftBlast x [center,horizontal,vertical,upper,lower,left,right] = Scale ((fromIntegral x)/48.0) ((fromIntegral x)/48.0) left
drawBlast RightBlast x [center,horizontal,vertical,upper,lower,left,right] = Scale ((fromIntegral x)/48.0) ((fromIntegral x)/48.0) right


{- | 
Module: Main 
Description: Módulo descrito em Haskell que  
            implementa o jogo Bomberman utilizando o Gloss (e a biblioteca
            gloss-juicy).
Copyright: Alexandre Mendonça Pinho <a82441@alunos.uminho.pt>; 
           Joel Filipe Esteves Gama <a82202@alunos.uminho.pt>; 
-}
module Main where

import Graphics.Gloss         
import Graphics.Gloss.Data.Picture  
import Graphics.Gloss.Interface.Pure.Game

import System.Random
import Data.Maybe (fromJust)
import Graphics.Gloss.Juicy

import Tarefa5_common
import Tarefa5_config
import Tarefa5_update
import Tarefa5_draw

import Bomberman

import Tarefa6_li1g168

-- | Frame rate
fr :: Int
fr = 50

-- | Jogador humano 1
player0 = (0,(1,1),(0,0),StillD,
           (Left ((SpecialKey KeyUp),
                  (SpecialKey KeyDown),
                  (SpecialKey KeyLeft),
                  (SpecialKey KeyRight),
                  (Char '5'))))
-- | Jogador humano 2
player1 = (1,(mapDim-2,mapDim-2),(0,0),StillD,
           (Left ((Char 'w'),
                  (Char 's'),
                  (Char 'a'),
                  (Char 'd'),
                  (Char '1'))))
-- | Jogador humano 3
player2 = (2,(mapDim-2,1),(0,0),StillD,
           (Left ((SpecialKey KeyUp),
                  (SpecialKey KeyDown),
                  (SpecialKey KeyLeft),
                  (SpecialKey KeyRight),
                  (Char '5'))))
-- | Jogador humano 4
player3 = (3,(1,mapDim-2),(0,0),StillD,
           (Left ((SpecialKey KeyUp),
                  (SpecialKey KeyDown),
                  (SpecialKey KeyLeft),
                  (SpecialKey KeyRight),
                  (Char '5'))))

-- | Jogador não-humano 1
playerAI0 = (0,(1,1),(0,0),StillD,
             (Right bot))
-- | Jogador não-humano 2
playerAI1 = (1,(mapDim-2,mapDim-2),(0,0),StillD,
             (Right bot))
-- | Jogador não-humano 3
playerAI2 = (2,(mapDim-2,1),(0,0),StillD,
             (Right bot))
-- | Jogador não-humano 4
playerAI3 = (3,(1,mapDim-2),(0,0),StillD,
             (Right bot))

-- | O estado inicial do jogo.
initialState :: [Picture] -> Int -> Estado
initialState pics seed =
    (E (Menu [("Jogar",mkGame)] 0) pics tiles pws [] initPlayerList [] (fromIntegral(gameLength)*tickLength)) -- 'x' para selecionar botão
    --(E Game pics tiles pws [] initPlayerList [] (fromIntegral(gameLength)*tickLength))
    where (tiles,pws) = convertMap $ mapa mapDim seed
          initPlayerList = [player0,playerAI1,playerAI2,playerAI3]

-- | Função que altera o estado principal para o jogo (começa o jogo).
mkGame :: Estado -> Estado
mkGame s = s {mainState = Game}

-- | Display mode
dm :: Display
dm = InWindow "Bomberman" (windowWidth, windowHeight) (0, 0) -- para testar
--dm = FullScreen (windowWidth, windowHeight) -- para mostrar

-- | Função principal que invoca o jogo.
main :: IO ()
main = do bomb <- loadJuicyPNG "Bomba.png"

          playerD <- loadJuicyPNG "playerD.png"
          playerU <- loadJuicyPNG "playerU.png"
          playerL <- loadJuicyPNG "playerL.png"
          playerR <- loadJuicyPNG "playerR.png"

          player2D <- loadJuicyPNG "player2D.png"
          player2U <- loadJuicyPNG "player2U.png"
          player2L <- loadJuicyPNG "player2L.png"
          player2R <- loadJuicyPNG "player2R.png"

          player3D <- loadJuicyPNG "player3D.png"
          player3U <- loadJuicyPNG "player3U.png"
          player3L <- loadJuicyPNG "player3L.png"
          player3R <- loadJuicyPNG "player3R.png"

          player4D <- loadJuicyPNG "player4D.png"
          player4U <- loadJuicyPNG "player4U.png"
          player4L <- loadJuicyPNG "player4L.png"
          player4R <- loadJuicyPNG "player4R.png"

          bombPW  <- loadJuicyPNG "BombPW.png"
          flamePW <- loadJuicyPNG "FlamePW.png"

          brick <- loadJuicyPNG "brick.png"
          empty <- loadJuicyPNG "empty.png"

          rockTL     <- loadJuicyPNG "rockTL.png"
          rockTR     <- loadJuicyPNG "rockTR.png"
          rockBL     <- loadJuicyPNG "rockBL.png"
          rockBR     <- loadJuicyPNG "rockBR.png"
          rockTop    <- loadJuicyPNG "rockTop.png"
          rockBottom <- loadJuicyPNG "rockBottom.png"
          rockLeft   <- loadJuicyPNG "rockLeft.png"
          rockRight  <- loadJuicyPNG "rockRight.png"
          rockCenter <- loadJuicyPNG "rockCenter.png"

          center     <- loadJuicyPNG "centerBlast.png"
          horizontal <- loadJuicyPNG "horizontalBlast.png"
          vertical   <- loadJuicyPNG "verticalBlast.png"
          upper      <- loadJuicyPNG "upperBlast.png"
          lower      <- loadJuicyPNG "lowerBlast.png"
          left       <- loadJuicyPNG "leftBlast.png"
          right      <- loadJuicyPNG "rightBlast.png"

          seed <- randomRIO (1,10000)

          play dm              -- display mode
              (greyN 0.7)     -- côr do fundo da janela
              fr              -- frame rate
              (initialState (map (fromJust) [bomb,
                                             playerD,playerU,playerL,playerR,
                                             player2D,player2U,player2L,player2R,
                                             player3D,player3U,player3L,player3R,
                                             player4D,player4U,player4L,player4R,
                                             bombPW,flamePW,
                                             brick,empty,rockTL,rockTR,rockBL,rockBR,rockTop,rockBottom,rockLeft,rockRight,rockCenter,
                                             center,horizontal,vertical,upper,lower,left,right]) seed)   -- estado inicial
              drawState       -- desenha o estado do jogo
              handleEvent     -- reage a um evento
              handleTime      -- reage ao passar do tempo

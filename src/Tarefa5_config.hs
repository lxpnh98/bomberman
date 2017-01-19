{- | 
Module: Tarefa5_config 
Description: Módulo descrito em Haskell que  
             configura o jogo Bomberman. 
Copyright: Alexandre Mendonça Pinho <a82441@alunos.uminho.pt>; 
           Joel Filipe Esteves Gama <a82202@alunos.uminho.pt>; 
 -}
module Tarefa5_config where

-- | Largura da janela
windowWidth :: Int
windowWidth = 800

-- | Altura da janela
windowHeight :: Int
windowHeight = 600

-- | Tempo que um tick demora (em segundos)
tickLength :: Float
tickLength = 1/3

-- | Número de ticks numa partida
gameLength :: Int
gameLength = (mapDim + 3) ^ 2 -- ticks

-- | Dimensão do mapa gerado.
mapDim :: Int
mapDim = 13


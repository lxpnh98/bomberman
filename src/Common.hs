{- | 
Module: Common 
Description: Módulo descrito em Haskell que  
             define funções comuns entre a tarefa 4 e 6. 
Copyright: Alexandre Mendonça Pinho <a82441@alunos.uminho.pt>; 
           Joel Filipe Esteves Gama <a82202@alunos.uminho.pt>; 
 -}
module Common where

-- | Tipo que representa o mapa do jogo.
type Map = [String]

-- | Tipo que representa uma posição.
type Pos = (Int, Int)

-- | Função que verifica se existe uma pedra numa dada posição.
isRock :: Pos -> Map -> Bool
isRock (c,l) map = ((map !! l) !! c) == '#'

-- | Função que verifica se existe um tijolo numa dada posição.
isBrick :: Pos -> Map -> Bool
isBrick (c,l) map = elem ((map !! l) !! c) ['?','X']

-- | Função que verifica se existem algum Powerup, bomba, ou jogador (dependendo
-- do terceiro argumento) numa dada posição.
--
-- >>> isInPos (3,8) ["! 3 8"] ['!'] = True
--
-- >>> isInPos (3,8) ["2 3 8"] ['0','1','2','3'] = True
--
-- >>> isInPos (3,8) ["+ 3 8"] ['*'] = False
--
-- >>> isInPos (5,2) ["+ 3 8"] ['+'] = False
isInPos :: Pos -> Map -> [Char] -> Bool
isInPos _ [] _ = False
isInPos (c,l) (h:t) charList = if (elem (h !! 0) charList) && (read (w !! 1) == c) && (read (w !! 2) == l)
                      then True
                      else isInPos (c,l) t charList
                      where w = words h

-- | Função que retorna o conteúdo de uma célula.
--
-- >>> getPos ["#######","#     #","# # # #",
--             "# ??  #","# #?# #","#     #",
--             "#######","+ 3 3","+ 3 4",
--             "* 3 5 0 1 10","0 3 5 +","1 5 5"] (3,3) = "+" 
getPos :: [String] -> Pos -> Char
getPos m (c,l) = (m !! l) !! c 

-- | Função que dados um mapa, uma posição e um carater, retorna uma mapa onde
-- o carater na posição dada foi alterado para o carater no terceiro argumento.
setPos :: Map -> Pos -> Char -> Map
setPos map (c,l) chr = (take l map) ++ [setPosAux (map !! l) c chr] ++ (drop (l+1) map)

-- | Função que dada uma linha do mapa, o numero da coluna a alterar e um
-- carater, retorna a linha com essa coluna alterada para o carater.
setPosAux :: String -> Int -> Char -> String
setPosAux str c chr = (take c str) ++ [chr] ++ (drop (c+1) str)

-- | Função que marca um powerup numa dada posição como removido.
rmPW :: Map -> Pos -> Map
rmPW [] _ = []
rmPW (h:t) pos = if isInPos pos [h] ['!','+']
                 then ("X " ++ (w!!1) ++ " " ++ (w!!2)) : rmPW t pos
                 else h : rmPW t pos
                 where w = words h

-- | Função que dado um mapa calcula a sua dimensão.
calcDim :: Map -> Int
calcDim (h:_) = length h

-- | Função que dada a dimensão de uma mapa calcula o número de ticks
-- necessários até que a espiral comece.
calcTicks :: Int -> Int
calcTicks n = (n-2)^2

-- | Função que implementa a espiral.
spiral :: Map -> Int -> Int -> Pos -> (Int,Int) -> Map
spiral map 0 _ _ _ = map
spiral map blocks border pos@(c,l) movement =
    spiral map_ (blocks-1) border_ pos_ movement_
    where map_ = if (c==(div (calcDim map) 2)) && (l==(div (calcDim map) 2))
                 then map
                 else rmPW (setPos map pos '#') pos
          pos_ = (fst (pos) + fst (movement),snd (pos) + snd (movement))
          (movement_, border_) = reactToEdge map pos movement border

-- | Função que muda a direção de um vetor quando a dada posição de aproxima um
-- dado número de blocos da borda, e aumenta este último número sempre que muda
-- de direção para cima. A funçäo determina em que direção a espiral deverá se
-- mover no próximo tick.
reactToEdge :: Map -> Pos -> (Int,Int) -> Int -> ((Int,Int), Int)
reactToEdge map pos m@(1,0) border =
    if (fst (pos) + border) == ((calcDim map) - 2)
    then ((0,1),border)
    else (m,border)
reactToEdge map pos m@(0,1) border =
    if (snd (pos) + border) == ((calcDim map) - 2)
    then (((-1),0),border)
    else (m,border)
reactToEdge map pos m@((-1),0) border =
    if (fst (pos) - border) == 1
    then ((0,(-1)),(border+1))
    else (m,border)
reactToEdge map pos m@(0,(-1)) border =
    if (snd (pos) - border) == 1
    then ((1,0),border)
    else (m,border)


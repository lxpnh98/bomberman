{- | 
Module: Tarefa5_update 
Description: Módulo descrito em Haskell que  
             reage a eventos e faz o tempo avançar no jogo Bomberman.
Copyright: Alexandre Mendonça Pinho <a82441@alunos.uminho.pt>; 
           Joel Filipe Esteves Gama <a82202@alunos.uminho.pt>; 
 -}
module Tarefa5_update where

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List (intersect)

import Tarefa5_config
import Tarefa5_common

-- | Função que converte mapas gerados pela função /mapa/ no módulo Bomberman
-- em tuplos de mapas como representados na tarefa 5 e listas de powerups.
convertMap :: [String] -> ([[Tile]], [PW])
convertMap m = let (tiles,pws) = splitAt (length (m!!0)) m
                   pws_ = splitPWs pws
               in (map (map charToTile) tiles,pws_)

-- | Função que extrai os powerups da lista de /String/ gerada na tarefa 1.
splitPWs :: [String] -> [PW]
splitPWs [] = []
splitPWs (h:t)
    | (h!!0) == '+' = ((BombPW,pos):rPws)
    | (h!!0) == '!' = ((FlamePW,pos):rPws)
    where rPws = splitPWs t
          w = words h
          pos = (read (w!!1),(read (w!!2)))

-- | Função que converte um carater num /Tile/.
charToTile :: Char -> Tile
charToTile '#' = Rock
charToTile '?' = Brick
charToTile ' ' = Empty

-- | Função que converte um /Tile/ num carater.
tileToChar :: Tile -> Char
tileToChar Rock  = '#'
tileToChar Brick = '?'
tileToChar Empty = ' '

-- | Função que contrói um /String/ que representa um powerup para utilizar
-- quando é chamada a função do bot. Powerups não visíveis não são incluidos
-- para não dar uma vantagem aos bots.
pwToString :: [[Tile]] -> PW -> String -- não introduzir powerup se não for visível
pwToString mapa (pwType,(c,l))
    | isBrick (c,l) mapa = ""
    | pwType == BombPW = "+ " ++ show c ++ " " ++ show l
    | pwType == FlamePW = "! " ++ show c ++ " " ++ show l

-- | Função que contrói um /String/ que representa uma bomba.
bombToString :: Bomb -> String
bombToString ((c,l),player,size,ticks) = "* " ++ show c ++ " " ++ show l ++ " " ++
                                         show player ++ " " ++
                                         show size ++ " " ++
                                         show ticks

-- | Função que contrói um /String/ que representa um jogador.
playerToString :: Player -> String
playerToString (num,(c,l),(b,f),_,_) = show num ++ " " ++ show c ++ " " ++ show l ++ " " ++
                                       (replicate b '+') ++ (replicate f '!')

-- | Função que converte o mapa utilizado na tarefa 5 num mapa para ser passado
-- como argumento das funções dos bots, não sendo incluídos os powerups não
-- visíveis.
mkStringMap :: [[Tile]] -> [PW] -> [Bomb] -> [Player] -> [String]
mkStringMap mapa pws bombs players =
    let grid = map (map (tileToChar)) mapa
        pwLines = filter (/="") $ map (pwToString mapa)  pws
        bombLines = map (bombToString) bombs
        playerLines = map (playerToString) players
    in grid ++ pwLines ++ bombLines ++ playerLines

-- | Função que altera o estado do jogo quando acontece um evento.
handleEvent :: Event -> Estado -> Estado
handleEvent e s@(E mainState _ _ _ _ _ _ _) =
    case mainState of
        Game -> handleGameEvents e s []
        Menu buttons seleted -> handleMenuEvents e s

-- | Função que lida com os eventos quando se está no menu principal.
handleMenuEvents :: Event -> Estado -> Estado
handleMenuEvents e s@(E (Menu buttons selected) _ _ _ _ _ _ _) =
    case e of
        EventKey (SpecialKey KeyUp) Down _ _ -> if selected > 0 then (s {mainState = (Menu buttons (selected-1))}) else s
        EventKey (SpecialKey KeyDown) Down _ _ -> if selected < (length buttons) - 1 then (s {mainState = (Menu buttons (selected+1))}) else s
        EventKey (Char 'x') Down _ _ -> (snd (buttons !! selected)) s
        _ -> s

-- | Função que lida com os eventos quando o jogo está a decorrer.
handleGameEvents :: Event -> Estado -> [Player] -> Estado
handleGameEvents e (s@(E mainState pics mapa pws bombs (player@(n,pos@(c,l),ppws,motion,keys@(Left (kUp,kDown,kLeft,kRight,kBomb))):t) blasts time)) players =
    case e of
        EventKey key Down _ _ -> handleGameEvents e handledDown handledPlayers1
            where (handledDown,handledPlayers1) = handleButtonDown s key players
        _ -> handleGameEvents e (s {players=t}) (player:players)
handleGameEvents e s@(E _ _ _ _ _ (h:t) _ _) players = handleGameEvents e (s {players=t}) (h:players)
handleGameEvents _ s@(E _ _ _ _ _ [] _ _) players = s {players=players}

-- | Função que lida com os eventos de teclas pressionadas.
handleButtonDown :: Estado -> Key -> [Player] -> (Estado,[Player]) -- esta função serve apenas para organizar o código melhor
handleButtonDown s@(E mainState pics mapa pws bombs [] blasts time) key players = ((s {players=players}),[])
handleButtonDown s@(E mainState pics mapa pws bombs (player@(n,pos@(c,l),ppws@(pBombs,pFlames),motion,keys@(Left (kUp,kDown,kLeft,kRight,kBomb))):t) blasts time) key players
    | key == kUp    && isEmpty (c,l-1) mapa = ((s {players=t}),((n,pos,ppws,MUp,keys):players))
    | key == kDown  && isEmpty (c,l+1) mapa = ((s {players=t}),((n,pos,ppws,MDown,keys):players))
    | key == kRight && isEmpty (c+1,l) mapa = ((s {players=t}),((n,pos,ppws,MRight,keys):players))
    | key == kLeft  && isEmpty (c-1,l) mapa = ((s {players=t}),((n,pos,ppws,MLeft,keys):players))
    | key == kBomb  && (not (isBomb (c,l) bombs)) && (countBombs bombs n) < (pBombs+1) = ((s {bombs=(((c,l),n,pFlames,10):bombs),players=t}),(player:players))
    | otherwise = ((s {players=t}),(player:players))

-- | Função que conta as bombas postas por um dado jogador.
countBombs :: [Bomb] -> Int -> Int
countBombs bombList player = length $ filter (\(_,n,_,_) -> n == player) bombList

--------------------------------------------------------------------------------

-- | Função que lida com os comandos dados pelos bots.
handleBotCmd :: Estado -> [Player] -> (Estado,[Player])
handleBotCmd s@(E mainState pics mapa pws bombs ps@(player@(n,pos@(c,l),ppws@(pBombs,pFlames),motion,(Right botFunc)):t) blasts time) players
    | cmd == Just 'U' && isEmpty (c,l-1) mapa = handleBotCmd (s {players=t}) ((n,pos,ppws,MUp,Right botFunc):players)
    | cmd == Just 'D' && isEmpty (c,l+1) mapa = handleBotCmd (s {players=t}) ((n,pos,ppws,MDown,Right botFunc):players)
    | cmd == Just 'R' && isEmpty (c+1,l) mapa = handleBotCmd (s {players=t}) ((n,pos,ppws,MRight,Right botFunc):players)
    | cmd == Just 'L' && isEmpty (c-1,l) mapa = handleBotCmd (s {players=t}) ((n,pos,ppws,MLeft,Right botFunc):players)
    | cmd == Just 'B' && (not (isBomb (c,l) bombs)) && (countBombs bombs n) < (pBombs+1) = handleBotCmd (s {bombs=(((c,l),n,pFlames,10):bombs),players=t}) (player:players)
    | otherwise = handleBotCmd (s {players=t}) (player:players)
    where cmd = (botFunc (mkStringMap mapa pws bombs (players++ps)) n (truncate (time/tickLength)))
handleBotCmd s@(E mainState pics mapa pws bombs (h:t) blasts time) players = handleBotCmd (s {players=t}) (h:players)
handleBotCmd s@(E mainState pics mapa pws bombs [] blasts time) players = ((s {players=players}),[])

-- | Função que altera o estado do jogo quando o tempo avança @n@ segundos.
handleTime :: Float -> Estado -> Estado
handleTime f s@(E mainState pics mapa pws bombs players blasts time) =
    if mainState == Game && time > 0 && length players > 1
    then if (truncate (time/tickLength) - 1 == truncate ((time-f)/tickLength))
         then let (E _ _ mapa2 pws2 bombs2 players2 _ _) = fst $ handleBotCmd s []
                  (mapa3,pws3,bombs3,players3) =
                      if truncate (time/tickLength) <= calcTicks (calcDim mapa)
                      then spiral (mapa2,pws2,bombs2,players2) (calcTicks (calcDim mapa) - truncate (time/tickLength) + 1) 1 (1,1) (1,0)
                      else (mapa2,pws2,bombs2,players2)
                  (exploding,bombs4) = takeExploding bombs3
                  bombs5 = tickBombs bombs4
                  (pws4,players4) = movePlayers mapa3 pws3 players3 f
                  (mapa4,pws5,bombs6,players5,blasts2) = explode mapa3 pws4 exploding bombs5 players4 []
                  (mapa5,pws6) = removeExploded mapa4 pws5
              in (E mainState pics mapa5 pws6 bombs6 players5 blasts2 (time-f))
        else (s {time=time-f})
    else s

-- | Função que, dada uma dimensão, calcula quantos ticks é preciso faltarem
-- até o mapa começar a ser preenchido pelas pedras da espiral.
calcTicks :: Int -> Int
calcTicks n = (n-2)^2

-- | Função que calcula a dimensão de um mapa.
calcDim :: [[Tile]] -> Int
calcDim (h:_) = length h

-- | Função que, dado um mapa, retorna um par de uma lista de bombas que
-- explodem no instante atual, e a lista das restantes bombas.
takeExploding :: [Bomb] -> ([Bomb], [Bomb])
takeExploding [] = ([],[])
takeExploding (b@(pos,player,size,ticks):t) =
    if ticks == 1
    then ((pos,player,size,0):exploding,rest)
    else (exploding,b:rest)
    where (exploding,rest) = takeExploding t

-- | Função que reduz o contador das bombas em um.
tickBombs :: [Bomb] -> [Bomb]
tickBombs [] = []
tickBombs ((pos,x,y,tick):t) = (pos,x, y, (tick-1)) : tickBombs t

-- | Função que implementa a espiral.
spiral :: ([[Tile]],[PW],[Bomb],[Player]) -> Int -> Int -> Pos -> (Int,Int) -> ([[Tile]],[PW],[Bomb],[Player])
spiral map 0 _ _ _ = map
spiral map@(mapa,pws,bombs,players) blocks border pos@(c,l) movement =
    spiral map_ (blocks-1) border_ pos_ movement_
    where map_ = if (c==(div (calcDim mapa) 2)) && (l==(div (calcDim mapa) 2))
                 then map
                 else ((setPos mapa pos Rock),(rmPW pws pos),rmBomb bombs pos,killPlayers players pos)
          pos_ = (c + fst (movement),l + snd (movement))
          (movement_,border_) = reactToEdge mapa pos movement border

-- | Função que muda a direção de um vetor quando a dada posição de aproxima um
-- dado número de blocos da borda, e aumenta este último número sempre que muda
-- de direção para cima. A funçäo determina em que direção a espiral deverá se
-- mover no próximo tick.
reactToEdge :: [[Tile]] -> Pos -> (Int,Int) -> Int -> ((Int,Int), Int)
reactToEdge mapa pos m@(1,0) border =
    if (fst (pos) + border) == ((calcDim mapa) - 2)
    then ((0,1),border)
    else (m,border)
reactToEdge mapa pos m@(0,1) border =
    if (snd (pos) + border) == ((calcDim mapa) - 2)
    then (((-1),0),border)
    else (m,border)
reactToEdge mapa pos m@((-1),0) border =
    if (fst (pos) - border) == 1
    then ((0,(-1)),(border+1))
    else (m,border)
reactToEdge mapa pos m@(0,(-1)) border =
    if (snd (pos) - border) == 1
    then ((1,0),border)
    else (m,border)

-- | Função que dado um mapa e uma lista de bombas retorna um outro mapa onde
-- se sentem os efeitos das explosões das bombas.
explode :: [[Tile]] -> [PW] -> [Bomb] -> [Bomb] -> [Player] -> [Blast] -> ([[Tile]],[PW],[Bomb],[Player],[Blast])
explode mapa pws [] unexploded players blasts = (mapa,pws,unexploded,players,blasts)
explode mapa pws (h:t) unexploded players blasts = explode mapa_ pws_ t unexploded_ players_ blasts_ 
    where (mapa_,pws_,unexploded_,players_,blasts_) = (explodeBomb mapa pws h unexploded players blasts 0) 

-- | Função que explode a bomba na sua própria posição, e nas 4 direções em que
-- a bomba explode.
explodeBomb :: [[Tile]] -> [PW] -> Bomb -> [Bomb] -> [Player] -> [Blast] -> Int -> ([[Tile]],[PW],[Bomb],[Player],[Blast])
explodeBomb mapa pws bomb@(pos@(c,l),_,size,_) unexploded players blasts i =
    if i==0
    then explodeBomb mapa pws bomb unexploded (killPlayers players pos) ((CenterBlast,pos):blasts) (i+1)
    else foldl (explodeBombAux size 1 pos) (mapa,pws,unexploded,players,blasts) [(c,l-i),(c-i,l),(c+i,l),(c,l+i)]

-- | Função que, dado o tamanho da explosão, o tamanho atual, a posição da
-- bomba, um mapa, e a posição atual da explosão, retorna um mapa onde se
-- sentem os efeitos da explosão da bomba apenas na direção atual.
explodeBombAux :: Int -> Int -> Pos -> ([[Tile]],[PW],[Bomb],[Player],[Blast]) -> Pos -> ([[Tile]],[PW],[Bomb],[Player],[Blast])
explodeBombAux size i (x,y) (mapa,pws,bombs,players,blasts) (c,l)
    | isRock (c,l) mapa = (mapa,pws,bombs,players_,blasts)
    | isBrick (c,l) mapa = ((setPos mapa (c,l) RemovedTile),pws,bombs,players_,((calcBlast (x,y) (c,l) True),(c,l)):blasts)
    | isBomb (c,l) bombs =  (mapa,pws,(triggerBomb bombs (c,l)),players_,blasts)
    | isPW (c,l) pws = (mapa,(rmPW pws (c,l)),bombs,players_,((calcBlast (x,y) (c,l) True),(c,l)):blasts)
    | otherwise = if i<=size
                  then explodeBombAux size (i+1) (x,y) (mapa,pws,bombs,players_,((calcBlast (x,y) (c,l) False),(c,l)):blasts) (calcNextPos (x,y) (c,l))
                  else (mapa,pws,bombs,players_,((calcBlast (x,y) (c,l) True),(c,l)):blasts)
    where players_ = killPlayers players (c,l)

-- | Função que determina o tipo de explosão numa dada posição.
calcBlast :: Pos -> Pos -> Bool -> BlastType
calcBlast (x,y) (c,l) isEnd
    | x == c && (not isEnd) = VerticalBlast
    | y == l && (not isEnd) = HorizontalBlast
    | x == c && signum (y-l) == (-1) = LowerBlast
    | x == c && signum (y-l) == 1 = UpperBlast
    | y == l && signum (x-c) == 1 = LeftBlast
    | y == l && signum (x-c) == (-1) = RightBlast
    | otherwise = CenterBlast

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

-- | Função que testa se uma posição é uma posição válida para um mapa de uma
-- dada dimensão.
inMap :: Int -> Pos -> Bool
inMap dim (x,y) = x>=0 && x<dim && y>=0 && y<dim

-- | Função que testa se existe uma bomba numa dada posição.
isBomb :: Pos -> [Bomb] -> Bool
isBomb _ [] = False
isBomb pos ((bPos,_,_,_):t) =
    if pos == bPos
    then True
    else isBomb pos t

-- | Função que testa se existe um powerup numa dada posição.
isPW :: Pos -> [PW] -> Bool
isPW _ [] = False
isPW pos ((_,pwPos):t) =
    if pos == pwPos
    then True
    else isPW pos t

-- | Função que altera um tipo bloco de uma dada posição para outro
-- especificado no terceiro argumento.
setPos :: [[Tile]] -> Pos -> Tile -> [[Tile]]
setPos mapa (c,l) tile = (take l mapa) ++ [setPosAux (mapa !! l) c tile] ++ (drop (l+1) mapa)

-- | Função auxiliar de /setPos/.
setPosAux :: [Tile] -> Int -> Tile -> [Tile]
setPosAux tileLine c tile = (take c tileLine) ++ [tile] ++ (drop (c+1) tileLine)

-- | Função que, caso exista, altera o contador de uma bomba numa dada posição
-- para 1.
triggerBomb :: [Bomb] -> Pos -> [Bomb]
triggerBomb [] _ = []
triggerBomb (bomb@(bPos,p,s,ticks):t) pos =
    if pos == bPos
    then (bPos,p,s,1) : t
    else bomb : triggerBomb t pos

-- | Função que marca um powerup numa dada posição como removido.
rmPW :: [PW] -> Pos -> [PW]
rmPW [] _ = []
rmPW (pw@(_,pwPos):t) pos =
    if pos == pwPos
    then (RemovedPW,pwPos) : t
    else pw : rmPW t pos

-- | Função que acrescenta a contagem de powerups apanhados de um jogador se
-- existir um powerup na sua posição.
pickUpPWs :: [PW] -> Pos -> (Int,Int) -> (Int,Int)
pickUpPWs [] _ pws = pws
pickUpPWs ((pwType,pwPos):t) pos (b,f) =
    if pos == pwPos
    then case pwType of
        BombPW -> (b+1,f)
        FlamePW -> (b,f+1)
    else pickUpPWs t pos (b,f)

-- | Função que, se existir, remove a bomba numa dada posição.
rmBomb :: [Bomb] -> Pos -> [Bomb]
rmBomb [] _ = []
rmBomb (b@(bPos,_,_,_):t) pos =
    if bPos == pos
    then rmBomb t pos
    else b : rmBomb t pos

-- | Função que retira os jogadores que se encontram numa dada posição.
killPlayers :: [Player] -> Pos -> [Player]
killPlayers [] _ = []
killPlayers (player@(_,pPos,_,_,_):t) pos =
    if pos == pPos
    then killPlayers t pos
    else player : killPlayers t pos

-- | Função que remove os blocos e powerups marcados como para remover.
removeExploded :: [[Tile]] -> [PW] -> ([[Tile]],[PW])
removeExploded mapa pws = 
    let rMapa = map (map (\x -> if x == RemovedTile then Empty else x)) mapa
        rPws = filter (\x -> (fst x)/=RemovedPW) pws
    in (rMapa,rPws)

-- | Função que move os jogadores.
movePlayers :: [[Tile]] -> [PW] -> [Player] -> Float -> ([PW],[Player])
movePlayers _ pws [] _ = (pws,[])
movePlayers mapa pws (player@(n,(c,l),(bombs,flames),motion,keys):t) f
    | motion == StillD = (pws2,player:players)
    | motion == MUp    && isEmpty (c,l-1) mapa = ((intersect (rmPW pws (c,l-1)) pws2),(n,(c,l-1),pickUpPWs pws (c,l-1) (bombs,flames),StillU,keys):players)
    | motion == MDown  && isEmpty (c,l+1) mapa = ((intersect (rmPW pws (c,l+1)) pws2),(n,(c,l+1),pickUpPWs pws (c,l+1) (bombs,flames),StillD,keys):players)
    | motion == MLeft  && isEmpty (c-1,l) mapa = ((intersect (rmPW pws (c-1,l)) pws2),(n,(c-1,l),pickUpPWs pws (c-1,l) (bombs,flames),StillL,keys):players)
    | motion == MRight && isEmpty (c+1,l) mapa = ((intersect (rmPW pws (c+1,l)) pws2),(n,(c+1,l),pickUpPWs pws (c+1,l) (bombs,flames),StillR,keys):players)
    | otherwise = (pws2,(stopPlayer player):players)
    where (pws2,players) = movePlayers mapa pws t f

-- | Função que para um jogador, tendo em conta a direção do seu movimento para
-- depois desenhar corretamente o jogador.
stopPlayer :: Player -> Player
stopPlayer (num,pos,pws,MUp,keys) = (num,pos,pws,StillU,keys)
stopPlayer (num,pos,pws,MDown,keys) = (num,pos,pws,StillD,keys)
stopPlayer (num,pos,pws,MLeft,keys) = (num,pos,pws,StillL,keys)
stopPlayer (num,pos,pws,MRight,keys) = (num,pos,pws,StillR,keys)
stopPlayer player = player

{-|
Module: Main
Description: Módulo descrito em Haskell que
             faz o encode e decode das funções
             definidas nas tarefas anteriores
Copyrigth: Alexandre Mendonça Pinho <a82441@alunos.uminho.pt>;
           Joel Filipe Esteves Gama <a82202@alunos.uminho.pt>;
 -}
module Main where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Environment
import Data.Char

-- | /Int/ que representa um bit por 0 ou 1.
type Bit = Int
-- | Lista de /Bits/, cada elemento representando um bit.
type Bits = [Bit]

-- | Função que codifica o estado inicial do mapa.
encode :: [String] -> String
encode l = let (map, extras) = splitMap l
               (pws, bombs, players) = splitExtras extras
               dim = length $ map !! 0
               rand = dropPredefined map dim
               mapBits = mkMapBits $ concat $ rand
               extraBits = (intToBits (countPWs l) 9) ++ mkPWsBits pws ++
                           (intToBits (countBombs l) 7) ++ mkBombsBits bombs ++
                           countPlayers l ++ mkPlayersBits players
               chs = mkChars ((intToBits dim 7) ++ mapBits ++ extraBits)
           in chs

-- | Função que separa as linhas do mapa que contêm as celulas das que contêm
-- os Power Ups, as Bombas, e os Jogadores
splitMap :: [String] -> ([String],[String])
splitMap [] = ([],[])
splitMap (h:t) = if head h /= '#'
                 then ([],h:t)
                 else (h:fst s, snd s)
                     where s = splitMap t

-- | Função que separa num /tripo/ os Power Ups, as Bombas, e os Jogadores,
-- respetivamente.
splitExtras :: [String] -> ([String],[String],[String])
splitExtras [] = ([],[],[])
splitExtras (h:t)
    | head h == '+' || head h == '!' = (h:(fst3 r), (snd3 r), (thrd3 r))
    | head h == '*' = ((fst3 r), h:(snd3 r), (thrd3 r))
    | isDigit (head h) = ((fst3 r), (snd3 r), h:(thrd3 r))
        where r = splitExtras t
-- | Função que retorna o primeiro elemento de um /triplo/.
fst3 :: (a,a,a) -> a
fst3 (x,_,_) = x

-- | Função que retorna o segundo elemento de um /triplo/.
snd3 :: (a,a,a) -> a
snd3 (_,x,_) = x

-- | Função que retorna o terceiro elemento de um /triplo/.
thrd3 :: (a,a,a) -> a
thrd3 (_,_,x) = x

-- | Função que retira as células predefinidas de um mapa.
dropPredefined :: [String] -> Int -> [String]
dropPredefined mapa dim = dropPredefinedAux 0 mapa dim

-- | Função auxiliar que, dada uma linha, uma lista de /Strings/ e uma
-- dimensão, retira
dropPredefinedAux :: Int -> [String] -> Int -> [String]
dropPredefinedAux _ [] _ = []
dropPredefinedAux line (h:t) dim
    | line==0 || line==dim-1 = dropPredefinedAux (line+1) t dim
    | line==1 || line==dim-2 = (drop 3 $ take (dim-3) h):dropPredefinedAux (line+1) t dim
    | line==2 || line==dim-3 = (filter (/='#') (drop 2 $ take (dim-2) h)):dropPredefinedAux (line+1) t dim
    | otherwise = (filter (/='#') h):dropPredefinedAux (line+1) t dim

-- | Função que retorna uma lista de /Bits/, com /False/ a representar espaços
-- vazios no mapa, e /True/ tijolos.
mkMapBits :: [Char] -> Bits
mkMapBits [] = []
mkMapBits (' ':t) = 0:mkMapBits t
mkMapBits ('?':t) = 1:mkMapBits t

-- | Função que retorna uma lista de /Bits/, com /False/ a representar a
-- inexistência de Power Ups, e /True/ a existência de Power Ups.
mkPWsBits :: [String] -> Bits
mkPWsBits [] = []
mkPWsBits (h:t) =
    if (h!!0) == '+'
        then (1:(intToBits (read (w!!1)) 7)) ++ ((intToBits (read (w!!2)) 7)) ++ (mkPWsBits t)
        else (0:(intToBits (read (w!!1)) 7)) ++ ((intToBits (read (w!!2)) 7)) ++ (mkPWsBits t)
    where w = words h

-- | Função que retorna uma lista de /Bits/ com as informações acerca das
-- bombas no mapa (posição, jogador que a colocou, raio de explosão e intantes
-- de explosão).
mkBombsBits :: [String] -> Bits
mkBombsBits [] = []
mkBombsBits (h:t) = (intToBits (read (w!!1)) 7) ++ -- coluna
                    (intToBits (read (w!!2)) 7) ++ -- linha
                    (intToBits (read (w!!3)) 2) ++ -- jogador
                    (intToBits (read (w!!4)) 7) ++ -- raio de explosão
                    (intToBits (read (w!!5)) 4) ++ -- instantes de explosão
                    mkBombsBits t
    where w = words h

-- | Função que retorna uma lista de /Bits/ com as informações dos jogadores
-- (posição, número e tipo dos Power Ups que cada um possui).
mkPlayersBits :: [String] -> Bits
mkPlayersBits [] = []
mkPlayersBits (h:t) =
    if length (w) == 3
    then (intToBits (read (w!!1)) 7) ++                        -- coluna
         (intToBits (read (w!!2)) 7) ++                        -- linha
         mkPlayersBits t
    else (intToBits (read (w!!1)) 7) ++
         (intToBits (read (w!!2)) 7) ++
         (intToBits (length (filter (=='+') (w!!3))) 7) ++     -- numero de bombas
         (intToBits (length (filter (=='!') (w!!3))) 7) ++     -- numero de flames
         mkPlayersBits t
    where w = words h

-- | Função que conta o número de Power Ups que cada jogador possui.
countPWs :: [String] -> Int
countPWs [] = 0
countPWs (h:t) = if (h!!0) == '+' || (h!!0) == '!'
                 then 1 + countPWs t
                 else countPWs t

-- | Função que conta o número de bombas que cada jogador tem colocadas no
-- mapa.
countBombs :: [String] -> Int
countBombs [] = 0
countBombs (h:t) = if (h!!0) == '*'
                 then 1 + countBombs t
                 else countBombs t

-- | Função que devolve uma lista com os jogadores existentes no jogo e os
-- Power Ups que cada um possui.
countPlayers :: [String] -> Bits
countPlayers (h:t) = [playerExists 0 (h:t), playerHasPWs 0 (h:t),
                      playerExists 1 (h:t), playerHasPWs 1 (h:t),
                      playerExists 2 (h:t), playerHasPWs 2 (h:t),
                      playerExists 3 (h:t), playerHasPWs 3 (h:t)]

-- | Função que verifica se um dado jogador existe no mapa, devolvendo '1' em
-- caso afirmativo ou '0' em caso negativo.
playerExists :: Int -> [String] -> Int
playerExists _ [] = 0
playerExists x (h:t) = if (h!!0) == intToDigit x
                       then 1
                       else playerExists x t

-- | Função que verifica se um jogador possui Power Ups, devolvendo '1' em caso
-- afirmativo ou '0' em caso negativo.
playerHasPWs :: Int -> [String] -> Int
playerHasPWs _ [] = 0
playerHasPWs x (h:t) = if (h!!0) == intToDigit x && (length w) == 4
                       then 1
                       else playerHasPWs x t
                       where w = words h

-- | Função que recebe uma lista de 8 /Bits/ (1 ou 0) e retorna o /Char/ com o
-- valor do /Byte/.
mkChar :: Bits -> Char
mkChar (b1:b2:b3:b4:b5:b6:b7:b8:t) = chr $ sum $ zipWith (\x y -> y*(2^x)) [x | x <- reverse [0..7]] [b1,b2,b3,b4,b5,b6,b7,b8]

-- | Função que dada uma lista de /Ints/ (1 ou 0), retorna um /String/ cujos
-- caracteres tem valores dos /bytes/ formados ao agrupar a lista em conjuntos
-- de 8. Se restarem elementos da lista (se forem menos de 8), o último
-- /Char/ do output e contruido adicionando zeros (0) à lista.

-- >>> mkChars [0,0,1,1,0,1,0,1,0,0,1,1]
--             '5'
mkChars :: Bits -> String
mkChars [] = []
mkChars l = if length l >= 8
            then mkChar (take 8 l):mkChars (drop 8 l)
            else mkChars (l ++ [0])

-- | Função que converte um número decimal para a sua representação binária,
-- utilizando um número de /bits/ dado como argumento. Os /bits/ tem ordem
-- decrescente de valorizacao.

-- >>> intToBits 5 11
--     [0,1,0,1,1]
--
-- >>> intToBits 6 55
--     [1,1,0,1,1,1]

intToBits :: Int -> Int -> Bits
intToBits x bits = iTBAux x (bits-1)

-- | Função auxiliar de 'intToBits'.
iTBAux :: Int -> Int -> Bits
iTBAux x (-1) = []
iTBAux x y =
    if x-2^y >= 0
    then 1:iTBAux (x-2^y) (y-1)
    else 0:iTBAux x (y-1)

--------------------------------------------------------------------------------

-- | Função que descodifica a versão comprimida do mapa.
decode :: String -> [String]
decode l = let bits = charsToBits l
               (dim, bits2) = xBitsToInt 7 bits
               (mapBits, bits3) = splitAt (getNumCells dim) bits2
               (numPWs, bits4) = xBitsToInt 9 bits3
               (pws, bits5) = splitAt (numPWs*15) bits4
               (numBombs, bits6) = xBitsToInt 7 bits5
               (bombs, bits7) = splitAt (numBombs*27) bits6
               (playerCount, players) = splitAt 8 bits7
           in (mkMap 0 dim mapBits) ++ (mkPWs pws) ++ (mkBombs bombs) ++ (filter (/="") (mkPlayers 0 playerCount players))

-- | Função que, dada a linha, a dimensão e a lista de /Bits/, devolve uma
-- lista de /Strings/ que representam as linhas do mapa.
mkMap :: Int -> Int -> Bits -> [String]
mkMap l dim bits
    | l>=dim     = []
    | l==0       = mkMapCols 0 l dim bits : mkMap (l+1) dim bits
    | l==1       = mkMapCols 0 l dim bits : mkMap (l+1) dim (drop (dim-6) bits)
    | l==2       = mkMapCols 0 l dim bits : mkMap (l+1) dim (drop (dim-4-(div (dim-3) 2)) bits)
    | l==dim-1   = mkMapCols 0 l dim bits : mkMap (l+1) dim bits
    | l==dim-2   = mkMapCols 0 l dim bits : mkMap (l+1) dim (drop (dim-6) bits)
    | l==dim-3   = mkMapCols 0 l dim bits : mkMap (l+1) dim (drop (dim-4-(div (dim-3) 2)) bits)
    | mod l 2==0 = mkMapCols 0 l dim bits : mkMap (l+1) dim (drop (dim-2-(div (dim-3) 2)) bits)
    | mod l 2/=0 = mkMapCols 0 l dim bits : mkMap (l+1) dim (drop (dim-2) bits)

-- | Função que, dada a coluna, a linha, a dimensão e a lista de /Bits/,
-- devolve uma /Strings/ que representa as colunas do mapa.
mkMapCols :: Int -> Int -> Int -> Bits -> String
mkMapCols c l dim bits
    | c>=dim                                     = []
    | l == 0 || l == (dim-1)             = '#' : mkMapCols (c+1) l dim bits
    | c == 0 || c == (dim-1)                     = '#' : mkMapCols (c+1) l dim bits
    | mod l 2 == 0 && mod c 2 == 0           = '#' : mkMapCols (c+1) l dim bits
    | elem (l, c) (normDim dim lista_vazios) = ' ' : mkMapCols (c+1) l dim bits
    | otherwise                                  = mkBlock (bits !! 0) : mkMapCols (c+1) l dim (tail bits)

-- | Lista de coordenadas das posições do mapa que ficarão obrigatoriamente
-- vazias. Números negativos significa que se conta a posição a partir do
-- fim da linha ou coluna.
lista_vazios = [(x,y) | x <- [(-2),(-1),1,2], y <- [(-2),(-1),1,2], (abs x /=
2) || (abs y /= 2)]

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

-- | Função que dado '0' o converte num espaço ou dado um '1' o converte num
-- Power Up.
mkBlock :: Int -> Char
mkBlock 0 = ' '
mkBlock 1 = '?'

-- | Função que dada uma lista de /Ints/ retorna uma lista de /Strings/, cada
-- um representando um Power Up. A função processa a lista de /Ints/ de 15
-- em 15, pois esse é o número de /bits/ necessário para codificar cada Power
-- Up.
mkPWs :: Bits -> [String]
mkPWs [] = []
mkPWs bits = mkPW pw : mkPWs pws
    where (pw,pws) = splitAt 15 bits

-- | Função que dada uma lista de 15 /Ints/ retorna um /String/ que representa
-- um Power Up.
mkPW :: Bits -> String
mkPW (h:t) = if h==1
             then "+ " ++ (show . bitsToInt) c ++ " " ++ (show . bitsToInt) l
             else "! " ++ (show . bitsToInt) c ++ " " ++ (show . bitsToInt) l
    where (c,l) = splitAt 7 t

-- | Função que dada uma lista de /Ints/ retorna uma lista de /Strings/, cada
-- um representando um Power Up. A função processa a lista de /Ints/ de 27 em
-- 27, pois esse é o número de /bits/ necessário para codificar cada Power
-- Up.
mkBombs :: Bits -> [String]
mkBombs [] = []
mkBombs bits = mkBomb b : mkBombs bs
    where (b,bs) = splitAt 27 bits

-- | Função que dada uma lista de 27 /Ints/ retorna um /String/ que representa
-- um Power Up.
mkBomb :: Bits -> String
mkBomb bits = let (c, bits2) = xBitsToInt 7 bits
                  (l, bits3) = xBitsToInt 7 bits2
                  (player, bits4) = xBitsToInt 2 bits3
                  (flameSize, time) = xBitsToInt 7 bits4
              in "* " ++ show c ++ " " ++ show l ++ " " ++
                 show player ++ " " ++ show flameSize ++ " " ++ (show . bitsToInt) time

-- | Função que dado um número (número do jogador - 0 como valor de
-- acumulação), e duas listas de /Ints/ retorna uma lista de /Strings/, cada um
-- representando um jogador.
mkPlayers :: Int -> Bits -> Bits -> [String]
mkPlayers _ [] _ = []
mkPlayers pnum count bits = player : mkPlayers (pnum+1) (drop 2 count) bits2
    where (player, bits2) = mkPlayer pnum (take 2 count) bits

-- | Função que dado um número (número do jogador - 0 como valor de
-- acumulação), e duas listas de /Ints/ retorna um /tuplo/ cujo primeiro
-- elemento e um /String/ e o segundo uma lista de /Int/, onde o /String/
-- representa um jogador, e a lista de /Ints/ e a lista contendo todos aqueles
-- que não foram necessários para fazer o /String/. O segundo argumento, uma
-- lista de 8 /bits/, codifica se os jogadores existem e se cada um deles tem
-- Power Ups acumulados (nas posicoes 0, 2, 4 e 6, e 1, 3, 5 e 7,
-- respetivamente).
mkPlayer :: Int -> Bits -> Bits -> (String, Bits)
mkPlayer _ (0:t) bits = ("",bits)
mkPlayer player b@(1:0:t) bits = let (c, bits2) = xBitsToInt 7 bits
                                     (l, bits3) = xBitsToInt 7 bits2
                                 in (show player ++ " " ++ show c ++ " " ++ show l, bits3)
mkPlayer player b@(1:1:t) bits = let (c, bits2) = xBitsToInt 7 bits
                                     (l, bits3) = xBitsToInt 7 bits2
                                     (bombs, bits4) = xBitsToInt 7 bits3
                                     (flames, bits5) = xBitsToInt 7 bits4
                                 in (show player ++ " " ++ show c ++ " " ++ show l ++ " " ++ replicate bombs '+' ++ replicate flames '!', bits5)

-- | Função que recebe um /Char/ e retorna a sua ordem no formato de /Bits/.
charToBits :: Char -> Bits
charToBits c = (intToBits (ord c)  8)

-- | Função que recebe uma lista de /Char/ e retorna a sua ordem no formato de
-- /Bits/.
charsToBits :: [Char] -> Bits
charsToBits [] = []
charsToBits (h:t) = charToBits h ++ charsToBits t

-- | Função que dado uma série de Ints (1 ou 0), retorna um tuplo /(Int, Bits)/
-- em que o primeiro elemento é um número feito a partir de um certo número de
-- /Ints/ na lista (determinado pelo utilizador), e uma outra lista de /Ints/
-- que não foram utilizados ao calcular o /Int/.

-- >>> xBitsToInt 3 [1,1,1,0,1,1,0,0]
--                  (7,   [0,1,1,0,0])
xBitsToInt :: Int -> Bits -> (Int, Bits)
xBitsToInt x l = (bitsToInt $ take x l, drop x l)

-- | Função que dada uma lista de /Ints/ (1 ou 0), converte o número em notação
-- binária por eles representado no correspondente número em sistema de base
-- 10.

-- >>> bitsToInt [1,1,1]
--     7
bitsToInt :: Bits -> Int
bitsToInt [] = 0
bitsToInt l@(h:t) =  h*(2^((length l)-1)) + bitsToInt t

-- | Função que dada a dimensão de um mapa, calcula o número de casas não fixas
-- desse mapa.
getNumCells :: Int -> Int
getNumCells d = d^2-4*(d-1)-12-(div (d-3) 2)^2 -- magia


-- | Função main.
main :: IO ()
main = do a <- getArgs
          setLocaleEncoding utf8
          let p = a !! 0
          w <- getContents
          if length a == 1 && length p == 2 && (p=="-e" || p=="-d")
             then if p=="-e" then putStr $ encode $ lines w
                             else putStr $ unlines $ decode w
             else putStrLn "Parâmetros inválidos"


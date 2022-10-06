module Utiles (
        -- Funciones Matrix
        rangos,
        numeroElementosMatriz,
        columnasMatriz,
        diagonalesMatriz,
        valido,
        dentroDelTablero,
        casillasVacias,
        casillasAlrededorFicha,
        buscaPieza,
        movimientosPosibles,
        esEstadoFinal,
        puntuaEstado,
        -- Funciones Normales
        esInt,
        stringToInt,
        listasDePares,
        siguiente,
        marcaDeLaMaquina,
        -- Funciones IO
        nuevaLinea,
        now,
        leeDigito,
        sacaPuntuacionesDeIO
    ) where

import Data.Matrix
import Data.List (nub)
import Data.Char
import Data.Time.Clock
import qualified Data.Functor

import Tipos

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones de Matrix en útiles.
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

rangos :: Matrix a -> (Int,Int)
rangos m = (1, nrows m)

numeroElementosMatriz :: Matrix a -> Int
numeroElementosMatriz m = length $ toList m

columnasMatriz :: Matrix a -> [[a]]
columnasMatriz m = toLists $ transpose m

diagonalesMatriz :: Matrix a -> [[a]]
diagonalesMatriz m = diagonalPMatriz n m:[diagonalSMatriz n m]
    where n = nrows m

diagonalPMatriz :: Int -> Matrix a -> [a]
diagonalPMatriz n m = [m ! (x,x) | x<-[1..n]]

diagonalSMatriz :: Int -> Matrix a -> [a]
diagonalSMatriz n m = [m ! (x,n-x+1) | x<-[1..n]]

valido :: Pos -> Tablero -> Bool
valido pos m = (v==" ") && dentroDelTablero pos m
    where
        v = m ! pos

dentroDelTablero :: Pos -> Tablero -> Bool
dentroDelTablero (i,j) m = (i>=rmin && i<=rmax) && (j>=rmin && j<=rmax)
    where
        (rmin,rmax) = rangos m

casillasVacias :: Tablero -> [Pos]
casillasVacias m = filter (\ c -> (m ! c) == " ") casillas
    where
        (min,max) = rangos m
        casillas = [(i,j) | i<-[min..max], j<-[min..max]]

casillasAlrededorFicha :: Tablero -> Pos -> [Pos]
casillasAlrededorFicha m (f,c) = [(i,j) | i<-[f-1,f,f+1], j<-[c-1,c,c+1], dentroDelTablero (i,j) m]

buscaPieza :: Tablero -> String -> Pos
buscaPieza m pieza = head [(f,c) | f<-[min..max], c<-[min..max], (m ! (f,c)) == pieza]
    where
        (min,max) = rangos m

movimientosPosibles :: Tablero -> Int -> String -> String -> Movimientos
movimientosPosibles estado quienJuega marcaMaquina juego
    | juego == "3enRaya" = movs3enRaya estado marcaMaquina
    | juego == "gato" = movsGato estado marcaMaquina
    | otherwise = otros

esEstadoFinal :: Tablero -> String -> Bool
esEstadoFinal t juego
    | juego == "3enRaya" = fin3enRaya t
    | juego == "gato" = finGato t
    | otherwise = otros

puntuaEstado :: Tablero -> Pos -> String -> Double
puntuaEstado t pos juego
    | juego == "3enRaya" = puntua3enRaya t pos
    | juego == "gato" = puntuaGato t pos
    | otherwise = otros

otros = undefined
{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones normales en útiles.
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

esInt :: String -> Bool
esInt = foldr ((&&) . isDigit) True

stringToInt :: String -> Int
stringToInt [] = 0
stringToInt c@(s:ss)
    | s == '-' = (-1)*stringToInt ss
    | otherwise = (digitToInt s * 10^(length c - 1)) + stringToInt ss

listasDePares :: [a] -> [[a]]
listasDePares [] = []
listasDePares lista = par : listasDePares resto
    where
        par = take 2 lista
        resto = drop 2 lista

siguiente :: Int -> Int
siguiente j = if j == 1 then 2 else 1

marcaDeLaMaquina :: String -> String -> String
marcaDeLaMaquina marca juego
    | juego == "3enRaya" = if marca == "X" then "O" else "X"
    | juego == "gato" = if marca == "R" then "G" else "R"
    | otherwise = " "

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones de IO en útiles.
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

nuevaLinea :: IO()
nuevaLinea = do putStrLn ""

now :: IO Int
now = getCurrentTime Data.Functor.<&> (floor . fromRational . toRational . utctDayTime)

leeDigito :: String -> IO Int
leeDigito c = do
    putStrLn c
    digito <- getLine
    if esInt digito
        then do
            return $ stringToInt digito
        else do
            return 0

sacaPuntuacionesDeIO :: [IO Double] -> IO [Double]
sacaPuntuacionesDeIO [] = return []
sacaPuntuacionesDeIO (p:ps) = do
    np <- p
    nps <- sacaPuntuacionesDeIO ps
    let puntuaciones = np:nps
    return puntuaciones

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones privadas del módulo que son auxiliares de otras.
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

movs3enRaya :: Tablero -> String -> Movimientos
movs3enRaya t marcaMaquina = zip tableros listaVacias
    where
        listaVacias = casillasVacias t
        tableros = map (\pos -> setElem marcaMaquina pos t) listaVacias

fin3enRaya :: Tablero -> Bool
fin3enRaya t = lleno t || hay3EnRaya t

puntua3enRaya :: Tablero -> Pos -> Double
-- puntua3enRaya t pos = if hay3EnRaya t then 10.0 else 0
puntua3enRaya t pos = hay2
    where
        hay3 = if hay3EnRaya t then 10.0 else 0
        corta3 = if corta3EnRaya t pos then hay3 + 7.5 else hay3
        hay2 = if hay2EnRaya t pos then corta3 + 5.0 else corta3

casillasVaciasRaton :: Tablero -> [Pos]
casillasVaciasRaton m = filter (\ c -> (m ! c) == " ") casillasAlrededor
    where
        (min,max) = rangos m
        casillas = [(i,j) | i<-[min..max], j<-[min..max]]
        (f,c) = head $ filter (\pos -> (m ! pos) == "R") casillas
        casillasAlrededor = casillasAlrededorFicha m (f,c)

casillasVaciasGatos :: Tablero -> [Pos]
casillasVaciasGatos m = filter (\ c -> (m ! c) == " ") casillasAlrededor
    where
        (min,max) = rangos m
        casillas = [(i,j) | i<-[min..max], j<-[min..max]]
        casillasGatos = filter (\pos -> (m ! pos) `elem` ["G1","G3","G5","G7"]) casillas
        casillasAlrededor = concatMap (casillasValidasGatos m) casillasGatos

movsGato :: Tablero -> String -> Movimientos
movsGato t marca
    | marca == "R" = nub $ map (mueveRaton t) (casillasVaciasRaton t)
    | otherwise = nub $ mueveGato t (casillasVaciasGatos t)

finGato :: Tablero -> Bool
finGato t = ratonEncerrado t || ratonEscapado t

ratonEncerrado :: Tablero -> Bool
ratonEncerrado t = null (casillasVaciasRaton t)

ratonEscapado :: Tablero -> Bool
ratonEscapado t = filaRaton >= filaGato
    where
        (rmen, rmay) = rangos t
        posiciones = [(i,j) | i<-[rmen..rmay], j<-[rmen..rmay]]
        filaRaton = fst $ head $ filter (\p -> (t ! p) == "R") posiciones
        filaGato = maximum $ map fst $ filter (\p -> (t ! p) `elem` ["G1","G3","G5","G7"]) posiciones

puntuaGato :: Tablero -> Pos -> Double
puntuaGato t pos
    | finGato t = 10.0
    -- | hayHueco t || casiEncerrado = -10.0
    | otherwise = 0.0

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones privadas del módulo que son auxiliares de las auxiliares y, 
a partir de aquí, auxiliares de forma recursiva si fuera necesario.
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

lleno :: Tablero -> Bool
lleno t = null (casillasVacias t)

hay3EnRaya :: Tablero -> Bool
hay3EnRaya t = not (null fsx) || not (null csx) || not (null dsx)
    where
        fs = toLists t
        cs = columnasMatriz t
        ds = diagonalesMatriz t
        tripleX = takeWhile (=="X")
        tripleO = takeWhile (=="O")
        fsx = filter (\f -> length (tripleX f) == 3 || length (tripleO f) == 3) fs
        csx = filter (\c -> length (tripleX c) == 3 || length (tripleO c) == 3) cs
        dsx = filter (\d -> length (tripleX d) == 3 || length (tripleO d) == 3) ds

corta3EnRaya :: Tablero -> Pos -> Bool
corta3EnRaya t pos = not (null fsx) || not (null csx) || not (null dsx)
    where
        fs = toLists t
        cs = columnasMatriz t
        ds = diagonalesMatriz t
        marcaMaq = t ! pos
        marcaHum = marcaDeLaMaquina marcaMaq "3enRaya"
        fsx = filter (\f -> elem marcaMaq f && 2 == length [x | x<-f, x==marcaHum]) fs
        csx = filter (\c -> elem marcaMaq c && 2 == length [x | x<-c, x==marcaHum]) cs
        dsx = filter (\d -> elem marcaMaq d && 2 == length [x | x<-d, x==marcaHum]) ds

hay2EnRaya :: Tablero -> Pos -> Bool
hay2EnRaya t pos = not (null fsx) || not (null csx) || not (null dsx)
    where
        fs = toLists t
        cs = columnasMatriz t
        ds = diagonalesMatriz t
        marcaMaq = t ! pos
        marcaHum = marcaDeLaMaquina marcaMaq "3enRaya"
        fsx = filter (\f -> notElem marcaHum f && 2 == length [x | x<-f, x==marcaMaq]) fs
        csx = filter (\c -> notElem marcaHum c && 2 == length [x | x<-c, x==marcaMaq]) cs
        dsx = filter (\d -> notElem marcaHum d && 2 == length [x | x<-d, x==marcaMaq]) ds

casillasValidasGatos :: Tablero -> Pos -> [Pos]
casillasValidasGatos m pos@(f,c) = filter (\(i,j) -> i < f) casillasAlrededor
    where
        casillasAlrededor = casillasAlrededorFicha m pos

mueveRaton :: Tablero -> Pos -> Movimiento
mueveRaton t pos
    | length estaAlrededor == 1 = (intercambiaPieza t "R" pos (head estaAlrededor), pos)
    | otherwise = (t,pos)
        where
            validas = casillasAlrededorFicha t pos
            estaAlrededor = filter (\p -> (t ! p) == "R") validas

mueveGato :: Tablero -> [Pos] -> Movimientos
mueveGato _ [] = []
mueveGato t (p:ps)
    | length gatosAlrededor == 1 = (intercambiaPieza t (t ! head gatosAlrededor) p (head gatosAlrededor), p) : mueveGato t ps
    | length gatosAlrededor > 1 = intercambia2piezas t gatosAlrededor p ++ mueveGato t ps
    | otherwise = (t,p) : mueveGato t ps
        where
            validas = casillasAlrededorFicha t p
            gatosAlrededor = filter (\v -> (t ! v)`elem` ["G1","G3","G5","G7"]) validas

intercambiaPieza :: Tablero -> String -> Pos -> Pos -> Tablero
intercambiaPieza t pieza posNueva posAntigua = setElem pieza posNueva $ setElem " " posAntigua t

intercambia2piezas :: Tablero -> [Pos] -> Pos -> Movimientos
intercambia2piezas t gatosAlrededor pos = [(intercambiaPieza t (t ! g) pos g, pos) | g<-gatosAlrededor]

{- hayHueco :: Tablero -> Bool
hayHueco t  -}

-- casiEncerrado :: Tablero -> Bool
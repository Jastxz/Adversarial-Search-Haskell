module Utiles (
        -- Funciones Matrix
        rangos,
        numeroElementosMatriz,
        columnasMatriz,
        diagonalesMatriz,
        valido,
        dentroDelTablero,
        casillaVacia,
        casillasVacias,
        casillasAlrededorFicha,
        casillasSegundoNivel,
        buscaPieza,
        intercambiaPieza,
        eliminaPieza,
        movimientosPosibles,
        esEstadoFinal,
        puntuaEstado,
        -- Funciones Normales
        cabeza,
        esInt,
        stringToInt,
        listasDePares,
        siguiente,
        marcaDeLaMaquina,
        distanciaEuclidea,
        intercambia,
        introduce,
        elimina,
        eliminaElemento,
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

casillaVacia :: Tablero -> Pos -> Bool
casillaVacia m p
    | espacio || nula = True
    | otherwise = False
        where
            espacio = (m ! p) == " "
            nula = (m ! p) == ""

casillasVacias :: Tablero -> [Pos]
casillasVacias m = filter (casillaVacia m) casillas
    where
        (min,max) = rangos m
        casillas = [(i,j) | i<-[min..max], j<-[min..max]]

casillasAlrededorFicha :: Tablero -> Pos -> [Pos]
casillasAlrededorFicha m (f,c) = [(i,j) | i<-filas, j<-columnas, auxiliarCasillas m (i,j) && (i,j) `notElem` exluidas]
    where
        filas = [f-1,f,f+1]
        columnas = [c-1,c,c+1]
        exluidas = [(f-1,c),(f+1,c),(f,c-1),(f,c+1)]

-- Aux
auxiliarCasillas :: Tablero -> Pos -> Bool
auxiliarCasillas m pos = dentroDelTablero pos m && casillaVacia m pos

casillasSegundoNivel :: Tablero -> Pos -> [Pos]
casillasSegundoNivel m (f,c) = [(i,j) | i<-filas, j<-columnas, auxiliarCasillas m (i,j) && (i,j) `notElem` exluidas]
    where
        filas = [f-2,f,f+2]
        columnas = [c-2,c,c+2]
        exluidas = [(f-2,c),(f+2,c),(f,c-2),(f,c+2)]

buscaPieza :: Tablero -> String -> Pos
buscaPieza m pieza
    | null listaPiezas = (1,1)
    | otherwise = cabeza "buscaPieza" listaPiezas
    where
        (min,max) = rangos m
        listaPiezas = [(f,c) | f<-[min..max], c<-[min..max], (m ! (f,c)) == pieza]

intercambiaPieza :: Tablero -> String -> Pos -> Pos -> Tablero
intercambiaPieza t pieza posNueva posAntigua = setElem pieza posNueva $ setElem " " posAntigua t

eliminaPieza :: Tablero -> Pos -> Tablero
eliminaPieza t p = setElem " " p t

movimientosPosibles :: Tablero -> Int -> String -> String -> Movimientos
movimientosPosibles estado quienJuega marcaMaquina juego
    | juego == "3enRaya" = movs3enRaya estado marcaMaquina
    | juego == "gato" = movsGato estado marcaMaquina
    | otherwise = error $ "Error en la función movimientosPosibles, el juego especificado no existe. Juego: " ++ show juego

esEstadoFinal :: Tablero -> String -> Bool
esEstadoFinal t juego
    | juego == "3enRaya" = fin3enRaya t
    | juego == "gato" = finGato t
    | otherwise = False

puntuaEstado :: Tablero -> Pos -> String -> Double
puntuaEstado t pos juego
    | juego == "3enRaya" = puntua3enRaya t pos
    | juego == "gato" = puntuaGato t pos
    | otherwise = 0.0

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones normales en útiles.
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

cabeza :: String -> [a] -> a
cabeza funcion lista
    | null lista = error $ "La lista pasada en la funcion " ++ funcion ++ " esta vacia."
    | otherwise = head lista

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

distanciaEuclidea :: Float -> Float -> Float
distanciaEuclidea a b = sqrt $ (a - b)**2

intercambia :: [a] -> a -> Int -> [a]
intercambia (x:xs) a p
    | p == 0 = a : xs
    | null xs = [a]
    | otherwise = x : intercambia xs a (p-1)

introduce :: [a] -> a -> Int -> [a]
introduce (x:xs) a p
    | p == 0 = x : a : xs
    | null xs = x : [a]
    | otherwise = x : introduce xs a (p-1)

elimina :: [a] -> Int -> [a]
elimina (x:xs) p
    | p == 0 = xs
    | null xs = [x]
    | otherwise = x : elimina xs (p-1)

eliminaElemento :: Eq a => [a] -> a -> [a]
eliminaElemento (x:xs) a
    | x == a = xs
    | null xs = [x]
    | otherwise = x : eliminaElemento xs a

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
puntua3enRaya t pos = hay3
    where
        hay2 = if hay2EnRaya t pos then 5.0 else 0.0
        corta3 = if corta3EnRaya t pos then 7.5 else hay2
        hay3 = if hay3EnRaya t then 10.0 else corta3

casillasVaciasRaton :: Tablero -> Pos -> [Pos]
casillasVaciasRaton m posRaton = filter (\ c -> (m ! c) == " ") casillasAlrededor
    where
        casillasAlrededor = casillasAlrededorFicha m posRaton

movsGato :: Tablero -> String -> Movimientos
movsGato t marca
    | marca == "R" = nub $ map (\pos -> (intercambiaPieza t "R" pos posPieza, pos)) (casillasVaciasRaton t posPieza)
    | otherwise = nub $ mueveGato t posGatos
        where
            posPieza = buscaPieza t marca
            posGatos = [buscaPieza t m | m<-nombresGatos]

finGato :: Tablero -> Bool
finGato t = ratonEncerrado t posRaton || ratonEscapado t posRaton posGatos
    where
        posRaton = buscaPieza t "R"
        posGatos = [buscaPieza t m | m<-nombresGatos]

ratonEncerrado :: Tablero -> Pos -> Bool
ratonEncerrado t pos = null (casillasVaciasRaton t pos)

ratonEscapado :: Tablero -> Pos -> [Pos] -> Bool
ratonEscapado t raton gatos = filaRaton >= filaGato
    where
        filaRaton = fst raton
        filaGato = maximum $ map fst gatos

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

posicionesInicialesGatos = [(8,2),(8,4),(8,6),(8,8)]
nombresGatos = ["G2","G4","G6","G8"]

casillasValidasGatos :: Tablero -> Pos -> [Pos]
casillasValidasGatos m pos@(f,c) = filter (\(i,j) -> i < f) casillasAlrededor
    where
        casillasAlrededor = casillasAlrededorFicha m pos

mueveGato :: Tablero -> [Pos] -> Movimientos
mueveGato _ [] = []
mueveGato t (g:gs) = movimientosDelGato ++ mueveGato t gs
        where
            validas = casillasValidasGatos t g
            nombre = t ! g
            movimientosDelGato = map (\v -> (intercambiaPieza t nombre v g, v)) validas

{- hayHueco :: Tablero -> Bool
hayHueco t  -}

-- casiEncerrado :: Tablero -> Bool
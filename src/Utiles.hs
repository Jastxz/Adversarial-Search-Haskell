module Utiles (
        -- Funciones Matrix
        rangos,
        numeroElementosMatriz,
        columnasMatriz,
        diagonalesMatriz,
        valido,
        dentroDelTablero,
        casillasVacias,
        movimientosPosibles,
        esEstadoFinal,
        puntuaEstado,
        -- Funciones Básicas
        esInt,
        stringToInt,
        siguiente,
        -- Funciones IO
        nuevaLinea,
        now,
        leeDigito,
        sacaPuntuacionesDeIO
    ) where

import Data.Matrix
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
valido (i,j) m = (v==" ") && (i>=rmin && i<=rmax) && (j>=rmin && j<=rmax)
    where v = m ! (i,j)
          (rmin,rmax) = rangos m

dentroDelTablero :: Pos -> Tablero -> Bool
dentroDelTablero (i,j) m = (i>=rmin && i<=rmax) && (j>=rmin && j<=rmax)
    where v = m ! (i,j)
          (rmin,rmax) = rangos m

casillasVacias :: Tablero -> [Pos]
casillasVacias m = filter (\ c -> (m ! c) == " ") casillas
    where
        (min,max) = rangos m
        casillas = [(i,j) | i<-[min..max], j<-[min..max]]

movimientosPosibles :: Tablero -> String -> Int -> Movimientos
movimientosPosibles estado juego quienJuega
    | juego == "3enRaya" = movs3enRaya estado quienJuega
    | otherwise = otros

esEstadoFinal :: Tablero -> String -> Bool
esEstadoFinal t juego
    | juego == "3enRaya" = fin3enRaya t
    | otherwise = otros

puntuaEstado :: Tablero -> Pos -> String -> Double
puntuaEstado t pos juego
    | juego == "3enRaya" = puntua3enRaya t pos
    | otherwise = otros

otros = undefined
{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones que no son Matrix ni IO en útiles.
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

esInt :: String -> Bool
esInt = foldr ((&&) . isDigit) True

stringToInt :: String -> Int
stringToInt [] = 0
stringToInt c@(s:ss)
    | s == '-' = (-1)*stringToInt ss
    | otherwise = (digitToInt s * 10^(length c - 1)) + stringToInt ss

siguiente :: Int -> Int
siguiente j = if j == 1 then 2 else 1

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

movs3enRaya :: Tablero -> Int -> Movimientos
movs3enRaya t quienJuega
    | quienJuega == 1 = map (\pos -> (setElem "X" pos t, pos)) listaVacias
    | otherwise = map (\pos -> (setElem "O" pos t, pos)) listaVacias
        where
            listaVacias = casillasVacias t

fin3enRaya :: Tablero -> Bool
fin3enRaya t = lleno t || hay3EnRaya t

puntua3enRaya :: Tablero -> Pos -> Double
puntua3enRaya t pos
    | hay3EnRaya t = 2.0
    | hay2EnRaya t pos = 1.0
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
        tripleX = ["X","X","X"]
        tripleO = ["O","O","O"]
        fsx = filter (\f -> f == tripleX || f == tripleO) fs
        csx = filter (\c -> c == tripleX || c == tripleO) cs
        dsx = filter (\d -> d == tripleX || d == tripleO) ds

hay2EnRaya :: Tablero -> Pos -> Bool
hay2EnRaya t pos@(f,c) = or [marca == m | m<-marcasAlrededores]
    where
        alrededores = [a | a<-zip [f-1..f+1] [c-1..c+1], dentroDelTablero a t && a /= pos]
        marca = getElem f c t
        marcasAlrededores = map (\(i,j) -> getElem i j t) alrededores
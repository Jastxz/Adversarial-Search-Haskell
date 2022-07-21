module Utiles (
        -- Funciones Matrix
        rangos,
        numeroElementosMatriz,
        columnasMatriz,
        diagonalesMatriz,
        valido,
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

valido :: (Int,Int) -> Tablero -> Bool
valido (i,j) m = (v==" ") && (i>=rmin && i<=rmax) && (j>=rmin && j<=rmax)
    where v = m ! (i,j)
          (rmin,rmax) = rangos m

casillasVacias :: Tablero -> [(Int,Int)]
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

puntuaEstado :: Tablero -> String -> Double
puntuaEstado t juego
    | juego == "3enRaya" = puntua3enRaya t
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
    | quienJuega == 1 = map (\pos -> setElem "X" pos t) listaVacias
    | otherwise = map (\pos -> setElem "O" pos t) listaVacias
        where
            listaVacias = casillasVacias t

fin3enRaya :: Tablero -> Bool
fin3enRaya t = lleno t || hay3EnRaya t

puntua3enRaya :: Tablero -> Double
puntua3enRaya t = if hay3EnRaya t then 1.0 else 0.0

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones privadas del módulo que son auxiliares de las auxiliares y, 
a partir de aquí, auxiliares de forma recursiva si fuera necesario.
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

lleno :: Tablero -> Bool
lleno t = null (casillasVacias t)

hay3EnRaya :: Tablero -> Bool
hay3EnRaya t = or [x==3 | x<-lss]
    where fs = toLists t
          cs = columnasMatriz t
          ds = diagonalesMatriz t
          tripleX = ["X","X","X"]
          tripleO = ["O","O","O"]
          fsx = [x | x<-fs,x==tripleX]
          csx = [x | x<-cs,x==tripleX]
          dsx = [x | x<-ds,x==tripleX]
          fso = [x | x<-fs,x==tripleO]
          cso = [x | x<-cs,x==tripleO]
          dso = [x | x<-ds,x==tripleO]
          ess = fsx++csx++dsx++fso++cso++dso
          lss = [length x | x<-ess]

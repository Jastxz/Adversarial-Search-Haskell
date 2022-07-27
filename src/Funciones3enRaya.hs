module Funciones3enRaya (
        -- Funciones normales
        inicial,
        finalizado,
        lleno,
        hay3EnRaya,
        siguiente,
        -- Funciones escritura
        escribeTablero,
    ) where

import Data.Matrix
import Tipos
import Utiles

inicial :: Movimiento
inicial = (t,pos)
    where
        t = matrix 3 3 $ \(i,j) -> " "
        pos = (0,0)

finalizado :: Tablero -> Bool
finalizado t = lleno t || hay3EnRaya t

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

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones que tienen que ver con la escritura y lectura en consola.
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

escribeTablero :: [String] -> String
escribeTablero [] = []
escribeTablero (xs:xss) = (escribeFila xs++"\n"++guiones++"\n")++escribeTablero xss
    where guiones = escribeGuiones (length xs)

escribeFila :: [Char] -> String
escribeFila [] = []
escribeFila (x:xs) = " "++(x:" |")++escribeFila xs

escribeGuiones :: Int -> String
escribeGuiones n
    | n == 0 = []
    | otherwise = "----"++escribeGuiones (n-1)
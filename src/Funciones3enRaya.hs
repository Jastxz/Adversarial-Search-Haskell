module Funciones3enRaya (
        -- Variables
        Tablero,
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
import Utiles

type Tablero = Matrix Char

maquina = 'O'

inicial :: Tablero
inicial = matrix 3 3 $ \(i,j) -> ' '

finalizado :: Tablero -> Bool
finalizado t = lleno t || hay3EnRaya t

lleno :: Tablero -> Bool
lleno t = length es == suma
    where es = toList t
          xs = [x | x<-es,x=='X']
          os = [o | o<-es,o=='O']
          suma = length xs+length os

hay3EnRaya :: Tablero -> Bool
hay3EnRaya t = or [x==3 | x<-lss]
    where fs = toLists t
          cs = columnasMatriz t
          ds = diagonalesMatriz t
          fsx = [x | x<-fs,x=="XXX"]
          csx = [x | x<-cs,x=="XXX"]
          dsx = [x | x<-ds,x=="XXX"]
          fso = [x | x<-fs,x=="OOO"]
          cso = [x | x<-cs,x=="OOO"]
          dso = [x | x<-ds,x=="OOO"]
          ess = fsx++csx++dsx++fso++cso++dso
          lss = [length x | x<-ess]

siguiente :: Int -> Int
siguiente j = if j == 1 then 2 else 1

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
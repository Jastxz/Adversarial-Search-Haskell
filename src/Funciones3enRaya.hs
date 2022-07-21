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

inicial :: Tablero
inicial = matrix 3 3 $ \(i,j) -> " "

finalizado :: Tablero -> Bool
finalizado t = lleno t || hay3EnRaya t

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
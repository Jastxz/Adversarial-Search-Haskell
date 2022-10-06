module Funciones3enRaya
  ( -- Funciones normales
    inicial,
    finalizado,
    lleno,
    hay3EnRaya,
    -- Funciones escritura
    escribeTablero,
    -- Funciones gráficas
    tamMatriz,
    tamTablero,
    ancho,
    diferenciaParaCasillas,
    ajusteInicial,
    matrizPosiciones,
    distribucionOpciones,
    alturasCasillas,
    infoEstatica,
    alturasEstaticas,
    cambiaOpcion,
    creaTableroConOpciones,
    posXboton,
    pintaMarca,
  )
where

import Data.Matrix
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Tipos
import Utiles
import UtilesGraficos

inicial :: Movimiento
inicial = (t, pos)
  where
    t = matrix 3 3 $ \(i, j) -> " "
    pos = (1, 1)

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
    tripleX = ["X", "X", "X"]
    tripleO = ["O", "O", "O"]
    fsx = filter (\f -> f == tripleX || f == tripleO) fs
    csx = filter (\c -> c == tripleX || c == tripleO) cs
    dsx = filter (\d -> d == tripleX || d == tripleO) ds

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones que tienen que ver con la escritura y lectura en consola.
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

escribeTablero :: [String] -> String
escribeTablero [] = []
escribeTablero (xs : xss) = (escribeFila xs ++ "\n" ++ guiones ++ "\n") ++ escribeTablero xss
  where
    guiones = escribeGuiones (length xs)

escribeFila :: [Char] -> String
escribeFila [] = []
escribeFila (x : xs) = " " ++ (x : " |") ++ escribeFila xs

escribeGuiones :: Int -> String
escribeGuiones n
  | n == 0 = []
  | otherwise = "----" ++ escribeGuiones (n -1)

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones auxiliares para los gráficos
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

tamMatriz :: Float
tamMatriz = 3.0

tamTablero :: Float
tamTablero = 500.0

ancho :: Float
ancho = tamTablero / 2.0

diferenciaParaCasillas :: Float
diferenciaParaCasillas = tamTablero / tamMatriz

ajusteInicial :: Float
ajusteInicial = ancho / tamMatriz

listaPosiciones :: [Point]
listaPosiciones = [(x, y) | y <- [a, a - d .. (- a)], x <- [a, a - d .. (- a)]]
  where
    a = ancho - ajusteInicial
    d = diferenciaParaCasillas

matrizPosiciones :: Matrix Point
matrizPosiciones = matrix t t $ \p -> fst (head (filter (\(num, pos) -> pos == p) relacion))
  where
    t = round tamMatriz
    ps = [(i, j) | i <- [1 .. t], j <- [1 .. t]]
    relacion = zip listaPosiciones ps

distribucionOpciones :: Point
distribucionOpciones = (-15.0, 5.0)

alturasCasillas :: [Float]
alturasCasillas = [ancho, ancho - diferencia .. (- ancho)]
  where
    diferencia = ancho / 5

infoEstatica :: [[String]]
infoEstatica = [dif, turnos, marcas]
  where
    dif = ["Aleatoria", "Mínima", "Fácil", "Normal", "Difícil"]
    turnos = ["Primero", "Segundo"]
    marcas = ["X", "O"]

alturasEstaticas :: [Float]
alturasEstaticas = [dif, turnos, marcas]
  where
    dif = alturasCasillas !! 2
    turnos = alturasCasillas !! 5
    marcas = alturasCasillas !! 8

-- -----------------------------------------------------------------------------------------------------------------------
cambiaOpcion :: Mundo -> Int -> String -> Mundo
cambiaOpcion mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina) nivel opcion
  | nivel == 0 = (mov, juego, traduceDif opcion, prof, marca, turno, seleccionado, esMaquina)
  | nivel == 1 = (mov, juego, dif, prof, marca, traduceTurnos opcion, seleccionado, esMaquina)
  | nivel == 2 = (mov, juego, dif, prof, opcion, turno, seleccionado, esMaquina)
  | otherwise = error "El nivel de opciones especificado para la función cambiaOpción del 3 en raya no existe."

-- Aux
traduceDif :: String -> Int
traduceDif dif
  | dif == "Aleatoria" = 0
  | dif == "Mínima" = 1
  | dif == "Fácil" = 2
  | dif == "Normal" = 3
  | otherwise = 4

-- Aux
traduceTurnos :: String -> Int
traduceTurnos turno
  | turno == "Primero" = 1
  | otherwise = 2

-- -----------------------------------------------------------------------------------------------------------------------

creaTableroConOpciones :: Mundo -> Mundo
creaTableroConOpciones mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina) = (inicial, juego, dif, prof, marca, turno, seleccionado, esMaquina)

posXboton :: Float
posXboton = 200.0

pintaMarca :: Pos -> Tablero -> Matrix Point -> Picture
pintaMarca pos estado posiciones
  | marca == "X" = pintaCruz lugar
  | marca == "O" = pintaCirculo lugar
  | otherwise = blank
  where
    marca = estado ! pos
    lugar = posiciones ! pos

pintaCruz :: Point -> Picture
pintaCruz (x, y) = translate x y cruz
  where
    cruz = color black $ rotate 45.0 lineasJuntas
    lineasJuntas = pictures [vertical, horizontal]
    vertical = line [(0.0, 83.0), (0.0, -83.0)]
    horizontal = line [(83.0, 0.0), (-83.0, 0.0)]

pintaCirculo :: Point -> Picture
pintaCirculo (x, y) = translate x y circulo
  where
    circulo = color black $ circle 83.0
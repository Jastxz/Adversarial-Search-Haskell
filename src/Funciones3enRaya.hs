module Funciones3enRaya
  ( -- Funciones normales
    inicial,
    fin3enRaya,
    lleno,
    hay3EnRaya,
    movs3enRaya,
    puntua3enRaya,
    marcaMaquina3enRaya,
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
    posMenu,
    posOpciones,
    posCargar,
    posCargarJuego,
    posGuardarJuego,
    posVolver,
    posBoton,
    pintaMarca,
  )
where

import Data.Matrix
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Tipos
import Utiles
import UtilesGraficos

-- Inicialización
inicial :: Movimiento
inicial = (t, pos)
  where
    t = matrix 3 3 $ \(i, j) -> " "
    pos = (1, 1)

-- Fin de partida
fin3enRaya :: Tablero -> Bool
fin3enRaya t = lleno t || hay3EnRaya t

lleno :: Tablero -> Bool
lleno t = null (casillasVacias t)

hay3EnRaya :: Tablero -> Bool
hay3EnRaya t = not (null fsx) || not (null csx) || not (null dsx)
  where
    fs = toLists t
    cs = columnasMatriz t
    ds = diagonalesMatriz t
    tripleX = takeWhile (== "X")
    tripleO = takeWhile (== "O")
    fsx = filter (\f -> length (tripleX f) == 3 || length (tripleO f) == 3) fs
    csx = filter (\c -> length (tripleX c) == 3 || length (tripleO c) == 3) cs
    dsx = filter (\d -> length (tripleX d) == 3 || length (tripleO d) == 3) ds

-- Movimientos
movs3enRaya :: Tablero -> String -> Movimientos
movs3enRaya t marcaMaquina = zip tableros listaVacias
  where
    listaVacias = casillasVacias t
    tableros = map (\pos -> setElem marcaMaquina pos t) listaVacias

-- Puntuaciones
puntua3enRaya :: Tablero -> Pos -> IO Double
puntua3enRaya t pos = do
  let hay2 = if hay2EnRaya t pos then 5.0 else 0.0
  let corta3 = if corta3EnRaya t pos then 7.5 else hay2
  let hay3 = if hay3EnRaya t then 10.0 else corta3
  return hay3

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones para los gráficos
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Parámetros %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

ajusteInicialMenu :: Float
ajusteInicialMenu = ancho / (2 * tamMatriz)

distribucionOpciones :: Point
distribucionOpciones = (-450.0, 130.0)

infoEstatica :: [[String]]
infoEstatica = [dif, turnos, marcas]
  where
    dif = ["Random", "Lowest", "Easy", "Medium", "Hard"]
    turnos = ["First", "Second"]
    marcas = ["X", "O"]

alturasEstaticas :: [Float]
alturasEstaticas = [dif, turnos, marcas]
  where
    dif = alturasCasillas !! 2
    turnos = alturasCasillas !! 5
    marcas = alturasCasillas !! 8

posMenu :: Point
posMenu = ((- ancho) - 2*ajusteInicial, ancho + ajusteInicial/2)

posOpciones :: Point
posOpciones = ((- ancho) - 2*ajusteInicial, ancho)

posCargar :: Point
posCargar = (ancho - ajusteInicial/2, - ancho + ajusteInicial)

posCargarJuego :: Point
posCargarJuego = ((- ancho) - 2*ajusteInicial, 0)

posGuardarJuego :: Point
posGuardarJuego = (ancho + 2*ajusteInicial, 0)

posVolver :: Point
posVolver = (ancho + 2*ajusteInicial, ancho)

posBoton :: Point
posBoton = (ancho - ajusteInicial/2, (- ancho) + 2*ajusteInicial)
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Fin parámetros %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

alturasCasillas :: [Float]
alturasCasillas = [a, a - diferencia .. (- a)]
  where
    a = ancho - ajusteInicialMenu
    diferencia = a / 4.5

cambiaOpcion :: Mundo -> Int -> String -> Mundo
cambiaOpcion mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional) nivel opcion
  | nivel == 0 = (mov, juego, traduceDif opcion, traduceProf opcion, marca, turno, seleccionado, esMaquina, adicional)
  | nivel == 1 = (mov, juego, dif, prof, marca, traduceTurnos opcion, seleccionado, esMaquina, adicional)
  | nivel == 2 = (mov, juego, dif, prof, opcion, turno, seleccionado, esMaquina, adicional)
  | nivel == 99 = mundo
  | otherwise = error "El nivel de opciones especificado para la función cambiaOpción del 3 en raya no existe."

creaTableroConOpciones :: Mundo -> Mundo
creaTableroConOpciones mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional)
  | turno == 2 = (inicial, juego, dif, p, m, turno, seleccionado, True, adicional)
  | otherwise = (inicial, juego, dif, p, m, turno, seleccionado, False, adicional)
  where
    m
      | marca == "O" || marca == "X" = marca
      | otherwise = "O"
    p
      | prof == 0 = 1
      | otherwise = prof

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

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones auxiliares
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

marcaMaquina3enRaya :: String -> String
marcaMaquina3enRaya marca = if marca == "X" then "O" else "X"

corta3EnRaya :: Tablero -> Pos -> Bool
corta3EnRaya t pos = not (null fsx) || not (null csx) || not (null dsx)
  where
    fs = toLists t
    cs = columnasMatriz t
    ds = diagonalesMatriz t
    marcaMaq = t ! pos
    marcaHum = marcaMaquina3enRaya marcaMaq
    fsx = filter (\f -> elem marcaMaq f && 2 == length [x | x <- f, x == marcaHum]) fs
    csx = filter (\c -> elem marcaMaq c && 2 == length [x | x <- c, x == marcaHum]) cs
    dsx = filter (\d -> elem marcaMaq d && 2 == length [x | x <- d, x == marcaHum]) ds

hay2EnRaya :: Tablero -> Pos -> Bool
hay2EnRaya t pos = not (null fsx) || not (null csx) || not (null dsx)
  where
    fs = toLists t
    cs = columnasMatriz t
    ds = diagonalesMatriz t
    marcaMaq = t ! pos
    marcaHum = marcaMaquina3enRaya marcaMaq
    fsx = filter (\f -> notElem marcaHum f && 2 == length [x | x <- f, x == marcaMaq]) fs
    csx = filter (\c -> notElem marcaHum c && 2 == length [x | x <- c, x == marcaMaq]) cs
    dsx = filter (\d -> notElem marcaHum d && 2 == length [x | x <- d, x == marcaMaq]) ds

traduceDif :: String -> Int
traduceDif dif
  | dif == "Lowest" = 1
  | dif == "Easy" = 2
  | dif == "Medium" = 3
  | dif == "Hard" = 4
  | otherwise = 0

traduceProf :: String -> Int
traduceProf dif
  | dif == "Random" = 1
  | dif == "Lowest" = 1
  | otherwise = 9

traduceTurnos :: String -> Int
traduceTurnos turno
  | turno == "First" = 1
  | otherwise = 2
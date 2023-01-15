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
    accionRealizada,
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
import GuardarCargar
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
hay3EnRaya t = any (triple . concat) (fs ++ cs ++ ds)
  where
    triple xs = xs == "XXX" || xs == "OOO"
    fs = toLists t
    cs = columnasMatriz t
    ds = diagonalesMatriz t

-- Movimientos
movs3enRaya :: Tablero -> String -> Movimientos
movs3enRaya t marcaMaquina = map (\pos -> (setElem marcaMaquina pos t, pos)) (casillasVacias t)

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
posMenu = ((- ancho) - 2 * ajusteInicial, ancho + ajusteInicial / 2)

posOpciones :: Point
posOpciones = ((- ancho) - 2 * ajusteInicial, ancho)

posCargar :: Point
posCargar = (ancho - ajusteInicial / 2, - ancho + ajusteInicial)

posCargarJuego :: Point
posCargarJuego = ((- ancho) - 2 * ajusteInicial, 0)

posGuardarJuego :: Point
posGuardarJuego = (ancho + 2 * ajusteInicial, 0)

posVolver :: Point
posVolver = (ancho + 2 * ajusteInicial, ancho)

posBoton :: Point
posBoton = (ancho - ajusteInicial / 2, (- ancho) + 2 * ajusteInicial)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Fin parámetros %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

listaPosiciones :: [Point]
listaPosiciones = [(x, y) | y <- [a, a - d .. (- a)], x <- [a, a - d .. (- a)]]
  where
    a = ancho - ajusteInicial
    d = diferenciaParaCasillas

matrizPosiciones :: Matrix Point
matrizPosiciones = matrix t t $ \p -> fst $ head $ filter (\(_, pos) -> pos == p) $ zip listaPosiciones ps
  where
    t = round tamMatriz
    ps = [(i, j) | i <- [1 .. t], j <- [1 .. t]]

alturasCasillas :: [Float]
alturasCasillas = [a, a - diferencia .. (- a)]
  where
    a = ancho - ajusteInicialMenu
    diferencia = a / 4.5

cambiaOpcion :: Mundo -> Int -> String -> Mundo
cambiaOpcion mundo nivel opcion = case nivel of
  0 -> ponDificultad mundo $ traduceDif opcion
  1 -> ponTurno mundo $ traduceTurnos opcion
  2 -> ponMarca mundo opcion
  99 -> mundo
  _ -> error "El nivel de opciones especificado para la función cambiaOpción del 3 en raya no existe."

creaTableroConOpciones :: Mundo -> Mundo
creaTableroConOpciones (_, juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional) = mundo
  where
    m
      | marca == "X" || marca == "O" = marca
      | otherwise = "O"
    p
      | prof == 0 = 1
      | otherwise = prof
    mundo = (inicial, juego, dif, p, m, turno, seleccionado, turno == 2, adicional)

accionRealizada :: Mundo -> (String, Point) -> String -> [Point] -> IO Mundo
accionRealizada mundo (pulsado, accion) temporal posiblesAcciones
  | pulsado == etiquetaOpciones = return $ iniciaOpciones juego
  | pulsado == etiquetaCargar = return menuCargarPartida
  | pulsado == etiquetaGuardar = do
    guardarPartida mundo
    return mundo
  | pulsado == etiquetaVolver = cargarPartida temporal
  | pulsado == "accion" && (accion `elem` posiblesAcciones) = do
    temporalPartida mundo
    let t = round tamMatriz
    let relacion = zip (toList matrizPosiciones) $ [(f, c) | f <- [1 .. t], c <- [1 .. t]]
    let posNueva = (snd . head . filter (\(c, p) -> c == accion)) relacion
    let nuevoEstado = (setElem marca posNueva . fst . dameMovimiento) mundo
    let ad
          | lleno nuevoEstado && (not . hay3EnRaya) nuevoEstado = [["empate"]]
          | hay3EnRaya nuevoEstado = [["humano"]]
          | otherwise = dameAdicional mundo
    return ((nuevoEstado, posNueva), juego, dameDificultad mundo, prof, marca, dameTurno mundo, seleccionado, True, ad)
  | otherwise = return mundo
  where
    juego = dameJuego mundo
    prof = dameProfundidad mundo
    marca = dameMarca mundo
    seleccionado = dameSeleccionado mundo

pintaMarca :: Pos -> Tablero -> Matrix Point -> Picture
pintaMarca pos estado posiciones = case marca of
  "X" -> pintaCruz lugar
  "O" -> pintaCirculo lugar
  _ -> blank
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
corta3EnRaya t pos = any ((== True) . any (\f -> (count marcaHum f == 2) && (count marcaMaq f == 1))) [fs, cs, ds]
  where
    fs = toLists t
    cs = columnasMatriz t
    ds = diagonalesMatriz t
    marcaMaq = t ! pos
    marcaHum = marcaMaquina3enRaya marcaMaq
    count x = length . filter (== x)

hay2EnRaya :: Tablero -> Pos -> Bool
hay2EnRaya t pos = any ((== True) . any (\f -> (count marcaMaq f == 2) && (count marcaHum f == 0))) [fs, cs, ds]
  where
    fs = toLists t
    cs = columnasMatriz t
    ds = diagonalesMatriz t
    marcaMaq = t ! pos
    marcaHum = marcaMaquina3enRaya marcaMaq
    count x = length . filter (== x)

traduceDif :: String -> Int
traduceDif dif = case dif of
  "Lowest" -> 1
  "Easy" -> 2
  "Medium" -> 3
  "Hard" -> 4
  _ -> 0

traduceProf :: String -> Int
traduceProf dif = case dif of
  "Random" -> 1
  "Lowest" -> 1
  _ -> 9

traduceTurnos :: String -> Int
traduceTurnos turno
  | turno == "First" = 1
  | otherwise = 2
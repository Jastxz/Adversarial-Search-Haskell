module FuncionesGato
  ( -- Funciones normales
    posicionesInicialesGatos,
    nombresGatos,
    falsoInicial,
    inicial,
    finGato,
    ratonEncerrado,
    ratonEscapado,
    casillasVaciasRaton,
    casillasVaciasGatos,
    movsGato,
    puntuaGato,
    marcaMaquinaGato,
    -- Funciones gráficas
    tamMatriz,
    tamTablero,
    ancho,
    diferenciaParaCasillas,
    ajusteInicial,
    matrizPosiciones,
    distribucionOpciones,
    alturasCasillas,
    casillasNegras,
    casillasBlancas,
    infoEstatica,
    alturasEstaticas,
    alturaTablero,
    turnoApos,
    cambiaOpcion,
    creaTableroConOpciones,
    accionRealizada,
    calculaNuevoEstado,
    posMenu,
    posOpciones,
    posCargar,
    posCargarJuego,
    posGuardarJuego,
    posVolver,
    posBoton,
    pintaComienzoTablero,
    pintaMarca,
  )
where

import Data.List
import Data.Matrix
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import GuardarCargar
import System.Directory.Internal.Prelude (on)
import Tipos
import Utiles
import UtilesGraficos

-- Inicialización
posicionesInicialesGatos :: [Pos]
posicionesInicialesGatos = [(8, 2), (8, 4), (8, 6), (8, 8)]

nombresGatos :: [String]
nombresGatos = ["G2", "G4", "G6", "G8"]

falsoInicial :: Tablero
falsoInicial = matrix tam tam $ \(i, j) -> añadePiezas (i, j)
  where
    tam = round tamMatriz

inicial :: Pos -> Movimiento
inicial pos = (setElem "R" pos falsoInicial, pos)

-- Fin de partida
finGato :: Tablero -> Bool
finGato t = ratonEncerrado t posRaton || ratonEscapado t posRaton posGatos
  where
    posRaton = buscaPieza t "R"
    posGatos = buscaGatos t

ratonEncerrado :: Tablero -> Pos -> Bool
ratonEncerrado t p = null $ casillasVaciasRaton t p

ratonEscapado :: Tablero -> Pos -> [Pos] -> Bool
ratonEscapado t raton gatos = filaRaton > filaGato || filaRaton == ma || null casGatos
  where
    filaRaton = fst raton
    filaGato = fst $ maximumBy (compare `on` fst) gatos
    (_, ma) = rangos t
    casGatos = concatMap (casillasVaciasGatos t) gatos

-- Movimientos
casillasVaciasRaton :: Tablero -> Pos -> [Pos]
casillasVaciasRaton = casillasAlrededorFicha

casillasVaciasGatos :: Tablero -> Pos -> [Pos]
casillasVaciasGatos m (f, c) = filter (\(i, _) -> i < f) $ casillasAlrededorFicha m (f, c)

movsGato :: Tablero -> String -> Movimientos
movsGato t marca = nub movs
  where
    posPieza = buscaPieza t marca
    movs = case marca of
      "R" -> map (\pos -> (intercambiaPieza t "R" pos posPieza, pos)) $ casillasVaciasRaton t posPieza
      _ -> mueveGato t $ buscaGatos t

mueveGato :: Tablero -> [Pos] -> Movimientos
mueveGato _ [] = []
mueveGato t gs = concatMap movimientosDelGato gs
  where
    movimientosDelGato g = map (\v -> (intercambiaPieza t (t ! g) v g, v)) (casillasVaciasGatos t g)

-- Puntuaciones
puntuaGato :: Tablero -> Pos -> IO Double
puntuaGato t pos = do
  -- Necesitamos saber si la máquina actúa de ratón o de gatos
  let laMaquinaEs = cabeza "puntuaGato" $ t ! pos
  let esRaton = laMaquinaEs == 'R'
  let esGatos = laMaquinaEs == 'G'
  -- También necesitamos las posiciones de todos
  let posRaton = if esRaton then pos else buscaPieza t "R"
  let posGatos = buscaGatos t
  -- Procesamos un poco los datos que tenemos
  let filaRaton = fromIntegral $ fst posRaton
  let filasGatos = map fst posGatos
  let maxDist = (fromIntegral . maximum) [abs (g1 - g2) | g1 <- filasGatos, g2 <- filasGatos]
  let adelantado = fromIntegral $ minimum filasGatos
  let lineaAtravesada = filaRaton >= adelantado
  -- Definimos las puntuaciones
  let puntuacionEscapado
        | esRaton = 30.0
        | esGatos = -30.0
        | otherwise = 0.0
  let puntuacionEncerrado
        | esRaton = -30.0
        | esGatos = 30.0
        | otherwise = 0.0
  let puntuacionMeta
        | esRaton = 1.25 * filaRaton
        | otherwise = 0.0
  let puntuacionLineaAtravesada
        | esRaton && lineaAtravesada = 10.0
        | esGatos && lineaAtravesada = - 15.0
        | otherwise = 0.0
  let penalizacionDistanciaGatos
        | esRaton && (maxDist >= 2.0) = maxDist
        | esGatos && (maxDist >= 2.0) = - (8.0 - abs (maxDist - adelantado)) * maxDist
        | otherwise = 0.0
  let puntuacionBasica
        | ratonEscapado t posRaton posGatos = puntuacionEscapado
        | ratonEncerrado t posRaton = puntuacionEncerrado
        | otherwise = puntuacionMeta + puntuacionLineaAtravesada + penalizacionDistanciaGatos
  return puntuacionBasica

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones para los gráficos
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Parámetros %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tamMatriz :: Float
tamMatriz = 8.0

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

alturaTablero :: Float
alturaTablero = -125.0

origenMinitableros :: Float
origenMinitableros = ancho * 0.75

diferenciaParaMiniCasillas :: Float
diferenciaParaMiniCasillas = origenMinitableros / tamMatriz

miniAncho :: Float
miniAncho = origenMinitableros / 2

miniAjusteInicial :: Float
miniAjusteInicial = (ancho / tamMatriz) / 2.75

distribucionOpciones :: Point
distribucionOpciones = (-450.0, 130.0)

infoEstatica :: [[String]]
infoEstatica = [dif, turnoYmarca]
  where
    dif = ["Random", "Lowest", "Easy", "Medium", "Hard"]
    turnoYmarca = ["Mouse", "Cats"]

alturasEstaticas :: [Float]
alturasEstaticas = [dif, turnosYmarcas]
  where
    dif = alturasCasillas !! 2
    turnosYmarcas = alturasCasillas !! 5

posMenu :: Point
posMenu = ((- ancho) - 3 * ajusteInicial, ancho + ajusteInicial)

posOpciones :: Point
posOpciones = ((- ancho) - 4 * ajusteInicial, ancho)

posCargar :: Point
posCargar = (ancho - ajusteInicial / 2, - ancho + ajusteInicial)

posCargarJuego :: Point
posCargarJuego = ((- ancho) - 4 * ajusteInicial, 0)

posGuardarJuego :: Point
posGuardarJuego = (ancho + 4 * ajusteInicial, 0)

posVolver :: Point
posVolver = (ancho + 4 * ajusteInicial, ancho)

posBoton :: Point
posBoton = (ancho - ajusteInicial / 2, (- ancho) + 4 * ajusteInicial)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Fin parámetros %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

matrizPosiciones :: Matrix Point
matrizPosiciones = matrix t t $ \p -> fst (cabeza "matrizPosiciones" (filter (\(cas, pos) -> pos == p) relacion))
  where
    t = round tamMatriz
    d = diferenciaParaCasillas
    a = (- ancho) + ajusteInicial
    anchurasImpares = [a, a + (2 * d) .. a + ((tamMatriz -2.0) * d)]
    anchurasPares = [a + d, a + (3 * d) .. a + ((tamMatriz -1.0) * d)]
    bCuadImpares = [(j, i) | i <- reverse anchurasPares, j <- anchurasImpares]
    nCuadImpares = [(j, i) | i <- reverse anchurasPares, j <- anchurasPares]
    nCuadPares = [(j, i) | i <- reverse anchurasImpares, j <- anchurasImpares]
    bCuadPares = [(j, i) | i <- reverse anchurasImpares, j <- anchurasPares]
    casillas = uneFilas (uneCasillas bCuadImpares nCuadImpares) (uneCasillas nCuadPares bCuadPares)
    relacion = zip casillas [(i, j) | i <- [1 .. t], j <- [1 .. t]]

alturasCasillas :: [Float]
alturasCasillas = [a, a - diferencia .. 0]
  where
    a = ancho - ajusteInicialMenu * 2
    diferencia = a / 5.0

casillasNegras :: [Point]
casillasNegras = uneCasillas cuadradosImpares cuadradosPares
  where
    d = diferenciaParaCasillas
    a = (- ancho) + ajusteInicial
    anchurasImpares = [a, a + (2 * d) .. a + ((tamMatriz -2.0) * d)]
    anchurasPares = [a + d, a + (3 * d) .. a + ((tamMatriz -1.0) * d)]
    cuadradosImpares = [(j, i) | i <- reverse anchurasPares, j <- anchurasPares]
    cuadradosPares = [(j, i) | i <- reverse anchurasImpares, j <- anchurasImpares]

casillasBlancas :: [Point]
casillasBlancas = uneCasillas cuadradosImpares cuadradosPares
  where
    d = diferenciaParaCasillas
    a = (- ancho) + ajusteInicial
    anchurasImpares = [a, a + (2 * d) .. a + ((tamMatriz -2.0) * d)]
    anchurasPares = [a + d, a + (3 * d) .. a + ((tamMatriz -1.0) * d)]
    cuadradosImpares = [(j, i) | i <- reverse anchurasPares, j <- anchurasImpares]
    cuadradosPares = [(j, i) | i <- reverse anchurasImpares, j <- anchurasPares]

cambiaOpcion :: Point -> Mundo -> Int -> String -> IO Mundo
cambiaOpcion (x, y) mundo@(mov, juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional) nivel opcion = case nivel of
  0 -> return $ ponDificultad mundo $ traduceDif opcion
  1 -> return $ ponMarca mundo $ traduceMarca opcion
  99 -> cambiaMiniTablero (x, y - alturaTablero) mundo
  _ -> error "El nivel de opciones especificado para la función cambiaOpción del juego del gato no existe."

cambiaMiniTablero :: Point -> Mundo -> IO Mundo
cambiaMiniTablero raton mundo
  | (not . null) pulsadas && dameMarca mundo == "R" = return $ ponTurno mundo $ posAturno (cabeza "cambiaMiniTablero" pulsadas)
  | otherwise = return mundo
  where
    posPosibles = casillasVacias falsoInicial
    casillasPosibles = map (matrizMiniPosiciones !) posPosibles
    pulsadas = map snd $ filter (\(c, p) -> pulsaCercaMini raton c) $ zip casillasPosibles posPosibles

creaTableroConOpciones :: Mundo -> Mundo
creaTableroConOpciones (mov, juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional)
  | marca == "R" = (inicial (turnoApos turno), juego, dif, p, marca, turno, seleccionado, False, adicional)
  | otherwise = (mov, juego, dif, p, "G", turno, seleccionado, True, adicional)
  where
    p
      | prof == 0 = 1
      | otherwise = prof

accionRealizada :: Mundo -> (String, Point) -> String -> IO Mundo
accionRealizada mundo (pulsado, accion) temporal
  | pulsado == etiquetaOpciones = return $ (iniciaOpciones . dameJuego) mundo
  | pulsado == etiquetaCargar = return menuCargarPartida
  | pulsado == etiquetaGuardar = do
    guardarPartida mundo
    return mundo
  | pulsado == etiquetaVolver = cargarPartida temporal
  | pulsado == "accion" = do
    -- Guardamos el estado actual en un archivo temporal
    temporalPartida mundo
    calculaNuevoEstado accion mundo
  | otherwise = return mundo

calculaNuevoEstado :: Point -> Mundo -> IO Mundo
calculaNuevoEstado casilla mundo
  | (seleccionado == "R") && (posSeñalada `elem` vaciasRaton) = do
    calculaMundo posSeñalada mundo
  | (seleccionado `elem` nombresGatos) && (posSeñalada `elem` vaciasGatos) = do
    calculaMundo posSeñalada mundo
  | ((marca == "R") && (el == "R")) || ((marca == "G") && (el `elem` nombresGatos)) = do
    return $ ponSeleccionado (ponEsMaquina mundo False) el
  | otherwise = return mundo
  where
    estado = (fst . dameMovimiento) mundo
    marca = dameMarca mundo
    seleccionado = dameSeleccionado mundo
    vaciasRaton = casillasVaciasRaton estado $ buscaPieza estado "R"
    vaciasGatos = casillasVaciasGatos estado $ buscaPieza estado $ dameSeleccionado mundo
    t = round tamMatriz
    relacionadas = zip (toList matrizPosiciones) [(i, j) | i <- [1 .. t], j <- [1 .. t]]
    posSeñalada = snd $ cabeza "calculaNuevoEstado" $ filter (\(c, p) -> c == casilla) relacionadas
    el = estado ! posSeñalada

calculaMundo :: Pos -> Mundo -> IO Mundo
calculaMundo casilla mundo@((estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional) = do
  let posAntigua = buscaPieza estado seleccionado
  let nuevoEstado = intercambiaPieza estado seleccionado casilla posAntigua
  let posRaton
        | seleccionado == "R" = casilla
        | otherwise = buscaPieza nuevoEstado "R"
  let posGatos = map (buscaPieza nuevoEstado) nombresGatos
  let haEscapado = ratonEscapado nuevoEstado posRaton posGatos
  let estaEncerrado = ratonEncerrado nuevoEstado posRaton
  let ad
        | ((marca == "R") && haEscapado) || ((marca == "G") && estaEncerrado) = [["humano"]]
        | ((marca == "G") && haEscapado) || ((marca == "R") && estaEncerrado) = [["maquina"]]
        | otherwise = adicional
  return ((nuevoEstado, casilla), juego, dif, prof, marca, turno, "", True, ad)

pintaMarca :: Pos -> Tablero -> Picture
pintaMarca pos estado
  | marca == "R" = pintaRaton lugar
  | marca `elem` nombresGatos = pintaGato lugar
  | otherwise = blank
  where
    marca = estado ! pos
    lugar = matrizPosiciones ! pos

pintaComienzoTablero :: Movimiento -> IO Picture
pintaComienzoTablero (estado, pos) = do
  let borde = rectangleWire origenMinitableros origenMinitableros
  let cAjdrz = [1 .. round tamMatriz]
  let marcasDibujadas = [pintaMiniMarca (i, j) estado matrizMiniPosiciones | i <- cAjdrz, j <- cAjdrz]
  let res = translate 0 alturaTablero $ pictures [borde, pictures marcasDibujadas]
  return res

matrizMiniPosiciones :: Matrix Point
matrizMiniPosiciones = matrix t t $ \p -> fst (cabeza "matrizMiniPosiciones" (filter (\(cas, pos) -> pos == p) relacion))
  where
    t = round tamMatriz
    d = diferenciaParaMiniCasillas
    a = (- miniAncho) + miniAjusteInicial
    anchurasImpares = [a, a + (2 * d) .. a + ((tamMatriz -2.0) * d)]
    anchurasPares = [a + d, a + (3 * d) .. a + ((tamMatriz -1.0) * d)]
    bCuadImpares = [(j, i) | i <- reverse anchurasPares, j <- anchurasImpares]
    nCuadImpares = [(j, i) | i <- reverse anchurasPares, j <- anchurasPares]
    nCuadPares = [(j, i) | i <- reverse anchurasImpares, j <- anchurasImpares]
    bCuadPares = [(j, i) | i <- reverse anchurasImpares, j <- anchurasPares]
    casillas = uneFilas (uneCasillas bCuadImpares nCuadImpares) (uneCasillas nCuadPares bCuadPares)
    relacion = zip casillas [(i, j) | i <- [1 .. t], j <- [1 .. t]]

pintaMiniMarca :: Pos -> Tablero -> Matrix Point -> Picture
pintaMiniMarca pos estado posiciones
  | marca == "R" = pintaMiniRaton lugar
  | marca `elem` nombresGatos = pintaMiniGato lugar
  | marca == "X" = translate i j casNegra
  | otherwise = blank
  where
    marca = estado ! pos
    lugar@(i, j) = posiciones ! pos
    casNegra = color marron $ rectangleSolid diferenciaParaMiniCasillas diferenciaParaMiniCasillas

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones auxiliares
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

añadePiezas :: Pos -> String
añadePiezas actual
  | actual `elem` posicionesInicialesGatos = "G" ++ show (snd actual)
  | even suma = " "
  | otherwise = "X"
  where
    suma = uncurry (+) actual

marcaMaquinaGato :: String -> String
marcaMaquinaGato marca = if marca == "R" then "G" else "R"

buscaGatos :: Tablero -> [Pos]
buscaGatos t = map (buscaPieza t) nombresGatos

traduceDif :: String -> Int
traduceDif dif = case dif of
  "Lowest" -> 1
  "Easy" -> 2
  "Medium" -> 3
  "Hard" -> 4
  _ -> 0

traduceProf :: String -> Int
traduceProf dif = case dif of
  "Lowest" -> 2
  "Easy" -> 4
  "Medium" -> 6
  "Hard" -> 8
  _ -> 1

traduceMarca :: String -> String
traduceMarca marca = case marca of
  "Mouse" -> "R"
  "Cats" -> "G"
  _ -> "Fallo"

turnoApos :: Int -> Pos
turnoApos turno
  | turno == 0 = (1, 1)
  | otherwise = (turno `div` 10, turno `mod` 10)

posAturno :: Pos -> Int
posAturno (f, c) = stringToInt turno
  where
    fs = show f
    cs = show c
    lf = length fs
    lc = length cs
    ceros = concat $ replicate (abs (lf - lc)) "0"
    turno
      | lf > lc = fs ++ ceros ++ cs
      | otherwise = ceros ++ fs ++ cs

pintaRaton :: Point -> Picture
pintaRaton (x, y) = translate x y raton
  where
    raton = color white formaPeon

pintaGato :: Point -> Picture
pintaGato (x, y) = translate x y gato
  where
    gato = color black formaPeon

formaPeon :: Picture
formaPeon = pictures [circulo, triangulo]
  where
    tam = diferenciaParaCasillas / 2
    circulo = translate 0.0 tam $ circleSolid (tam / 2)
    triangulo = polygon [(0.0, tam), (- tam, 0.0), (tam, 0.0)]

uneFilas :: [Point] -> [Point] -> [Point]
uneFilas fi fp = take tam fi ++ take tam fp ++ uneFilas (drop tam fi) (drop tam fp)
  where
    tam = round tamMatriz

uneCasillas :: [Point] -> [Point] -> [Point]
uneCasillas as bs = concat [[a, b] | (a, b) <- zip as bs]

pintaMiniRaton :: Point -> Picture
pintaMiniRaton (x, y) = translate x y raton
  where
    raton = color white formaMiniPeon

pintaMiniGato :: Point -> Picture
pintaMiniGato (x, y) = translate x y gato
  where
    gato = color black formaMiniPeon

formaMiniPeon :: Picture
formaMiniPeon = pictures [circulo, triangulo]
  where
    tam = diferenciaParaMiniCasillas / 2
    circulo = translate 0.0 tam $ circleSolid (tam / 2)
    triangulo = polygon [(0.0, tam), (- tam, 0.0), (tam, 0.0)]

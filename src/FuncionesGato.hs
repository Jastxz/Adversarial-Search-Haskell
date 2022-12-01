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

import Data.List (nub, reverse)
import Data.Matrix
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Tipos
import Utiles
import UtilesGraficos

-- Inicialización
posicionesInicialesGatos = [(8, 2), (8, 4), (8, 6), (8, 8)]

nombresGatos = ["G2", "G4", "G6", "G8"]

falsoInicial :: Tablero
falsoInicial = matrix tam tam $ \(i, j) -> añadePiezas (i, j)
  where
    tam = round tamMatriz

inicial :: Pos -> Movimiento
inicial pos = (t, pos)
  where
    t = setElem "R" pos falsoInicial

añadePiezas :: Pos -> String
añadePiezas actual
  | actual `elem` posicionesInicialesGatos = "G" ++ show (snd actual)
  | even suma = " "
  | otherwise = "X"
  where
    suma = uncurry (+) actual

-- Fin de partida
finGato :: Tablero -> Bool
finGato t = ratonEncerrado t posRaton || ratonEscapado t posRaton posGatos
  where
    posRaton = buscaPieza t "R"
    posGatos = [buscaPieza t m | m <- nombresGatos]

ratonEncerrado :: Tablero -> Pos -> Bool
ratonEncerrado t pos = null (casillasVaciasRaton t pos)

ratonEscapado :: Tablero -> Pos -> [Pos] -> Bool
ratonEscapado t raton gatos = filaRaton > filaGato || filaRaton == ma || null casGatos
  where
    filaRaton = fst raton
    filaGato = maximum $ map fst gatos
    (_, ma) = rangos t
    casGatos = concatMap (casillasVaciasGatos t) gatos

-- Movimientos
casillasVaciasRaton :: Tablero -> Pos -> [Pos]
casillasVaciasRaton = casillasAlrededorFicha

casillasVaciasGatos :: Tablero -> Pos -> [Pos]
casillasVaciasGatos m posGato@(f, c) = filter (\(i, j) -> i < f) $ casillasAlrededorFicha m posGato

movsGato :: Tablero -> String -> Movimientos
movsGato t marca
  | marca == "R" = nub $ map (\pos -> (intercambiaPieza t "R" pos posPieza, pos)) (casillasVaciasRaton t posPieza)
  | otherwise = nub $ mueveGato t posGatos
  where
    posPieza = buscaPieza t marca
    posGatos = [buscaPieza t m | m <- nombresGatos]

mueveGato :: Tablero -> [Pos] -> Movimientos
mueveGato _ [] = []
mueveGato t (g : gs) = movimientosDelGato ++ mueveGato t gs
  where
    validas = casillasVaciasGatos t g
    nombre = t ! g
    movimientosDelGato = map (\v -> (intercambiaPieza t nombre v g, v)) validas

-- Puntuaciones
puntuaGato :: Tablero -> Pos -> IO Double
puntuaGato t pos = do
  -- Necesitamos saber si la máquina actúa de ratón o de gatos
  let laMaquinaEs = cabeza "puntuaGato" $ t ! pos
  let esRaton = laMaquinaEs == 'R'
  let esGatos = laMaquinaEs == 'G'
  -- También necesitamos las posiciones de todos
  let posRaton
        | esRaton = pos
        | otherwise = buscaPieza t "R"
  let posGatos = [buscaPieza t m | m <- nombresGatos]
  -- Procesamos un poco los datos que tenemos
  let haEscapado = ratonEscapado t posRaton posGatos
  let estaEncerrado = ratonEncerrado t posRaton
  let finJuego = haEscapado || estaEncerrado
  let filaRaton = fromIntegral $ fst posRaton
  let filasGatos = map fst posGatos
  let distancias = [abs (g1 - g2) | g1 <- filasGatos, g2 <- filasGatos]
  let maxDist = fromIntegral $ maximum distancias
  let perdido = maxDist >= 2.0
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
  let puntuacionLineaAtravesada | esRaton && lineaAtravesada = 10.0
        | esGatos && lineaAtravesada = - 15.0
        | otherwise = 0.0
  let penalizacionDistanciaGatos
        | esRaton && perdido = maxDist
        | esGatos && perdido = - (8.0 - abs (maxDist - adelantado)) * maxDist
        | otherwise = 0.0
  let puntuacionBasica
        | haEscapado = puntuacionEscapado
        | estaEncerrado = puntuacionEncerrado
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
posMenu = ((- ancho) - 3*ajusteInicial, ancho + ajusteInicial)

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
    ps = [(i, j) | i <- [1 .. t], j <- [1 .. t]]
    d = diferenciaParaCasillas
    a = (- ancho) + ajusteInicial
    anchurasImpares = [a, a + (2 * d) .. a + ((tamMatriz -2.0) * d)]
    anchurasPares = [a + d, a + (3 * d) .. a + ((tamMatriz -1.0) * d)]
    alturasImpares = reverse anchurasPares
    alturasPares = reverse anchurasImpares
    bCuadImpares = [(j, i) | i <- alturasImpares, j <- anchurasImpares]
    nCuadImpares = [(j, i) | i <- alturasImpares, j <- anchurasPares]
    filasImpares = uneCasillas bCuadImpares nCuadImpares
    nCuadPares = [(j, i) | i <- alturasPares, j <- anchurasImpares]
    bCuadPares = [(j, i) | i <- alturasPares, j <- anchurasPares]
    filasPares = uneCasillas nCuadPares bCuadPares
    casillas = uneFilas filasImpares filasPares
    relacion = zip casillas ps

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
    alturasImpares = reverse anchurasPares
    alturasPares = reverse anchurasImpares
    cuadradosImpares = [(j, i) | i <- alturasImpares, j <- anchurasPares]
    cuadradosPares = [(j, i) | i <- alturasPares, j <- anchurasImpares]

casillasBlancas :: [Point]
casillasBlancas = uneCasillas cuadradosImpares cuadradosPares
  where
    d = diferenciaParaCasillas
    a = (- ancho) + ajusteInicial
    anchurasImpares = [a, a + (2 * d) .. a + ((tamMatriz -2.0) * d)]
    anchurasPares = [a + d, a + (3 * d) .. a + ((tamMatriz -1.0) * d)]
    alturasImpares = reverse anchurasPares
    alturasPares = reverse anchurasImpares
    cuadradosImpares = [(j, i) | i <- alturasImpares, j <- anchurasImpares]
    cuadradosPares = [(j, i) | i <- alturasPares, j <- anchurasPares]

cambiaOpcion :: Point -> Mundo -> Int -> String -> IO Mundo
cambiaOpcion raton@(x, y) mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional) nivel opcion
  | nivel == 0 = do
    let nuevoMundo = (mov, juego, traduceDif opcion, traduceProf opcion, marca, turno, seleccionado, esMaquina, adicional)
    return nuevoMundo
  | nivel == 1 = do
    let nuevoMundo = (mov, juego, dif, prof, traduceMarca opcion, turno, seleccionado, esMaquina, adicional)
    return nuevoMundo
  | nivel == 99 = do
    let ratonCorregido = (x, y - alturaTablero)
    cambiaMiniTablero ratonCorregido mundo
  | otherwise = error "El nivel de opciones especificado para la función cambiaOpción del juego del gato no existe."

cambiaMiniTablero :: Point -> Mundo -> IO Mundo
cambiaMiniTablero raton mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional)
  | not (null pulsadas) && marca == "R" = do
    let nuevoTurno = posAturno (cabeza "cambiaMiniTablero" pulsadas)
    let nuevoMundo = (mov, juego, dif, prof, marca, nuevoTurno, seleccionado, esMaquina, adicional)
    return nuevoMundo
  | otherwise = return mundo
  where
    posPosibles = casillasVacias $ fst $ inicial $ turnoApos turno
    casillasPosibles = map (matrizMiniPosiciones !) posPosibles
    relacion = zip casillasPosibles posPosibles
    pulsadas = map snd $ filter (\(c, p) -> pulsaCercaMini raton c) relacion

creaTableroConOpciones :: Mundo -> Mundo
creaTableroConOpciones mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional)
  | marca == "R" = (inicial (turnoApos turno), juego, dif, p, marca, turno, seleccionado, False, adicional)
  | otherwise = (mov, juego, dif, p, "G", turno, seleccionado, True, adicional)
  where
    p
      | prof == 0 = 1
      | otherwise = prof

calculaNuevoEstado :: Point -> Mundo -> IO Mundo
calculaNuevoEstado casilla mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional)
  | (seleccionado == "R") && (posSeñalada `elem` vaciasRaton) = do
    return $ calculaMundo posSeñalada mundo
  | (seleccionado `elem` nombresGatos) && (posSeñalada `elem` vaciasGatos) = do
    return $ calculaMundo posSeñalada mundo
  | (marca == "R") && (el == "R") = do
    return (mov, juego, dif, prof, marca, turno, el, False, adicional)
  | (marca == "G") && (el `elem` nombresGatos) = do
    return (mov, juego, dif, prof, marca, turno, el, False, adicional)
  | otherwise = return mundo
  where
    posRaton = buscaPieza estado "R"
    posGato = buscaPieza estado seleccionado
    vaciasRaton = casillasVaciasRaton estado posRaton
    vaciasGatos = casillasVaciasGatos estado posGato
    t = round tamMatriz
    ps = [(i, j) | i <- [1 .. t], j <- [1 .. t]]
    matrz = toList matrizPosiciones
    relacionadas = zip matrz ps
    posSeñalada = snd $ cabeza "calculaNuevoEstado" $ filter (\(c, p) -> c == casilla) relacionadas
    el = estado ! posSeñalada

calculaMundo :: Pos -> Mundo -> Mundo
calculaMundo casilla ((estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional) = nuevoMundo
  where
    posAntigua = buscaPieza estado seleccionado
    nuevoEstado = intercambiaPieza estado seleccionado casilla posAntigua
    posRaton
      | seleccionado == "R" = casilla
      | otherwise = buscaPieza nuevoEstado "R"
    posGatos = [buscaPieza nuevoEstado m | m <- nombresGatos]
    ad
      | (marca == "R") && ratonEscapado nuevoEstado posRaton posGatos = [["humano"]]
      | (marca == "G") && ratonEncerrado nuevoEstado posRaton = [["humano"]]
      | (marca == "G") && ratonEscapado nuevoEstado posRaton posGatos = [["maquina"]]
      | (marca == "R") && ratonEncerrado nuevoEstado posRaton = [["maquina"]]
      | otherwise = adicional
    nuevoMundo = ((nuevoEstado, casilla), juego, dif, prof, marca, turno, "", True, ad)

pintaMarca :: Pos -> Tablero -> Picture
pintaMarca pos estado
  | marca == "R" = pintaRaton lugar
  | marca `elem` nombresGatos = pintaGato lugar
  | otherwise = blank
  where
    marca = estado ! pos
    lugar = matrizPosiciones ! pos

pintaComienzoTablero :: Movimiento -> IO Picture
pintaComienzoTablero mov@(estado, pos) = do
  let borde = rectangleWire origenMinitableros origenMinitableros
  let tam = round tamMatriz
  let cAjdrz = [1 .. tam]
  let posiciones = [(i, j) | i <- cAjdrz, j <- cAjdrz]
  let marcasDibujadas = map (\pos -> pintaMiniMarca pos estado matrizMiniPosiciones) posiciones
  let estadoDibujado = pictures marcasDibujadas
  let res = translate 0 alturaTablero $ pictures [borde, estadoDibujado]
  return res

matrizMiniPosiciones :: Matrix Point
matrizMiniPosiciones = matrix t t $ \p -> fst (cabeza "matrizMiniPosiciones" (filter (\(cas, pos) -> pos == p) relacion))
  where
    t = round tamMatriz
    ps = [(i, j) | i <- [1 .. t], j <- [1 .. t]]
    d = diferenciaParaMiniCasillas
    a = (- miniAncho) + miniAjusteInicial
    anchurasImpares = [a, a + (2 * d) .. a + ((tamMatriz -2.0) * d)]
    anchurasPares = [a + d, a + (3 * d) .. a + ((tamMatriz -1.0) * d)]
    alturasImpares = reverse anchurasPares
    alturasPares = reverse anchurasImpares
    bCuadImpares = [(j, i) | i <- alturasImpares, j <- anchurasImpares]
    nCuadImpares = [(j, i) | i <- alturasImpares, j <- anchurasPares]
    filasImpares = uneCasillas bCuadImpares nCuadImpares
    nCuadPares = [(j, i) | i <- alturasPares, j <- anchurasImpares]
    bCuadPares = [(j, i) | i <- alturasPares, j <- anchurasPares]
    filasPares = uneCasillas nCuadPares bCuadPares
    casillas = uneFilas filasImpares filasPares
    relacion = zip casillas ps

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

marcaMaquinaGato :: String -> String
marcaMaquinaGato marca = if marca == "R" then "G" else "R"

cerosEnCadena :: Int -> String
cerosEnCadena 0 = ""
cerosEnCadena d = "0" ++ cerosEnCadena (d -1)

traduceDif :: String -> Int
traduceDif dif
  | dif == "Lowest" = 1
  | dif == "Easy" = 2
  | dif == "Medium" = 3
  | dif == "Hard" = 4
  | otherwise = 0

traduceProf :: String -> Int
traduceProf dif
  | dif == "Lowest" = 1
  | dif == "Easy" = 5
  | dif == "Medium" = 8
  | dif == "Hard" = 9
  | otherwise = 1

traduceMarca :: String -> String
traduceMarca marca
  | marca == "Mouse" = "R"
  | marca == "Cats" = "G"
  | otherwise = "Fallo"

turnoApos :: Int -> Pos
turnoApos turno
  | turno == 0 = (1, 1)
  | otherwise = (f, c)
  where
    pos = show turno
    tamTurno = length pos
    componentes
      | even tamTurno = pos
      | otherwise = "0" ++ pos
    longCom = length componentes
    tamComponente = longCom `div` 2
    f = stringToInt $ take tamComponente componentes
    c = stringToInt $ drop tamComponente componentes

posAturno :: Pos -> Int
posAturno (f, c) = stringToInt turno
  where
    fs = show f
    cs = show c
    lf = length fs
    lc = length cs
    diferencia = abs (lf - lc)
    ceros = cerosEnCadena diferencia
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
uneFilas fi fp = f1 ++ f2 ++ uneFilas ri rp
  where
    tam = round tamMatriz
    f1 = take tam fi
    f2 = take tam fp
    ri = drop tam fi
    rp = drop tam fp

uneCasillas :: [Point] -> [Point] -> [Point]
uneCasillas [] _ = []
uneCasillas _ [] = []
uneCasillas (a : as) (b : bs) = [a, b] ++ uneCasillas as bs

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

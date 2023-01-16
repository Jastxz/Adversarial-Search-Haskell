module FuncionesDamas
  ( -- Funciones normales
    posicionesInicialesBlancas,
    posicionesInicialesNegras,
    nombresBlancas,
    nombresNegras,
    nombresReinasBlancas,
    nombresReinasNegras,
    inicial,
    finDamas,
    bandoGanador,
    calculaCasillasAtaque,
    casillasValidasDamas,
    casillasValidasReinas,
    movsDamas,
    puntuaDamas,
    piezasVivas,
    marcaMaquinaDamas,
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
    casillasDisponiblesParaElJugador,
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

import Data.List (nub, reverse)
import Data.Matrix
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import GuardarCargar
import Tipos
import Utiles
import UtilesGraficos

-- Constantes
posicionesInicialesBlancas :: [Pos]
posicionesInicialesBlancas = [(6, 2), (6, 4), (6, 6), (6, 8), (7, 1), (7, 3), (7, 5), (7, 7), (8, 2), (8, 4), (8, 6), (8, 8)]

posicionesInicialesNegras :: [Pos]
posicionesInicialesNegras = [(1, 1), (1, 3), (1, 5), (1, 7), (2, 2), (2, 4), (2, 6), (2, 8), (3, 1), (3, 3), (3, 5), (3, 7)]

nombresBlancas :: [String]
nombresBlancas = ["B62", "B64", "B66", "B68", "B71", "B73", "B75", "B77", "B82", "B84", "B86", "B88"]

nombresNegras :: [String]
nombresNegras = ["N11", "N13", "N15", "N17", "N22", "N24", "N26", "N28", "N31", "N33", "N35", "N37"]

nombresReinasBlancas :: [String]
nombresReinasBlancas = map ('R' :) nombresBlancas

nombresReinasNegras :: [String]
nombresReinasNegras = map ('R' :) nombresNegras

{- Pesos otorgados a cada pieza de información -}
valorDama :: Double
valorDama = 9.4

valorReina :: Double
valorReina = 10.5

valorAtaque :: Double
valorAtaque = 1.0

valorAtacado :: Double
valorAtacado = 0.5

penalizacionCentral :: Double
penalizacionCentral = -0.3

valorAprovechamientoTerreno :: Double
valorAprovechamientoTerreno = 2.9

inicial :: Pos -> Movimiento
inicial pos = (t, pos)
  where
    tam = round tamMatriz
    t = matrix tam tam $ \(i, j) -> añadePiezas (i, j)

-- Fin de partida
finDamas :: Tablero -> Bool
finDamas t = muertasB || muertasN || any null [movsDamas t "B", movsDamas t "N"]
  where
    muertasB = all null [piezasVivas t !! i | i <- [0, 1]]
    muertasN = all null [piezasVivas t !! i | i <- [2, 3]]

bandoGanador :: Tablero -> [[String]] -> String
bandoGanador t vivas
  | (not hayNegras && hayBlancas) || (noHayMovimientos && desempate > 0) = "blancas"
  | (not hayBlancas && hayNegras) || (noHayMovimientos && desempate < 0) = "negras"
  | otherwise = "empate"
  where
    nDB = fromIntegral $ length $ cabeza "bandoGanador" vivas
    nRB = fromIntegral $ length $ vivas !! 1
    nDN = fromIntegral $ length $ vivas !! 2
    nRN = fromIntegral $ length $ vivas !! 3
    hayBlancas = nDB + nRB > 0.0
    hayNegras = nDN + nRN > 0.0
    noHayMovimientos = any null [movsDamas t "B", movsDamas t "N"]
    desempate = nDB + nRB - nDN - nRN

-- Movimientos
casillasValidasDamas :: Tablero -> Pos -> ([Pos], Bool)
casillasValidasDamas m posPieza@(f, _) = casillasPosibles
  where
    bandoP = bando $ m ! posPieza
    casillasAlrededor = casillasAlrededorFicha m posPieza
    (casillas, casillasAtaque) = case bandoP of
      'B' -> (filter (\(i, j) -> i < f) casillasAlrededor, filter (\(i, j) -> i < f) $ calculaCasillasAtaque m bandoP posPieza)
      'N' -> (filter (\(i, j) -> i > f) casillasAlrededor, filter (\(i, j) -> i > f) $ calculaCasillasAtaque m bandoP posPieza)
      _ -> ([], [])
    -- Obligación de la captura
    casillasPosibles
      | null casillasAtaque = (casillas, False)
      | otherwise = (casillasAtaque, True)

-- Casillas validas para las reinas de la versión inglesa
casillasValidasReinas :: Tablero -> Pos -> ([Pos], Bool)
casillasValidasReinas m p = casillasPosibles
  where
    bandoP = bando $ m ! p
    casillasAtaque = calculaCasillasAtaque m bandoP p
    -- Obligación de la captura
    casillasPosibles
      | null casillasAtaque = (casillasAlrededorFicha m p, False)
      | otherwise = (casillasAtaque, True)

calculaCasillasAtaque :: Tablero -> Char -> Pos -> [Pos]
calculaCasillasAtaque m bando posPieza = filter (compruebaAtaque m bando posPieza) $ casillasSegundoNivel m posPieza

compruebaAtaque :: Tablero -> Char -> Pos -> Pos -> Bool
compruebaAtaque m bandoP posPieza@(f, _) posAtaque@(af, _) = condiciones
  where
    reina = esReina $ m ! posPieza
    piezaEliminada = m ! fst (ataque m posPieza posAtaque reina)
    bandoE = bando piezaEliminada
    condicionesBasicas = piezaEliminada `notElem` ["X", "", " "] && bandoE /= bandoP
    condiciones
      | not reina && bandoP == 'B' = (f > af) && condicionesBasicas
      | not reina && bandoP == 'N' = (f < af) && condicionesBasicas
      | otherwise = condicionesBasicas

-- Función casi perfecta para la versión española de las damas.
revisaDiagonal :: Tablero -> Pos -> Pos -> String -> ([Pos], Bool) -> ([Pos], Bool)
revisaDiagonal m p@(f, c) lims@(a, b) pieza (ps, at)
  | not (dentroDelTablero p m) = error mensajeError
  | nombre == pieza && dentroDelTablero pos m = revisaDiagonal m pos lims pieza (ps, at)
  | tocaLimite && casillaVacia m p = (p : ps, at)
  | tocaLimite || bandoActual == bandoPieza || (bandoContrario && not ataca) = (ps, at)
  | ataca || at = (pAtacadas, True)
  | otherwise = revisaDiagonal m pos lims pieza (p : ps, at)
  where
    mensajeError = "La posicion pasada de la pieza en revisaDiagonal es " ++ show p ++ "que esta fuera del tablero."
    tocaLimite = f == a || c == b
    nombre = m ! p
    bandoActual = bando nombre
    bandoPieza = bando pieza
    bandoContrario = bandoActual /= bandoPieza
    diferenciaFila = f - a
    diferenciaColumna = c - b
    filaE
      | diferenciaFila < 0 = f + 1
      | diferenciaFila > 0 = f - 1
      | otherwise = 0
    columnaE
      | diferenciaColumna < 0 = c + 1
      | diferenciaColumna > 0 = c - 1
      | otherwise = 0
    pos = (filaE, columnaE)
    ataca = bandoContrario && dentroDelTablero pos m && casillaVacia m pos
    pAtacadas
      | ataca && at = pos : ps
      | otherwise = [pos]

movsDamas :: Tablero -> String -> Movimientos
movsDamas t marca
  | atD && not atR = nub mvD
  | not atD && atR = nub mvR
  | otherwise = nub mvD ++ nub mvR
  where
    (posDamas, posReinas) = posicionesSegunBando t marca $ piezasVivas t
    (mvD, atD)
      | null posDamas = ([], False)
      | otherwise = mueveDama t (posDamas, False) []
    (mvR, atR)
      | null posReinas = ([], False)
      | otherwise = mueveReina t (posReinas, False) []

posicionesSegunBando :: Tablero -> String -> [[String]] -> ([Pos], [Pos])
posicionesSegunBando t marca vivas = (posDamas, posReinas)
  where
    damasBlancas = cabeza "movsDamas" vivas
    reinasBlancas = vivas !! 1
    damasNegras = vivas !! 2
    reinasNegras = vivas !! 3
    posDamas
      | marca == "B" && not (null damasBlancas) = map (buscaPieza t)  damasBlancas
      | marca == "N" && not (null damasNegras) = map (buscaPieza t) damasNegras
      | otherwise = []
    posReinas
      | marca == "B" && not (null reinasBlancas) = map (buscaPieza t) reinasBlancas
      | marca == "N" && not (null reinasNegras) = map (buscaPieza t) reinasNegras
      | otherwise = []

mueveDama :: Tablero -> ([Pos], Bool) -> Movimientos -> (Movimientos, Bool)
mueveDama _ ([], at) movs = (movs, at)
mueveDama t (d : ds, at) movs
  | null movimientosDama = mueveDama t (ds, at) movs
  | at && not ataca = mueveDama t (ds, at) movs
  | not at && ataca = mueveDama t (ds, ataca) movimientosDama
  | otherwise = mueveDama t (ds, at) (movimientosDama ++ movs)
  where
    (validas, ataca) = casillasValidasDamas t d
    nombre = t ! d
    movimientosDama
      | null validas = []
      | otherwise = concatMap (\v -> atacaOintercambia t nombre v d) validas

mueveReina :: Tablero -> ([Pos], Bool) -> Movimientos -> (Movimientos, Bool)
mueveReina _ ([], at) movs = (movs, at)
mueveReina t (r : rs, at) movs
  | null movimientosReina = mueveReina t (rs, at) movs
  | at && not ataca = mueveReina t (rs, at) movs
  | not at && ataca = mueveReina t (rs, ataca) movimientosReina
  | otherwise = mueveReina t (rs, at) (movimientosReina ++ movs)
  where
    (validas, ataca) = casillasValidasReinas t r
    nombre = t ! r
    movimientosReina
      | null validas = []
      | otherwise = concatMap (\v -> atacaOintercambia t nombre v r) validas

atacaOintercambia :: Tablero -> String -> Pos -> Pos -> Movimientos
atacaOintercambia t nombre posNueva posAntigua
  | nuevoNombre /= nombre = [(nuevoT, posNueva)]
  | ataca = atacaPieza nuevoT nuevoNombre posNueva $ esReina nuevoNombre
  | otherwise = [(nuevoT, posNueva)]
  where
    (posPiezaEliminada, ataca) = ataque t posAntigua posNueva (esReina nombre)
    nuevoNombre = nombreActualizado t nombre posNueva
    t' = intercambiaPieza t nuevoNombre posNueva posAntigua
    nuevoT
      | ataca = eliminaPieza t' posPiezaEliminada
      | otherwise = t'

atacaPieza :: Tablero -> String -> Pos -> Bool -> Movimientos
atacaPieza t nombre p@(f, _) reina
  | null casillasAtaque = [(t, p)]
  | not reina && (null casillasAtaque || null casillas) = [(t, p)]
  | otherwise = concatMap (\v -> atacaOintercambia t nombre v p) validas
  where
    bandoP = bando nombre
    casillasAtaque = calculaCasillasAtaque t bandoP p
    casillas
      | bandoP == 'B' = filter (\(i, j) -> i < f) casillasAtaque
      | otherwise = filter (\(i, j) -> i > f) casillasAtaque
    validas
      | esReina nombre = casillasAtaque
      | otherwise = casillas

ataque :: Tablero -> Pos -> Pos -> Bool -> (Pos, Bool)
ataque t og@(x, y) atacada@(i, j) reina = (pos, ataca && ((t ! pos) `notElem` ["X",""," "]))
  where
    diferenciaFila = x - i
    diferenciaColumna = y - j
    filaE
      | diferenciaFila < 0 = i -1
      | otherwise = i + 1
    columnaE
      | diferenciaColumna < 0 = j -1
      | otherwise = j + 1
    pos = (filaE, columnaE)
    ataca
      | reina = (abs diferenciaFila >= 2) || (abs diferenciaColumna >= 2)
      | otherwise = (abs diferenciaFila == 2) || (abs diferenciaColumna == 2)

-- Puntuaciones
puntuaDamas :: Tablero -> Pos -> IO Double
puntuaDamas t pos = do
  {- Necesitamos saber el número de piezas de cada bando, si atacamos o somos atacados,
  cuánto controlamos el centro, y si tenemos ventajas posicionales en los laterales del tablero. -}
  {- Información -}
  -- Información sobre el número de piezas y bando
  let pieza = t ! pos
  let bandoActual = bando pieza
  let vivas = piezasVivas t
  let nDB = fromIntegral $ length $ cabeza "puntuaDamas" vivas
  let nRB = fromIntegral $ length $ vivas !! 1
  let nDN = fromIntegral $ length $ vivas !! 2
  let nRN = fromIntegral $ length $ vivas !! 3
  let esBlanco = bandoActual == 'B'
  let esNegro = bandoActual == 'N'
  -- Información sobre los ataques
  let longAtaque = length $ atacaPieza t pieza pos $ esReina pieza
  let alrededor = map (\p -> (t ! p, p)) $ fichasAlrededorCasilla t pos
  let longAtaques = length $ concatMap (\(nom, p) -> atacaPieza t nom p (esReina nom)) alrededor
  -- Información del tablero
  let (mi, ma) = rangos t
  let distanciaAlCentro = div (div ma 2) 2
  let limiteInf = mi + distanciaAlCentro
  let limiteSup = ma - distanciaAlCentro
  let psLaterales = [(x, y) | x <- [mi .. ma], y <- [mi], even (x + y)] ++ [(x, y) | x <- [mi .. ma], y <- [ma], even (x + y)]
  -- Información sobre la victoria
  let ganador = bandoGanador t vivas
  {- Cálculo de las puntuaciones -}
  let puntuacionPiezas
        | esBlanco = valorReina * nRB + valorDama * nDB - (valorReina * nRN + valorDama * nDN)
        | esNegro = valorReina * nRN + valorDama * nDN - (valorReina * nRB + valorDama * nDB)
        | otherwise = 0.0
  let puntuacionAtaca
        | longAtaque >= 2 = valorAtaque * fromIntegral (longAtaque - 1)
        | otherwise = 0.0
  let puntuacionAtacado
        | longAtaques > length alrededor = - (valorAtacado * fromIntegral (longAtaques - length alrededor))
        | otherwise = 0.0
  let puntuacionCentral
        -- Posición de la pieza si pertenece a las posiciones centrales
        | pos `elem` [(x, y) | x <- [limiteInf .. limiteSup], y <- [limiteInf .. limiteSup], even (x + y)] = penalizacionCentral
        | otherwise = 0.0
  let puntuacionTerreno
        | pos `elem` psLaterales && longAtaque >= 2 = valorAprovechamientoTerreno
        | otherwise = 0.0
  -- Cálculo y asignación final de puntuación
  let puntuacion
        | (esBlanco && ganador == "blancas") || (esNegro && ganador == "negras") = 1000.0
        | (esBlanco && ganador == "negras") || (esNegro && ganador == "blancas") = -1000.0
        | otherwise = puntuacionPiezas + puntuacionAtaca + puntuacionAtacado
  return puntuacion

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

distribucionOpciones :: Point
distribucionOpciones = (-450.0, 130.0)

infoEstatica :: [[String]]
infoEstatica = [dif, turnoYmarca]
  where
    dif = ["Random", "Lowest", "Easy", "Medium", "Hard"]
    turnoYmarca = ["White", "Black"]

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
cambiaOpcion raton mundo nivel opcion
  | nivel == 0 = do
    let dif = traduceDif opcion
    let prof = traduceProf opcion
    return $ ponDificultad (ponProfundidad mundo prof) dif
  | nivel == 1 = return $ ponMarca mundo $ traduceMarca opcion
  | nivel == 99 = return mundo
  | otherwise = error "El nivel de opciones especificado para la función cambiaOpción del juego de las damas no existe."

creaTableroConOpciones :: Mundo -> Mundo
creaTableroConOpciones mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional)
  | marca == "B" = (inicial (turnoApos turno), juego, dif, p, marca, turno, seleccionado, False, ad)
  | otherwise = (inicial (turnoApos turno), juego, dif, p, "N", turno, seleccionado, True, ad)
  where
    p
      | prof == 0 = 1
      | otherwise = prof
    ad = [nombresBlancas, nombresReinasBlancas, nombresNegras, nombresReinasNegras]

accionRealizada :: Mundo -> (String, Point) -> String -> Bool -> IO Mundo
accionRealizada mundo (pulsado, accion) temporal hayPartidas
  | pulsado == etiquetaOpciones = return $ (iniciaOpciones . dameJuego) mundo
  | pulsado == etiquetaCargar && hayPartidas = return menuCargarPartida
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
  | (damaSeleccionada && (posSeñalada `elem` validasDamas)) || (reinaSeleccionada && (posSeñalada `elem` validasReinas)) = do
    calculaMundo posSeñalada mundo
  | ((marca == "B") && elBlancas) || ((marca == "N") && elNegras) = do
    return $ ponSeleccionado (ponEsMaquina mundo False) el
  | otherwise = return mundo
  where
    estado = (fst . dameMovimiento) mundo
    marca = dameMarca mundo
    seleccionado = dameSeleccionado mundo
    (base, lim) = rangos estado
    relacionadas = zip (toList matrizPosiciones) [(i, j) | i <- [base .. lim], j <- [base .. lim]]
    posSeñalada = (snd . cabeza "calculaNuevoEstado" . filter (\(c, p) -> c == casilla)) relacionadas
    (validasDamas, validasReinas) = casillasDisponiblesParaElJugador mundo
    -- Hacemos algunas preguntas sobre la pieza señalada
    damaSeleccionada = (seleccionado `elem` nombresBlancas) || (seleccionado `elem` nombresNegras)
    reinaSeleccionada = (seleccionado `elem` nombresReinasBlancas) || (seleccionado `elem` nombresReinasNegras)
    el = estado ! posSeñalada
    elBlancas = el `elem` (nombresBlancas ++ nombresReinasBlancas)
    elNegras = el `elem` (nombresNegras ++ nombresReinasNegras)

casillasDisponiblesParaElJugador :: Mundo -> ([Pos], [Pos])
casillasDisponiblesParaElJugador mundo = (validasDamas, validasReinas)
  where
    estado = (fst . dameMovimiento) mundo
    marca = dameMarca mundo
    seleccionado = dameSeleccionado mundo
    posPieza = buscaPieza estado seleccionado
    (posDamas, posReinas) = posicionesSegunBando estado marca $ piezasVivas estado
    psYatsDamas
      | null posDamas = [([], False)]
      | otherwise = map (casillasValidasDamas estado) posDamas
    psYatsReinas
      | null posReinas = [([], False)]
      | otherwise = map (casillasValidasReinas estado) posReinas
    atacanDamas = any snd psYatsDamas
    atacanReinas = any snd psYatsReinas
    casillasDama
      | null seleccionado = ([], False)
      | otherwise = casillasValidasDamas estado posPieza
    casillasReina
      | null seleccionado = ([], False)
      | otherwise = casillasValidasReinas estado posPieza
    validasDamas
      | (not (snd casillasDama) && atacanDamas) || (not (snd casillasDama) && atacanReinas) = []
      | otherwise = fst casillasDama
    validasReinas
      | (not (snd casillasReina) && atacanReinas) || (not (snd casillasReina) && atacanDamas) = []
      | otherwise = fst casillasReina

calculaMundo :: Pos -> Mundo -> IO Mundo
calculaMundo casilla mundo = do
  let estado = (fst . dameMovimiento) mundo
  let marca = dameMarca mundo
  let seleccionado = dameSeleccionado mundo
  let posAntigua = buscaPieza estado seleccionado
  let (posEliminada, ataca) = ataque estado posAntigua casilla $ esReina seleccionado
  let e
        | ataca = intercambiaPieza estado seleccionado casilla posAntigua
        | otherwise = estado
  let e' = eliminaPieza e posEliminada
  let nuevoNombre = nombreActualizado e' seleccionado casilla
  let nuevoEstado = setElem nuevoNombre casilla e'
  let casillasAtaque = calculaCasillasAtaque nuevoEstado (bando seleccionado) casilla
  let sel
        | null casillasAtaque || not ataca = ""
        | otherwise = nuevoNombre
  let tocaMaquina = null casillasAtaque || not ataca || nuevoNombre /= seleccionado
  let ad = piezasVivas nuevoEstado
  punt <- puntuaDamas nuevoEstado casilla
  let ad'
        | (marca == "B" && punt > 0) || (marca == "N" && punt > 0) = ad ++ [["humano"]]
        | (marca == "B" && punt < 0) || (marca == "N" && punt < 0) = ad ++ [["maquina"]]
        | otherwise = ad ++ [["empate"]]
  let ad''
        | finDamas nuevoEstado = ad'
        | otherwise = ad
  return $ ponMovimiento (ponSeleccionado (ponEsMaquina (ponAdicional mundo ad'') tocaMaquina) sel) (nuevoEstado, casilla)

pintaMarca :: Pos -> Tablero -> Picture
pintaMarca pos estado
  | marca `elem` nombresBlancas = pintaBlanca lugar
  | marca `elem` nombresNegras = pintaNegra lugar
  | marca `elem` nombresReinasBlancas = pintaReinaBlanca lugar
  | marca `elem` nombresReinasNegras = pintaReinaNegra lugar
  | otherwise = blank
  where
    marca = estado ! pos
    lugar = matrizPosiciones ! pos

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones auxiliares
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

añadePiezas :: Pos -> String
añadePiezas actual@(x, y)
  | actual `elem` posicionesInicialesNegras = "N" ++ show x ++ show y
  | actual `elem` posicionesInicialesBlancas = "B" ++ show x ++ show y
  | even suma = " "
  | otherwise = "X"
  where
    suma = uncurry (+) actual

marcaMaquinaDamas :: String -> String
marcaMaquinaDamas marca = if marca == "B" then "N" else "B"

nombreActualizado :: Tablero -> String -> Pos -> String
nombreActualizado t nombre pos = actualizado
  where
    cab = cabeza "nombreActualizado" nombre
    bandoP = bando nombre
    (mi, ma) = rangos t
    actualizado
      | (cab /= 'R') && (bandoP == 'B') && (fst pos == mi) = 'R' : nombre
      | (cab /= 'R') && (bandoP == 'N') && (fst pos == ma) = 'R' : nombre
      | otherwise = nombre

piezasVivas :: Tablero -> [[String]]
piezasVivas t = [filter (`elem` nombresBlancas) listaTablero, filter (`elem` nombresReinasBlancas) listaTablero,
  filter (`elem` nombresNegras) listaTablero, filter (`elem` nombresReinasNegras) listaTablero]
  where
    listaTablero = toList t

bando :: String -> Char
bando pieza
  | cab == 'R' = pieza !! 1
  | otherwise = cab
  where
    cab
      | null pieza = ' '
      | otherwise = cabeza "bando" pieza

esReina :: String -> Bool
esReina pieza = cabeza "esReina" pieza == 'R'

uneFilas :: [Point] -> [Point] -> [Point]
uneFilas fi fp = take tam fi ++ take tam fp ++ uneFilas (drop tam fi) (drop tam fp)
  where
    tam = round tamMatriz

uneCasillas :: [Point] -> [Point] -> [Point]
uneCasillas as bs = concat [[a, b] | (a, b) <- zip as bs]

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
    diferencia = abs (lf - lc)
    ceros = concat $ replicate (abs (lf - lc)) "0"
    turno
      | lf > lc = fs ++ ceros ++ cs
      | otherwise = ceros ++ fs ++ cs

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
  "Easy" -> 3
  "Medium" -> 4
  "Hard" -> 5
  _ -> 1

traduceMarca :: String -> String
traduceMarca marca = case marca of
  "White" -> "B"
  "Black" -> "N"
  _ -> "Fallo"

pintaBlanca :: Point -> Picture
pintaBlanca (x, y) = translate x y dama
  where
    dama = color white formaPeon

pintaNegra :: Point -> Picture
pintaNegra (x, y) = translate x y dama
  where
    dama = color black formaPeon

pintaReinaBlanca :: Point -> Picture
pintaReinaBlanca (x, y) = translate x y dama
  where
    dama = color white formaReina

pintaReinaNegra :: Point -> Picture
pintaReinaNegra (x, y) = translate x y dama
  where
    dama = color black formaReina

formaPeon :: Picture
formaPeon = pictures [circulo, triangulo]
  where
    tam = diferenciaParaCasillas / 2
    circulo = translate 0.0 tam $ circleSolid (tam / 2)
    triangulo = polygon [(0.0, tam), (- tam, 0.0), (tam, 0.0)]

formaReina :: Picture
formaReina = pictures [circulo, rectangulo, triangulo]
  where
    tam = diferenciaParaCasillas / 2
    circulo = translate 0.0 (1.5 * tam) $ circleSolid (tam / 2)
    rectangulo = translate 0.0 (tam / 2) $ rectangleSolid (tam / 2) tam
    triangulo = polygon [(0.0, tam), (- tam, 0.0), (tam, 0.0)]

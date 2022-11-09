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
    calculaNuevoEstado,
    casillasDisponiblesParaElJugador,
    posBoton,
    anchoBoton,
    altoBoton,
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
posicionesInicialesBlancas = [(6, 2), (6, 4), (6, 6), (6, 8), (7, 1), (7, 3), (7, 5), (7, 7), (8, 2), (8, 4), (8, 6), (8, 8)]

posicionesInicialesNegras = [(1, 1), (1, 3), (1, 5), (1, 7), (2, 2), (2, 4), (2, 6), (2, 8), (3, 1), (3, 3), (3, 5), (3, 7)]

nombresBlancas = ["B62", "B64", "B66", "B68", "B71", "B73", "B75", "B77", "B82", "B84", "B86", "B88"]

nombresNegras = ["N11", "N13", "N15", "N17", "N22", "N24", "N26", "N28", "N31", "N33", "N35", "N37"]

nombresReinasBlancas = map ('R' :) nombresBlancas

nombresReinasNegras = map ('R' :) nombresNegras

inicial :: Pos -> Movimiento
inicial pos = (t, pos)
  where
    tam = round tamMatriz
    t = matrix tam tam $ \(i, j) -> añadePiezas (i, j)

añadePiezas :: Pos -> String
añadePiezas actual@(x, y)
  | actual `elem` posicionesInicialesNegras = "N" ++ show x ++ show y
  | actual `elem` posicionesInicialesBlancas = "B" ++ show x ++ show y
  | even suma = " "
  | otherwise = "X"
  where
    suma = uncurry (+) actual

-- Fin de partida
finDamas :: Tablero -> Bool
finDamas t = (null dB && null rB) || (null dN && null rN)
  where
    vivas = piezasVivas t
    dB = cabeza "finDamas" vivas
    rB = vivas !! 1
    dN = vivas !! 2
    rN = vivas !! 3

-- Movimientos
casillasValidasDamas :: Tablero -> Pos -> ([Pos], Bool)
casillasValidasDamas m posPieza@(f, c) = casillasPosibles
  where
    bandoP = cabeza "casillasVaciasDamas" $ m ! posPieza
    casillasAlrededor = casillasAlrededorFicha m posPieza
    casillas
      | bandoP == 'B' = filter (\(i, j) -> i < f) casillasAlrededor
      | bandoP == 'N' = filter (\(i, j) -> i > f) casillasAlrededor
      | otherwise = []
    casillasAtaque
      | bandoP == 'B' = filter (\(i, j) -> i < f) $ calculaCasillasAtaque m bandoP posPieza
      | bandoP == 'N' = filter (\(i, j) -> i > f) $ calculaCasillasAtaque m bandoP posPieza
      | otherwise = []
    -- Obligación de la captura
    casillasPosibles
      | null casillasAtaque = (casillas, False)
      | otherwise = (casillasAtaque, True)

casillasValidasReinas :: Tablero -> Pos -> ([Pos], Bool)
casillasValidasReinas m p = (diagonales, atacamos)
  where
    (mi, ma) = rangos m
    pieza = m ! p
    resultados
      | pieza == "" || pieza == " " = []
      | otherwise = [revisaDiagonal m p (a, b) pieza ([], False) | a <- [mi, ma], b <- [mi, ma]]
    atacamos = any snd resultados
    posiciones
      | atacamos && not (null resultados) = filter (\(ps, a) -> not (null ps) && a) resultados
      | not (null resultados) = filter (\(ps, _) -> not (null ps)) resultados
      | otherwise = []
    diagonales = nub $ concatMap (\(ps, _) -> nub ps) posiciones

calculaCasillasAtaque :: Tablero -> Char -> Pos -> [Pos]
calculaCasillasAtaque m bando posPieza = filter (compruebaAtaque m bando posPieza) $ casillasSegundoNivel m posPieza

compruebaAtaque :: Tablero -> Char -> Pos -> Pos -> Bool
compruebaAtaque m bandoP posPieza@(f, c) posAtaque@(af, ac) = condiciones
  where
    reina = esReina $ m ! posPieza
    (posEliminada, _) = ataque posPieza posAtaque reina
    piezaEliminada = m ! posEliminada
    bandoE = bando piezaEliminada
    bandoContrario = (bandoE == 'B' && bandoP == 'N') || (bandoE == 'N' && bandoP == 'B')
    condicionesBasicas = piezaEliminada /= "X" && piezaEliminada /= "" && piezaEliminada /= " " && bandoContrario
    condiciones
      | not reina && bandoP == 'B' = (f > af) && condicionesBasicas
      | not reina && bandoP == 'N' = (f < af) && condicionesBasicas
      | otherwise = condicionesBasicas

revisaDiagonal :: Tablero -> Pos -> Pos -> String -> ([Pos], Bool) -> ([Pos], Bool)
revisaDiagonal m p@(f, c) lims@(a, b) pieza (ps, at)
  | not (dentroDelTablero p m) = error mensajeError
  | mismoNombre && dentroDelTablero pos m = revisaDiagonal m pos lims pieza (ps, at)
  | tocaLimite && casillaVacia m p = (p : ps, at)
  | tocaLimite || mismoBando || (bandoContrario && not ataca) = (ps, at)
  | ataca || at = (pAtacadas, True)
  | otherwise = revisaDiagonal m pos lims pieza (p : ps, at)
  where
    mensajeError = "La posicion pasada de la pieza en revisaDiagonal es " ++ show p ++ "que esta fuera del tablero."
    tocaLimite = f == a || c == b
    nombre = m ! p
    mismoNombre = nombre == pieza
    bandoActual = bando nombre
    bandoPieza = bando pieza
    mismoBando = bandoActual == bandoPieza
    bandoContrario = (bandoActual == 'B' && bandoPieza == 'N') || (bandoActual == 'N' && bandoPieza == 'B')
    diferenciaFila = f - a
    diferenciaColumna = c - b
    filaE
      | diferenciaFila < 0 = f + 1
      | otherwise = f - 1
    columnaE
      | diferenciaColumna < 0 = c + 1
      | otherwise = c - 1
    pos = (filaE, columnaE)
    ataca = bandoContrario && dentroDelTablero pos m && casillaVacia m pos
    pAtacadas
      | ataca && at = pos : ps
      | otherwise = [pos]

movsDamas :: Tablero -> String -> Movimientos
movsDamas t marca
  | atD && not atR = movimientosDamas
  | not atD && atR = movimientosReinas
  | otherwise = movimientosDamas ++ movimientosReinas
  where
    vivas = piezasVivas t
    (posDamas, posReinas) = posicionesSegunBando t marca vivas
    (mvD, atD)
      | null posDamas = ([], False)
      | otherwise = mueveDama t (posDamas, False) []
    (mvR, atR)
      | null posReinas = ([], False)
      | otherwise = mueveReina t (posReinas, False) []
    movimientosDamas = nub mvD
    movimientosReinas = nub mvR

posicionesSegunBando :: Tablero -> String -> [[String]] -> ([Pos], [Pos])
posicionesSegunBando t marca vivas = (posDamas, posReinas)
  where
    damasBlancas = cabeza "movsDamas" vivas
    reinasBlancas = vivas !! 1
    damasNegras = vivas !! 2
    reinasNegras = vivas !! 3
    posDamas
      | marca == "B" && not (null damasBlancas) = [buscaPieza t m | m <- damasBlancas]
      | marca == "N" && not (null damasNegras) = [buscaPieza t m | m <- damasNegras]
      | otherwise = []
    posReinas
      | marca == "B" && not (null reinasBlancas) = [buscaPieza t m | m <- reinasBlancas]
      | marca == "N" && not (null reinasNegras) = [buscaPieza t m | m <- reinasNegras]
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
  | ataca && reina = atacaPieza nuevoT nuevoNombre posNueva reina
  | ataca && not reina = atacaPieza nuevoT nuevoNombre posNueva reina
  | otherwise = [(nuevoT, posNueva)]
  where
    (posPiezaEliminada, ataca) = ataque posNueva posAntigua (esReina nombre)
    nuevoNombre = nombreActualizado t nombre posNueva
    reina = cabeza "atacaOintercambia" nuevoNombre == 'R'
    t' = intercambiaPieza t nuevoNombre posNueva posAntigua
    nuevoT
      | ataca = eliminaPieza t' posPiezaEliminada
      | otherwise = t'

atacaPieza :: Tablero -> String -> Pos -> Bool -> Movimientos
atacaPieza t nombre p@(f, c) esReina
  | null casillasAtaque = [(t, p)]
  | not esReina && (null casillasAtaque || null casillas) = [(t, p)]
  | otherwise = concatMap (\v -> atacaOintercambia t nuevoNombre v p) validas
  where
    nuevoNombre = nombreActualizado t nombre p
    cab = cabeza "atacaPieza" nuevoNombre
    bandoP = bando nuevoNombre
    casillasAtaque = calculaCasillasAtaque t bandoP p
    casillas
      | bandoP == 'B' = filter (\(i, j) -> i < f) casillasAtaque
      | otherwise = filter (\(i, j) -> i > f) casillasAtaque
    validas
      | cab == 'R' = casillasAtaque
      | otherwise = casillas

ataque :: Pos -> Pos -> Bool -> (Pos, Bool)
ataque og@(x, y) atacada@(i, j) reina = (pos, ataca)
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
puntuaDamas :: Tablero -> Pos -> Double
puntuaDamas t pos
  | finDamas t = 10.0
  | otherwise = 0.0

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
    dif = ["Aleatoria", "Mínima", "Fácil", "Normal", "Difícil"]
    turnoYmarca = ["B", "N"]

alturasEstaticas :: [Float]
alturasEstaticas = [dif, turnosYmarcas]
  where
    dif = alturasCasillas !! 2
    turnosYmarcas = alturasCasillas !! 5

posBoton :: (Float, Float)
posBoton = (ancho, (- ancho) + ajusteInicial)

anchoBoton :: Float
anchoBoton = 130.0

altoBoton :: Float
altoBoton = 40.0

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
cambiaOpcion raton mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional) nivel opcion
  | nivel == 0 = do
    let nuevoMundo = (mov, juego, traduceDif opcion, traduceProf opcion, marca, turno, seleccionado, esMaquina, adicional)
    return nuevoMundo
  | nivel == 1 = do
    let nuevoMundo = (mov, juego, dif, prof, opcion, turno, seleccionado, esMaquina, adicional)
    return nuevoMundo
  | nivel == 99 = return mundo
  | otherwise = error "El nivel de opciones especificado para la función cambiaOpción del juego del gato no existe."

creaTableroConOpciones :: Mundo -> Mundo
creaTableroConOpciones mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional)
  | marca == "B" = (inicial (turnoApos turno), juego, dif, p, marca, turno, seleccionado, False, ad)
  | otherwise = (inicial (turnoApos turno), juego, dif, p, "N", turno, seleccionado, True, ad)
  where
    p
      | prof == 0 = 1
      | otherwise = prof
    ad = [nombresBlancas, nombresReinasBlancas, nombresNegras, nombresReinasNegras]

calculaNuevoEstado :: Point -> Mundo -> IO Mundo
calculaNuevoEstado casilla mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional)
  | damaSeleccionada && (posSeñalada `elem` validasDamas) = do
    calculaMundo posSeñalada mundo
  | reinaSeleccionada && (posSeñalada `elem` validasReinas) = do
    calculaMundo posSeñalada mundo
  | ((marca == "B") && elBlancas) || ((marca == "N") && elNegras) = do
    return (mov, juego, dif, prof, marca, turno, el, False, adicional)
  | otherwise = do
    let (mi, ma) = rangos estado
    let resultados = [revisaDiagonalio estado posPieza (a, b) seleccionado ([], False) | a <- [mi, ma], b <- [mi, ma]]
    a1 <- head resultados
    a2 <- resultados !! 1
    a3 <- resultados !! 2
    a4 <- resultados !! 3
    print "-----------------------Diagonalio y casillas validas-----------------------------"
    print posPieza
    print $ show a1 ++ show a2 ++ show a3 ++ show a4
    print $ casillasValidasDamas estado posPieza
    print $ casillasValidasReinas estado posPieza
    return mundo
  where
    -- Inicializamos los datos que no tenemos aún y que vamos a necesitar
    t = round tamMatriz
    ps = [(i, j) | i <- [1 .. t], j <- [1 .. t]]
    matrz = toList matrizPosiciones
    relacionadas = zip matrz ps
    posSeñalada = snd $ cabeza "calculaNuevoEstado" $ filter (\(c, p) -> c == casilla) relacionadas
    posPieza = buscaPieza estado seleccionado
    (validasDamas, validasReinas) = casillasDisponiblesParaElJugador mundo
    -- Hacemos algunas preguntas sobre la pieza señalada
    damaSeleccionada = (seleccionado `elem` nombresBlancas) || (seleccionado `elem` nombresNegras)
    reinaSeleccionada = (seleccionado `elem` nombresReinasBlancas) || (seleccionado `elem` nombresReinasNegras)
    el = estado ! posSeñalada
    elBlancas = (el `elem` nombresBlancas) || (el `elem` nombresReinasBlancas)
    elNegras = (el `elem` nombresNegras) || (el `elem` nombresReinasNegras)

casillasDisponiblesParaElJugador :: Mundo -> ([Pos], [Pos])
casillasDisponiblesParaElJugador ((estado, _), _, _, _, marca, _, seleccionado, _, _) = (validasDamas, validasReinas)
  where
    ad = piezasVivas estado
    posPieza = buscaPieza estado seleccionado
    (posDamas, posReinas) = posicionesSegunBando estado marca ad
    psYatsDamas
      | null posDamas = [([], False)]
      | otherwise = map (casillasValidasDamas estado) posDamas
    psYatsReinas
      | null posReinas = [([], False)]
      | otherwise = map (casillasValidasReinas estado) posReinas
    atacanDamas
      | any snd psYatsDamas = True
      | otherwise = False
    atacanReinas
      | any snd psYatsReinas = True
      | otherwise = False
    casillasDama = casillasValidasDamas estado posPieza
    casillasReina = casillasValidasReinas estado posPieza
    validasDamas
      | not (snd casillasDama) && atacanDamas = []
      | not (snd casillasDama) && atacanReinas = []
      | otherwise = fst casillasDama
    validasReinas
      | not (snd casillasReina) && atacanReinas = []
      | not (snd casillasReina) && atacanDamas = []
      | otherwise = fst casillasReina

revisaDiagonalio :: Tablero -> Pos -> Pos -> String -> ([Pos], Bool) -> IO ([Pos], Bool)
revisaDiagonalio m p@(f, c) lims@(a, b) pieza (ps, at)
  | not (dentroDelTablero p m) = error mensajeError
  | mismoNombre && dentroDelTablero pos m = do
    print "pasa por el primero"
    revisaDiagonalio m pos lims pieza (ps, at)
  | tocaLimite && casillaVacia m p = do
    print "lo devuelve con casilla vacia"
    return (p : ps, at)
  | tocaLimite || mismoBando || (bandoContrario && not ataca) = do
    print "lo devuelve del tiron"
    return (ps, at)
  | ataca || at = do
    print "ha habido un ataque"
    return (pAtacadas, True)
  | otherwise = do
    print "pasa por otherwise"
    revisaDiagonalio m pos lims pieza (p : ps, at)
  where
    mensajeError = "La posicion pasada de la pieza en revisaDiagonal es " ++ show p ++ "que esta fuera del tablero."
    tocaLimite = f == a || c == b
    nombre = m ! p
    mismoNombre = nombre == pieza
    bandoActual = bando nombre
    bandoPieza = bando pieza
    mismoBando = bandoActual == bandoPieza
    bandoContrario = (bandoActual == 'B' && bandoPieza == 'N') || (bandoActual == 'N' && bandoPieza == 'B')
    diferenciaFila = f - a
    diferenciaColumna = c - b
    filaE
      | diferenciaFila < 0 = f + 1
      | otherwise = f - 1
    columnaE
      | diferenciaColumna < 0 = c + 1
      | otherwise = c - 1
    pos = (filaE, columnaE)
    ataca = bandoContrario && dentroDelTablero pos m && casillaVacia m pos
    pAtacadas
      | ataca && at = pos : ps
      | otherwise = [pos]

calculaMundo :: Pos -> Mundo -> IO Mundo
calculaMundo casilla ((estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional) = do
  let posAntigua = buscaPieza estado seleccionado
  let reina = esReina seleccionado
  let (posEliminada, ataca) = ataque posAntigua casilla reina
  let e
        | ataca = eliminaPieza estado posEliminada
        | otherwise = estado
  let e' = intercambiaPieza e seleccionado casilla posAntigua
  let nuevoNombre = nombreActualizado e' seleccionado casilla
  let bandoP = bando seleccionado
  let nuevoEstado = setElem nuevoNombre casilla e'
  let casillasAtaque = calculaCasillasAtaque nuevoEstado bandoP casilla
  let sel
        | null casillasAtaque || not ataca = ""
        | otherwise = nuevoNombre
  let tocaMaquina = null casillasAtaque || not ataca
  let ad = piezasVivas nuevoEstado
  let nuevoMundo = ((nuevoEstado, casilla), juego, dif, prof, marca, turno, sel, tocaMaquina, ad)
  return nuevoMundo

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
piezasVivas t = [damasBlancas, reinasBlancas, damasNegras, reinasNegras]
  where
    listaTablero = toList t
    damasBlancas = filter (`elem` nombresBlancas) listaTablero
    reinasBlancas = filter (`elem` nombresReinasBlancas) listaTablero
    damasNegras = filter (`elem` nombresNegras) listaTablero
    reinasNegras = filter (`elem` nombresReinasNegras) listaTablero

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

cerosEnCadena :: Int -> String
cerosEnCadena 0 = ""
cerosEnCadena d = "0" ++ cerosEnCadena (d -1)

traduceDif :: String -> Int
traduceDif dif
  | dif == "Mínima" = 1
  | dif == "Fácil" = 2
  | dif == "Normal" = 3
  | dif == "Difícil" = 4
  | otherwise = 0

traduceProf :: String -> Int
traduceProf dif
  | dif == "Mínima" = 1
  | dif == "Fácil" = 7
  | dif == "Normal" = 8
  | dif == "Difícil" = 10
  | otherwise = 1

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

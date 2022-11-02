module FuncionesDamas
  ( -- Funciones normales
    posicionesInicialesBlancas,
    posicionesInicialesNegras,
    nombresBlancas,
    nombresNegras,
    nombresReinasBlancas,
    nombresReinasNegras,
    inicial,
    casillasVaciasDama,
    casillasValidasDamas,
    casillasValidasReinas,
    movsDamas,
    casillasDiagonales,
    piezasVivas,
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

-- Aux
añadePiezas :: Pos -> String
añadePiezas actual@(x, y)
  | actual `elem` posicionesInicialesNegras = "N" ++ show x ++ show y
  | actual `elem` posicionesInicialesBlancas = "B" ++ show x ++ show y
  | even suma = " "
  | otherwise = "X"
  where
    suma = uncurry (+) actual

casillasVaciasDama :: Tablero -> Pos -> [Pos]
casillasVaciasDama m posPieza = casillasPosibles
  where
    casillasAlrededor = casillasAlrededorFicha m posPieza
    casillasAtaque = casillasSegundoNivel m posPieza
    -- Obligación de la captura
    casillasPosibles
      | null casillasAtaque = casillasAlrededor
      | otherwise = casillasAtaque

casillasValidasDamas :: Tablero -> Pos -> [Pos]
casillasValidasDamas m pos@(f, c) = casillas
  where
    casillasAlrededor = casillasVaciasDama m pos
    bando = head $ m ! pos
    casillas
      | bando == 'B' = filter (\(i, j) -> i < f) casillasAlrededor
      | otherwise = filter (\(i, j) -> i > f) casillasAlrededor

casillasValidasReinas :: Tablero -> Pos -> [Pos]
casillasValidasReinas = casillasDiagonales

movsDamas :: Tablero -> String -> [[String]] -> Movimientos
movsDamas t marca vivas = movimientosDamas ++ movimientosReinas
  where
    damasBlancas = cabeza "movsDamas" vivas
    reinasBlancas = vivas !! 1
    damasNegras = vivas !! 2
    reinasNegras = vivas !! 3
    posDamas
      | marca == "B" = [buscaPieza t m | m <- damasBlancas]
      | otherwise = [buscaPieza t m | m <- damasNegras]
    posReinas
      | marca == "B" = [buscaPieza t ('R' : m) | m <- reinasBlancas]
      | otherwise = [buscaPieza t ('R' : m) | m <- reinasNegras]
    movimientosDamas = nub $ mueveDama t posDamas
    movimientosReinas = nub $ mueveReina t posReinas

mueveDama :: Tablero -> [Pos] -> Movimientos
mueveDama _ [] = []
mueveDama t (d : ds)
  | null movimientosDama = mueveDama t ds
  | otherwise = movimientosDama ++ mueveDama t ds
  where
    validas = casillasValidasDamas t d
    nombre = t ! d
    movimientosDama
      | null validas = []
      | otherwise = concatMap (\v -> atacaOintercambia t nombre v d) validas

mueveReina :: Tablero -> [Pos] -> Movimientos
mueveReina _ [] = []
mueveReina t (r : rs)
  | null movimientosReina = mueveReina t rs
  | otherwise = movimientosReina ++ mueveReina t rs
  where
    validas = casillasValidasReinas t r
    nombre = t ! r
    movimientosReina
      | null validas = []
      | otherwise = concatMap (\v -> atacaOintercambia t nombre v r) validas

atacaOintercambia :: Tablero -> String -> Pos -> Pos -> Movimientos
atacaOintercambia t nombre posNueva posAntigua
  | ataca && reina = atacaPieza nuevoT nombre posNueva reina
  | ataca && not reina = atacaPieza nuevoT nombre posNueva reina
  | otherwise = [(nuevoT, posNueva)]
  where
    (posPiezaEliminada, ataca) = ataque posNueva posAntigua
    ultimaFila = fst posNueva == fst (rangos t)
    reina = cabeza "atacaOintercambia" nuevoNombre == 'R'
    nuevoNombre
      | ultimaFila && not reina = 'R' : nombre
      | otherwise = nombre
    t' = intercambiaPieza t nuevoNombre posNueva posAntigua
    nuevoT
      | ataca = eliminaPieza t' posPiezaEliminada
      | otherwise = t'

atacaPieza :: Tablero -> String -> Pos -> Bool -> Movimientos
atacaPieza t nombre p@(f,c) esReina
  | null casillasAtaque = [(t, p)]
  | not esReina && (null casillasAtaque || null casillas) = [(t, p)]
  | otherwise = concatMap (\v -> atacaOintercambia t nombre v p) validas
  where
    casillasAtaque = casillasSegundoNivel t p
    ultimaFila = fst p == fst (rangos t)
    bando
      | esReina = nombre !! 1
      | otherwise = cabeza "atacaPieza" nombre
    casillas
      | bando == 'B' = filter (\(i, j) -> i < f) casillasAtaque
      | otherwise = filter (\(i, j) -> i > f) casillasAtaque
    validas
      | esReina || ultimaFila = casillasAtaque
      | otherwise = casillas

-- Aux
ataque :: Pos -> Pos -> (Pos, Bool)
ataque (x, y) (i, j) = (pos, ataca)
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
    ataca = (abs diferenciaFila > 1) || (abs diferenciaColumna > 1)

casillasDiagonales :: Tablero -> Pos -> [Pos]
casillasDiagonales m p = diagonales
  where
    (mi, ma) = rangos m
    pieza = m ! p
    resultados = [revisaDiagonal m p (a, b) pieza | a <- [mi, ma], b <- [mi, ma]]
    -- Obligación de captura
    atacamos = filter snd resultados
    diagonales
      | null atacamos = concatMap fst $ filter (\(ps, _) -> not (null ps)) resultados
      | otherwise = concatMap fst $ filter (\(ps, _) -> not (null ps)) atacamos

-- Aux
revisaDiagonal :: Tablero -> Pos -> Pos -> String -> ([Pos], Bool)
revisaDiagonal m p@(f, c) (a, b) pieza
  | not (dentroDelTablero p m) = ([], False)
  | ((f == a || c == b) && (nombre == pieza)) || (bandoActual == bandoPieza) = ([], False)
  | f == a || c == b = ([p], ataca)
  | nombre == pieza = sig
  | ataca = (ps, ataca)
  | otherwise = (p : ps, at)
  where
    nombre = m ! p
    cabezaNombre = cabeza "revisaDiagonal" nombre
    bandoActual
      | cabezaNombre == 'R' = nombre !! 1
      | otherwise = cabezaNombre
    cabezaPieza = cabeza "revisaDiagonal" pieza
    bandoPieza
      | cabezaPieza == 'R' = pieza !! 1
      | otherwise = cabezaPieza
    diferenciaFila = f - a
    diferenciaColumna = c - b
    filaE
      | diferenciaFila < 0 = f + 1
      | otherwise = f - 1
    columnaE
      | diferenciaColumna < 0 = c + 1
      | otherwise = c - 1
    pos = (filaE, columnaE)
    ataca1 = (nombre /= pieza) && (bandoActual /= bandoPieza) && not (casillaVacia m p)
    ataca2 = dentroDelTablero pos m && casillaVacia m pos
    ataca = ataca1 && ataca2
    sig@(ps, at) = revisaDiagonal m pos (a, b) pieza

piezasVivas :: Tablero -> [[String]]
piezasVivas t = [damasBlancas, reinasBlancas, damasNegras, reinasNegras]
  where
    listaTablero = toList t
    damasBlancas = filter (`elem` nombresBlancas) listaTablero
    reinasBlancas = filter (`elem` nombresReinasBlancas) listaTablero
    damasNegras = filter (`elem` nombresNegras) listaTablero
    reinasNegras = filter (`elem` nombresReinasNegras) listaTablero

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones auxiliares para los gráficos
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

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

-- Aux
uneFilas :: [Point] -> [Point] -> [Point]
uneFilas fi fp = f1 ++ f2 ++ uneFilas ri rp
    where
        tam = round tamMatriz
        f1 = take tam fi
        f2 = take tam fp
        ri = drop tam fi
        rp = drop tam fp

distribucionOpciones :: Point
distribucionOpciones = (-450.0, 130.0)

alturasCasillas :: [Float]
alturasCasillas = [a, a - diferencia .. 0]
  where
    a = ancho - ajusteInicialMenu * 2
    diferencia = a / 5.0

uneCasillas :: [Point] -> [Point] -> [Point]
uneCasillas [] _ = []
uneCasillas _ [] = []
uneCasillas (a : as) (b : bs) = [a, b] ++ uneCasillas as bs

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

infoEstatica :: [[String]]
infoEstatica = [dif, turnoYmarca]
  where
    dif = ["Aleatoria", "Mínima", "Fácil", "Normal", "Difícil"]
    turnoYmarca = ["Blancas", "Negras"]

alturasEstaticas :: [Float]
alturasEstaticas = [dif, turnosYmarcas]
  where
    dif = alturasCasillas !! 2
    turnosYmarcas = alturasCasillas !! 5

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

-- -----------------------------------------------------------------------------------------------------------------------posAturno :: Pos -> Int
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

-- Aux
cerosEnCadena :: Int -> String
cerosEnCadena 0 = ""
cerosEnCadena d = "0" ++ cerosEnCadena (d -1)

-- -----------------------------------------------------------------------------------------------------------------------
-- -----------------------------------------------------------------------------------------------------------------------
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

-- Aux
traduceDif :: String -> Int
traduceDif dif
  | dif == "Mínima" = 1
  | dif == "Fácil" = 2
  | dif == "Normal" = 3
  | dif == "Difícil" = 4
  | otherwise = 0

-- Aux
traduceProf :: String -> Int
traduceProf dif
  | dif == "Mínima" = 1
  | dif == "Fácil" = 7
  | dif == "Normal" = 8
  | dif == "Difícil" = 10
  | otherwise = 1

-- -----------------------------------------------------------------------------------------------------------------------

creaTableroConOpciones :: Mundo -> Mundo
creaTableroConOpciones mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional)
  | marca == "B" = (inicial (turnoApos turno), juego, dif, p, marca, turno, seleccionado, False, ad)
  | otherwise = (mov, juego, dif, p, "N", turno, seleccionado, True, ad)
  where
    p
      | prof == 0 = 1
      | otherwise = prof
    ad = [nombresBlancas, nombresReinasBlancas, nombresNegras, nombresReinasNegras]

-- -----------------------------------------------------------------------------------------------------------------------
calculaNuevoEstado :: Point -> Mundo -> IO Mundo
calculaNuevoEstado casilla mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional)
  | damaSeleccionada && (posSeñalada `elem` validasDamas) = do
    return $ calculaMundo posSeñalada mundo
  | reinaSeleccionada && (posSeñalada `elem` validasReinas) = do
    return $ calculaMundo posSeñalada mundo
  | ((marca == "B") && elBlancas) || ((marca == "N") && elNegras) = do
    return (mov, juego, dif, prof, marca, turno, el, False, adicional)
  | otherwise = return mundo
  where
    t = round tamMatriz
    ps = [(i, j) | i <- [1 .. t], j <- [1 .. t]]
    matrz = toList matrizPosiciones
    relacionadas = zip matrz ps
    posSeñalada = snd $ cabeza "calculaNuevoEstado" $ filter (\(c, p) -> c == casilla) relacionadas
    validasDamas = casillasValidasDamas estado posSeñalada
    validasReinas = casillasValidasReinas estado posSeñalada
    validasAmbas = validasDamas ++ validasReinas
    damaSeleccionada = (seleccionado `elem` nombresBlancas) || (seleccionado `elem` nombresNegras)
    reinaSeleccionada = (seleccionado `elem` nombresReinasBlancas) || (seleccionado `elem` nombresReinasNegras)
    el = estado ! posSeñalada
    elBlancas = (el `elem` nombresBlancas) || (el `elem` nombresReinasBlancas)
    elNegras = (el `elem` nombresNegras) || (el `elem` nombresReinasNegras)

-- Aux
calculaMundo :: Pos -> Mundo -> Mundo
calculaMundo casilla ((estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional) = nuevoMundo
  where
    posAntigua = buscaPieza estado seleccionado
    (posEliminada, ataca) = ataque casilla posAntigua
    e
      | ataca = eliminaPieza estado posEliminada
      | otherwise = estado
    nuevoEstado = intercambiaPieza e seleccionado casilla posAntigua
    casillasAtaque = casillasSegundoNivel e casilla
    sel | null casillasAtaque = ""
      | otherwise = seleccionado
    ad = piezasVivas nuevoEstado
    nuevoMundo = ((nuevoEstado, casilla), juego, dif, prof, marca, turno, sel, null casillasAtaque, ad)

-- -----------------------------------------------------------------------------------------------------------------------
-- -----------------------------------------------------------------------------------------------------------------------
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

-- Aux
pintaBlanca :: Point -> Picture
pintaBlanca (x, y) = translate x y dama
  where
    dama = color white formaPeon

-- Aux
pintaNegra :: Point -> Picture
pintaNegra (x, y) = translate x y dama
  where
    dama = color black formaPeon

-- Aux
pintaReinaBlanca :: Point -> Picture
pintaReinaBlanca (x, y) = translate x y dama
  where
    dama = color white formaReina

-- Aux
pintaReinaNegra :: Point -> Picture
pintaReinaNegra (x, y) = translate x y dama
  where
    dama = color black formaReina

-- Aux
formaPeon :: Picture
formaPeon = pictures [circulo, triangulo]
  where
    tam = diferenciaParaCasillas / 2
    circulo = translate 0.0 tam $ circleSolid (tam / 2)
    triangulo = polygon [(0.0, tam), (- tam, 0.0), (tam, 0.0)]

-- Aux
formaReina :: Picture
formaReina = pictures [circulo, rectangulo, triangulo]
  where
    tam = diferenciaParaCasillas / 2
    circulo = translate 0.0 tam $ circleSolid (tam / 4)
    rectangulo = translate 0.0 (tam / 2) $ rectangleSolid (tam / 4) (tam / 2)
    triangulo = polygon [(0.0, tam / 2), (- tam / 2, 0.0), (tam / 2, 0.0)]

-- -----------------------------------------------------------------------------------------------------------------------

posBoton :: (Float, Float)
posBoton = (ancho, (- ancho) + ajusteInicial)

anchoBoton :: Float
anchoBoton = 130.0

altoBoton :: Float
altoBoton = 40.0
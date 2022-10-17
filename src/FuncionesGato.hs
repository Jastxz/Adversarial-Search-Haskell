module FuncionesGato (
        -- Funciones normales
        posicionesInicialesGatos,
        falsoInicial,
        inicial,
        casillasVaciasRaton,
        ratonEncerrado,
        ratonEscapado,
        movsGato,
        intercambiaPieza,
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
        pintaComienzoTablero,
        pintaMarca
    ) where

import Data.Matrix
import Data.List (nub,reverse)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Tipos
import Utiles
import UtilesGraficos

posicionesInicialesGatos = [(8,2),(8,4),(8,6),(8,8)]

falsoInicial :: Tablero
falsoInicial = matrix tam tam $ \(i,j) -> añadePiezas (i,j)
    where
        tam = round tamMatriz

inicial :: Pos -> Movimiento
inicial pos = (t,pos)
    where
        t = setElem "R" pos falsoInicial

añadePiezas :: Pos -> String
añadePiezas actual
    | actual `elem` posicionesInicialesGatos = "G" ++ show (snd actual)
    | even suma = " "
    | otherwise = "X"
        where
            suma = uncurry (+) actual

casillasVaciasRaton :: Tablero -> [Pos]
casillasVaciasRaton m = filter (\ c -> (m ! c) == " ") casillasAlrededor
    where
        (min,max) = rangos m
        casillas = [(i,j) | i<-[min..max], j<-[min..max]]
        raton = filter (\pos -> (m ! pos) == "R") casillas
        (f,c) | null raton = (1,1) | otherwise = cabeza "casillasVaciasRaton" raton
        casillasAlrededor = casillasAlrededorFicha m (f,c)

casillasVaciasGatos :: Tablero -> [Pos]
casillasVaciasGatos m = filter (\ c -> (m ! c) == " ") casillasAlrededor
    where
        (min,max) = rangos m
        casillas = [(i,j) | i<-[min..max], j<-[min..max]]
        casillasGatos = filter (\pos -> (m ! pos) `elem` ["G2","G4","G6","G8"]) casillas
        casillasAlrededor = concatMap (casillasValidasGatos m) casillasGatos

casillasValidasGatos :: Tablero -> Pos -> [Pos]
casillasValidasGatos m pos@(f,c) = filter (\(i,j) -> i < f) casillasAlrededor
    where
        casillasAlrededor = casillasAlrededorFicha m pos

ratonEncerrado :: Tablero -> Bool
ratonEncerrado t = null (casillasVaciasRaton t)

ratonEscapado :: Tablero -> Bool
ratonEscapado t = filaRaton >= filaGato
    where
        (rmen, rmay) = rangos t
        posiciones = [(i,j) | i<-[rmen..rmay], j<-[rmen..rmay]]
        filaRaton = fst $ cabeza "ratonEscapado" $ filter (\p -> (t ! p) == "R") posiciones
        filaGato = maximum $ map fst $ filter (\p -> (t ! p) `elem` ["G2","G4","G6","G8"]) posiciones

movsGato :: Tablero -> String -> Movimientos
movsGato t marca
    | marca == "R" = nub $ map (mueveRaton t) (casillasVaciasRaton t)
    | otherwise = nub $ mueveGato t (casillasVaciasGatos t)

mueveRaton :: Tablero -> Pos -> Movimiento
mueveRaton t pos
    | length estaAlrededor == 1 = (intercambiaPieza t "R" pos (cabeza "mueveRaton" estaAlrededor), pos)
    | otherwise = (t,pos)
        where
            validas = casillasAlrededorFicha t pos
            estaAlrededor = filter (\p -> (t ! p) == "R") validas

mueveGato :: Tablero -> [Pos] -> Movimientos
mueveGato _ [] = []
mueveGato t (p:ps)
    | tamG == 1 = (intercambiaPieza t (t ! cabeza "mueveRaton" gatosAlrededor) p (cabeza "mueveRaton" gatosAlrededor), p) : mueveGato t ps
    | tamG > 1 = intercambia2piezas t gatosAlrededor p ++ mueveGato t ps
    | otherwise = (t,p) : mueveGato t ps
        where
            validas = casillasAlrededorFicha t p
            gatosAlrededor = filter (\v -> (t ! v) `elem` ["G2","G4","G6","G8"]) validas
            tamG = length gatosAlrededor

intercambiaPieza :: Tablero -> String -> Pos -> Pos -> Tablero
intercambiaPieza t pieza posNueva posAntigua = setElem pieza posNueva $ setElem " " posAntigua t

intercambia2piezas :: Tablero -> [Pos] -> Pos -> Movimientos
intercambia2piezas t gatosAlrededor pos = [(intercambiaPieza t (t ! g) pos g, pos) | g<-gatosAlrededor]

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones que tienen que ver con la escritura y lectura en consola.
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones auxiliares para los gráficos
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

tamMatriz :: Float
tamMatriz = 8.0

tamTablero :: Float
tamTablero = 500.0

ancho :: Float
ancho = tamTablero/2.0

diferenciaParaCasillas :: Float
diferenciaParaCasillas = tamTablero/tamMatriz

ajusteInicial :: Float
ajusteInicial = ancho/tamMatriz

ajusteInicialMenu :: Float
ajusteInicialMenu = ancho / (2*tamMatriz)

alturaTablero :: Float
alturaTablero = -125.0

origenMinitableros :: Float
origenMinitableros = ancho * 0.75

diferenciaParaMiniCasillas :: Float
diferenciaParaMiniCasillas = origenMinitableros/tamMatriz

miniAncho :: Float
miniAncho = origenMinitableros/2

miniAjusteInicial :: Float
miniAjusteInicial = (ancho/tamMatriz)/2.75

matrizPosiciones :: Matrix Point
matrizPosiciones = matrix t t $ \p -> fst (cabeza "matrizPosiciones" (filter (\(cas,pos) -> pos==p) relacion))
    where
        t = round tamMatriz
        ps = [(i,j) |i<-[1..t],j<-[1..t]]
        d = diferenciaParaCasillas
        a = (-ancho) + ajusteInicial
        anchurasImpares = [a,a+(2*d)..a+((tamMatriz-2.0)*d)]
        anchurasPares = [a+d,a+(3*d)..a+((tamMatriz-1.0)*d)]
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
uneCasillas (a:as) (b:bs) = [a,b] ++ uneCasillas as bs

casillasNegras :: [Point]
casillasNegras = uneCasillas cuadradosImpares cuadradosPares
    where
        d = diferenciaParaCasillas
        a = (-ancho) + ajusteInicial
        anchurasImpares = [a,a+(2*d)..a+((tamMatriz-2.0)*d)]
        anchurasPares = [a+d,a+(3*d)..a+((tamMatriz-1.0)*d)]
        alturasImpares = reverse anchurasPares
        alturasPares = reverse anchurasImpares
        cuadradosImpares = [(j, i) | i <- alturasImpares, j <- anchurasPares]
        cuadradosPares = [(j, i) | i <- alturasPares, j <- anchurasImpares]

casillasBlancas :: [Point]
casillasBlancas = uneCasillas cuadradosImpares cuadradosPares
    where
        d = diferenciaParaCasillas
        a = (-ancho) + ajusteInicial
        anchurasImpares = [a,a+(2*d)..a+((tamMatriz-2.0)*d)]
        anchurasPares = [a+d,a+(3*d)..a+((tamMatriz-1.0)*d)]
        alturasImpares = reverse anchurasPares
        alturasPares = reverse anchurasImpares
        cuadradosImpares = [(j, i) | i <- alturasImpares, j <- anchurasImpares]
        cuadradosPares = [(j, i) | i <- alturasPares, j <- anchurasPares]

infoEstatica :: [[String]]
infoEstatica = [dif, turnoYmarca]
    where
        dif = ["Aleatoria", "Mínima", "Fácil", "Normal", "Difícil"]
        turnoYmarca = ["R", "G"]

alturasEstaticas :: [Float]
alturasEstaticas = [dif, turnosYmarcas]
    where
        dif = alturasCasillas !! 2
        turnosYmarcas = alturasCasillas !! 5

turnoApos :: Int -> Pos
turnoApos turno
 | turno == 0 = (1,1)
 | otherwise = (f,c)
    where
        pos = show turno
        tamTurno = length pos
        componentes | even tamTurno = pos | otherwise = "0" ++ pos
        longCom = length componentes
        tamComponente = longCom `div` 2
        f = stringToInt $ take tamComponente componentes
        c = stringToInt $ drop tamComponente componentes

-- -----------------------------------------------------------------------------------------------------------------------
posAturno :: Pos -> Int
posAturno (f,c) = stringToInt turno
    where
        fs = show f
        cs = show c
        lf = length fs
        lc = length cs
        diferencia = abs (lf - lc)
        ceros = cerosEnCadena diferencia
        turno | lf > lc = fs ++ ceros ++ cs | otherwise = ceros ++ fs ++ cs

-- Aux
cerosEnCadena :: Int -> String
cerosEnCadena 0 = ""
cerosEnCadena d = "0" ++ cerosEnCadena (d-1)
-- -----------------------------------------------------------------------------------------------------------------------
-- -----------------------------------------------------------------------------------------------------------------------
cambiaOpcion :: Point -> Mundo -> Int -> String -> IO Mundo
cambiaOpcion raton@(x,y) mundo@(mov@(estado,pos),juego,dif,prof,marca,turno,seleccionado,esMaquina) nivel opcion
    | nivel == 0 = do
        let nuevoMundo = (mov,juego,traduceDif opcion,prof,marca,turno,seleccionado,esMaquina)
        return nuevoMundo
    | nivel == 1 = do
        let nuevoMundo = (mov,juego,dif,prof,opcion,turno,seleccionado,esMaquina)
        return nuevoMundo
    | nivel == 99 = do
        let ratonCorregido = (x,y-alturaTablero)
        cambiaMiniTablero ratonCorregido mundo
    | otherwise = error "El nivel de opciones especificado para la función cambiaOpción del juego del gato no existe."

-- Aux
traduceDif :: String -> Int
traduceDif dif
    | dif == "Aleatoria" = 0
    | dif == "Mínima" = 1
    | dif == "Fácil" = 2
    | dif == "Normal" = 3
    | otherwise  = 4

cambiaMiniTablero :: Point -> Mundo -> IO Mundo
cambiaMiniTablero raton mundo@(mov@(estado,pos),juego,dif,prof,marca,turno,seleccionado,esMaquina)
    | not (null pulsadas) && marca == "R" = do
        let nuevoMundo = (mov,juego,dif,prof,marca,posAturno (cabeza "cambiaMiniTablero" pulsadas),seleccionado,esMaquina)
        return nuevoMundo
    | otherwise = return mundo
        where
            posPosibles = casillasVacias $ fst $ inicial $ turnoApos turno
            casillasPosibles = map (matrizMiniPosiciones !) posPosibles
            relacion = zip casillasPosibles posPosibles
            pulsadas = map snd $ filter (\(c,p) -> pulsaCercaMini raton c) relacion
-- -----------------------------------------------------------------------------------------------------------------------

creaTableroConOpciones :: Mundo -> Mundo
creaTableroConOpciones mundo@(mov@(estado,pos),juego,dif,prof,marca,turno,seleccionado,esMaquina)
    | marca == "R" = (inicial (turnoApos turno),juego,dif,prof,marca,turno,seleccionado,esMaquina)
    | otherwise = (mov,juego,dif,prof,"G",turno,seleccionado,esMaquina)

-- -----------------------------------------------------------------------------------------------------------------------
calculaNuevoEstado :: Point -> Mundo -> IO Mundo
calculaNuevoEstado casilla mundo@(mov@(estado,pos),juego,dif,prof,marca,turno,seleccionado,esMaquina)
    | (seleccionado == "R") && (posSeñalada `elem` vaciasRaton) = do
        let posPosibles = casillasVaciasRaton estado
        let casillasPosibles = map (matrizPosiciones !) posPosibles
        let relacion = zip casillasPosibles posPosibles
        let posNueva = snd $ cabeza "calculaNuevoEstado" $ filter (\(c,p) -> c==casilla) relacion
        return $ calculaMundo posNueva mundo
    | (seleccionado `elem` ["G2","G4","G6","G8"]) && (posSeñalada `elem` vaciasGatos) = do
        let posPosibles = casillasVaciasGatos estado
        let casillasPosibles = map (matrizPosiciones !) posPosibles
        let relacion = zip casillasPosibles posPosibles
        let posNueva = snd $ cabeza "calculaNuevoEstado" $ filter (\(c,p) -> c==casilla) relacion
        return $ calculaMundo posNueva mundo
    | (marca == "R") && (el == "R") = do
        return (mov,juego,dif,prof,marca,turno,el,False)
    | (marca == "G") && (el `elem` ["G2","G4","G6","G8"]) = do
        return (mov,juego,dif,prof,marca,turno,el,False)
    | otherwise = return mundo
        where
            vaciasRaton = casillasVaciasRaton estado
            vaciasGatos = casillasVaciasGatos estado
            t = round tamMatriz
            ps = [(i,j) | i<-[1 .. t], j<-[1 .. t]]
            matrz = toList matrizPosiciones
            relacionadas = zip matrz ps
            posSeñalada = snd $ cabeza "calculaNuevoEstado" $ filter (\(c,p) -> c==casilla) relacionadas
            el = estado ! posSeñalada

-- Aux
calculaMundo :: Pos -> Mundo -> Mundo
calculaMundo casilla ((estado,pos),juego,dif,prof,marca,turno,seleccionado,esMaquina) = ((nuevoEstado,casilla),juego,dif,prof,marca,turno,"",True)
    where
        posAntigua = buscaPieza estado seleccionado
        nuevoEstado = intercambiaPieza estado seleccionado casilla posAntigua
-- -----------------------------------------------------------------------------------------------------------------------
-- -----------------------------------------------------------------------------------------------------------------------
pintaMarca :: Pos -> Tablero -> Matrix Point -> Picture
pintaMarca pos estado posiciones
    | marca == "R" = pintaRaton lugar
    | marca `elem` ["G2","G4","G6","G8"] = pintaGato lugar
    | otherwise = blank
        where
            marca = estado ! pos
            lugar = posiciones ! pos

-- Aux
pintaRaton :: Point -> Picture
pintaRaton (x,y) = translate x y raton
    where
        raton = color white formaPeon

-- Aux
pintaGato :: Point -> Picture
pintaGato (x,y) = translate x y gato
    where
        gato = color black formaPeon

-- Aux
formaPeon :: Picture
formaPeon = pictures [circulo,triangulo]
    where
        tam = diferenciaParaCasillas/2
        circulo = translate 0.0 tam $ circleSolid (tam/2)
        triangulo = polygon [(0.0,tam),(-tam,0.0),(tam,0.0)]
-- -----------------------------------------------------------------------------------------------------------------------

posBoton :: (Float, Float)
posBoton = (ancho, (-ancho) + ajusteInicial)

anchoBoton :: Float
anchoBoton = 130.0

altoBoton :: Float
altoBoton = 40.0

-- -----------------------------------------------------------------------------------------------------------------------
pintaComienzoTablero :: Movimiento -> IO Picture
pintaComienzoTablero mov@(estado,pos) = do
    let borde = rectangleWire origenMinitableros origenMinitableros
    let tam = round tamMatriz
    let cAjdrz = [1 .. tam]
    let posiciones = [(i, j) | i <- cAjdrz, j <- cAjdrz]
    let marcasDibujadas = map (\pos -> pintaMiniMarca pos estado matrizMiniPosiciones) posiciones
    let estadoDibujado = pictures marcasDibujadas
    let res = translate 0 alturaTablero $ pictures [borde,estadoDibujado]
    return res

-- Aux
matrizMiniPosiciones :: Matrix Point
matrizMiniPosiciones = matrix t t $ \p -> fst (cabeza "matrizMiniPosiciones" (filter (\(cas,pos) -> pos==p) relacion))
    where
        t = round tamMatriz
        ps = [(i,j) |i<-[1..t],j<-[1..t]]
        d = diferenciaParaMiniCasillas
        a = (-miniAncho) + miniAjusteInicial
        anchurasImpares = [a,a+(2*d)..a+((tamMatriz-2.0)*d)]
        anchurasPares = [a+d,a+(3*d)..a+((tamMatriz-1.0)*d)]
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
-- -----------------------------------------------------------------------------------------------------------------------
-- -----------------------------------------------------------------------------------------------------------------------
pintaMiniMarca :: Pos -> Tablero -> Matrix Point -> Picture
pintaMiniMarca pos estado posiciones
    | marca == "R" = pintaMiniRaton lugar
    | marca `elem` ["G2","G4","G6","G8"] = pintaMiniGato lugar
    | marca == "X" = translate i j casNegra
    | otherwise = blank
        where
            marca = estado ! pos
            lugar@(i,j) = posiciones ! pos
            casNegra = color marron $ rectangleSolid diferenciaParaMiniCasillas diferenciaParaMiniCasillas

-- Aux
pintaMiniRaton :: Point -> Picture
pintaMiniRaton (x,y) = translate x y raton
    where
        raton = color white formaMiniPeon

-- Aux
pintaMiniGato :: Point -> Picture
pintaMiniGato (x,y) = translate x y gato
    where
        gato = color black formaMiniPeon

-- Aux
formaMiniPeon :: Picture
formaMiniPeon = pictures [circulo,triangulo]
    where
        tam = diferenciaParaMiniCasillas/2
        circulo = translate 0.0 tam $ circleSolid (tam/2)
        triangulo = polygon [(0.0,tam),(-tam,0.0),(tam,0.0)]
-- -----------------------------------------------------------------------------------------------------------------------
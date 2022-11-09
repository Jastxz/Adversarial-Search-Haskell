module FuncionesGato (
        -- Funciones normales
        posicionesInicialesGatos,
        nombresGatos,
        falsoInicial,
        inicial,
        finGato,
        casillasVaciasRaton,
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

-- Inicialización
posicionesInicialesGatos = [(8,2),(8,4),(8,6),(8,8)]
nombresGatos = ["G2","G4","G6","G8"]

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

-- Fin de partida
finGato :: Tablero -> Bool
finGato t = ratonEncerrado t posRaton || ratonEscapado t posRaton posGatos
    where
        posRaton = buscaPieza t "R"
        posGatos = [buscaPieza t m | m<-nombresGatos]

ratonEncerrado :: Tablero -> Pos -> Bool
ratonEncerrado t pos = null (casillasVaciasRaton t pos)

ratonEscapado :: Tablero -> Pos -> [Pos] -> Bool
ratonEscapado t raton gatos = filaRaton >= filaGato
    where
        filaRaton = fst raton
        filaGato = maximum $ map fst gatos

-- Movimientos
casillasVaciasRaton :: Tablero -> Pos -> [Pos]
casillasVaciasRaton = casillasAlrededorFicha

casillasVaciasGatos :: Tablero -> [Pos]
casillasVaciasGatos m = concat [casillasValidasGatos m (buscaPieza m g) | g <- nombresGatos]

casillasValidasGatos :: Tablero -> Pos -> [Pos]
casillasValidasGatos m pos@(f,c) = filter (\(i,j) -> i < f) $ casillasAlrededorFicha m pos

movsGato :: Tablero -> String -> Movimientos
movsGato t marca
    | marca == "R" = nub $ map (\pos -> (intercambiaPieza t "R" pos posPieza, pos)) (casillasVaciasRaton t posPieza)
    | otherwise = nub $ mueveGato t posGatos
        where
            posPieza = buscaPieza t marca
            posGatos = [buscaPieza t m | m<-nombresGatos]

mueveGato :: Tablero -> [Pos] -> Movimientos
mueveGato _ [] = []
mueveGato t (g:gs) = movimientosDelGato ++ mueveGato t gs
        where
            validas = casillasValidasGatos t g
            nombre = t ! g
            movimientosDelGato = map (\v -> (intercambiaPieza t nombre v g, v)) validas

-- Puntuaciones
puntuaGato :: Tablero -> Pos -> Double
puntuaGato t pos
    | finGato t = 10.0
    -- | hayHueco t || casiEncerrado = -10.0
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

distribucionOpciones :: Point
distribucionOpciones = (-450.0, 130.0)

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

posBoton :: (Float, Float)
posBoton = (ancho, (-ancho) + ajusteInicial)

anchoBoton :: Float
anchoBoton = 130.0

altoBoton :: Float
altoBoton = 40.0
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Fin parámetros %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

alturasCasillas :: [Float]
alturasCasillas = [a, a - diferencia .. 0]
  where
    a = ancho - ajusteInicialMenu * 2
    diferencia = a / 5.0

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

cambiaOpcion :: Point -> Mundo -> Int -> String -> IO Mundo
cambiaOpcion raton@(x,y) mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional) nivel opcion
    | nivel == 0 = do
        let nuevoMundo = (mov,juego,traduceDif opcion,traduceProf opcion,marca,turno,seleccionado,esMaquina,adicional)
        return nuevoMundo
    | nivel == 1 = do
        let nuevoMundo = (mov,juego,dif,prof,opcion,turno,seleccionado,esMaquina,adicional)
        return nuevoMundo
    | nivel == 99 = do
        let ratonCorregido = (x,y-alturaTablero)
        cambiaMiniTablero ratonCorregido mundo
    | otherwise = error "El nivel de opciones especificado para la función cambiaOpción del juego del gato no existe."

cambiaMiniTablero :: Point -> Mundo -> IO Mundo
cambiaMiniTablero raton mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional)
    | not (null pulsadas) && marca == "R" = do
        let nuevoTurno = posAturno (cabeza "cambiaMiniTablero" pulsadas)
        let nuevoMundo = (mov,juego,dif,prof,marca,nuevoTurno,seleccionado,esMaquina, adicional)
        return nuevoMundo
    | otherwise = return mundo
        where
            posPosibles = casillasVacias $ fst $ inicial $ turnoApos turno
            casillasPosibles = map (matrizMiniPosiciones !) posPosibles
            relacion = zip casillasPosibles posPosibles
            pulsadas = map snd $ filter (\(c,p) -> pulsaCercaMini raton c) relacion

creaTableroConOpciones :: Mundo -> Mundo
creaTableroConOpciones mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional)
    | marca == "R" = (inicial (turnoApos turno),juego,dif,p,marca,turno,seleccionado,False,adicional)
    | otherwise = (mov,juego,dif,p,"G",turno,seleccionado,True,adicional)
        where
            p | prof == 0 = 1 | otherwise = prof

calculaNuevoEstado :: Point -> Mundo -> IO Mundo
calculaNuevoEstado casilla mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional)
    | (seleccionado == "R") && (posSeñalada `elem` vaciasRaton) = do
        return $ calculaMundo posSeñalada mundo
    | (seleccionado `elem` nombresGatos) && (posSeñalada `elem` vaciasGatos) = do
        return $ calculaMundo posSeñalada mundo
    | (marca == "R") && (el == "R") = do
        return (mov,juego,dif,prof,marca,turno,el,False,adicional)
    | (marca == "G") && (el `elem` nombresGatos) = do
        return (mov,juego,dif,prof,marca,turno,el,False,adicional)
    | otherwise = return mundo
        where
            posRaton = buscaPieza estado "R"
            vaciasRaton = casillasVaciasRaton estado posRaton
            vaciasGatos = casillasVaciasGatos estado
            t = round tamMatriz
            ps = [(i,j) | i<-[1 .. t], j<-[1 .. t]]
            matrz = toList matrizPosiciones
            relacionadas = zip matrz ps
            posSeñalada = snd $ cabeza "calculaNuevoEstado" $ filter (\(c,p) -> c==casilla) relacionadas
            el = estado ! posSeñalada

pintaMarca :: Pos -> Tablero -> Picture
pintaMarca pos estado
    | marca == "R" = pintaRaton lugar
    | marca `elem` nombresGatos = pintaGato lugar
    | otherwise = blank
        where
            marca = estado ! pos
            lugar = matrizPosiciones ! pos

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

pintaMiniMarca :: Pos -> Tablero -> Matrix Point -> Picture
pintaMiniMarca pos estado posiciones
    | marca == "R" = pintaMiniRaton lugar
    | marca `elem` nombresGatos = pintaMiniGato lugar
    | marca == "X" = translate i j casNegra
    | otherwise = blank
        where
            marca = estado ! pos
            lugar@(i,j) = posiciones ! pos
            casNegra = color marron $ rectangleSolid diferenciaParaMiniCasillas diferenciaParaMiniCasillas

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones auxiliares
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

marcaMaquinaGato :: String -> String
marcaMaquinaGato marca = if marca == "R" then "G" else "R"

cerosEnCadena :: Int -> String
cerosEnCadena 0 = ""
cerosEnCadena d = "0" ++ cerosEnCadena (d-1)

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

calculaMundo :: Pos -> Mundo -> Mundo
calculaMundo casilla ((estado,pos),juego,dif,prof,marca,turno,seleccionado,esMaquina,adicional) = nuevoMundo
    where
        posAntigua = buscaPieza estado seleccionado
        nuevoEstado = intercambiaPieza estado seleccionado casilla posAntigua
        nuevoMundo = ((nuevoEstado,casilla),juego,dif,prof,marca,turno,"",True,adicional)

pintaRaton :: Point -> Picture
pintaRaton (x,y) = translate x y raton
    where
        raton = color white formaPeon

pintaGato :: Point -> Picture
pintaGato (x,y) = translate x y gato
    where
        gato = color black formaPeon

formaPeon :: Picture
formaPeon = pictures [circulo,triangulo]
    where
        tam = diferenciaParaCasillas/2
        circulo = translate 0.0 tam $ circleSolid (tam/2)
        triangulo = polygon [(0.0,tam),(-tam,0.0),(tam,0.0)]

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
uneCasillas (a:as) (b:bs) = [a,b] ++ uneCasillas as bs

pintaMiniRaton :: Point -> Picture
pintaMiniRaton (x,y) = translate x y raton
    where
        raton = color white formaMiniPeon

pintaMiniGato :: Point -> Picture
pintaMiniGato (x,y) = translate x y gato
    where
        gato = color black formaMiniPeon

formaMiniPeon :: Picture
formaMiniPeon = pictures [circulo,triangulo]
    where
        tam = diferenciaParaMiniCasillas/2
        circulo = translate 0.0 tam $ circleSolid (tam/2)
        triangulo = polygon [(0.0,tam),(-tam,0.0),(tam,0.0)]
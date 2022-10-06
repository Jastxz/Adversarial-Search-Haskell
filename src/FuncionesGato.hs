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
        cambiaOpcion,
        creaTableroConOpciones,
        calculaNuevoEstado,
        posBoton,
        pintaComienzoTablero,
        pintaMarca
    ) where

import Data.Matrix
import Data.List (nub)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Tipos
import Utiles

posicionesInicialesGatos = [(8,1),(8,3),(8,5),(8,7)]

falsoInicial :: Tablero
falsoInicial = matrix 8 8 $ \(i,j) -> añadePiezas (i,j)

inicial :: Pos -> Movimiento
inicial pos = (t,pos)
    where
        t = setElem "R" pos falsoInicial

añadePiezas :: Pos -> String
añadePiezas actual
    | actual `elem` posicionesInicialesGatos = "G" ++ show (snd actual)
    | odd suma = " "
    | otherwise = "X"
        where
            suma = uncurry (+) actual

casillasVaciasRaton :: Tablero -> [Pos]
casillasVaciasRaton m = filter (\ c -> (m ! c) == " ") casillasAlrededor
    where
        (min,max) = rangos m
        casillas = [(i,j) | i<-[min..max], j<-[min..max]]
        (f,c) = head $ filter (\pos -> (m ! pos) == "R") casillas
        casillasAlrededor = casillasAlrededorFicha m (f,c)

casillasVaciasGatos :: Tablero -> [Pos]
casillasVaciasGatos m = filter (\ c -> (m ! c) == " ") casillasAlrededor
    where
        (min,max) = rangos m
        casillas = [(i,j) | i<-[min..max], j<-[min..max]]
        casillasGatos = filter (\pos -> (m ! pos) `elem` ["G1","G3","G5","G7"]) casillas
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
        filaRaton = fst $ head $ filter (\p -> (t ! p) == "R") posiciones
        filaGato = maximum $ map fst $ filter (\p -> (t ! p) `elem` ["G1","G3","G5","G7"]) posiciones

movsGato :: Tablero -> String -> Movimientos
movsGato t marca
    | marca == "R" = nub $ map (mueveRaton t) (casillasVaciasRaton t)
    | otherwise = nub $ mueveGato t (casillasVaciasGatos t)

mueveRaton :: Tablero -> Pos -> Movimiento
mueveRaton t pos
    | length estaAlrededor == 1 = (intercambiaPieza t "R" pos (head estaAlrededor), pos)
    | otherwise = (t,pos)
        where
            validas = casillasAlrededorFicha t pos
            estaAlrededor = filter (\p -> (t ! p) == "R") validas

mueveGato :: Tablero -> [Pos] -> Movimientos
mueveGato _ [] = []
mueveGato t (p:ps)
    | length gatosAlrededor == 1 = (intercambiaPieza t (t ! head gatosAlrededor) p (head gatosAlrededor), p) : mueveGato t ps
    | length gatosAlrededor > 1 = intercambia2piezas t gatosAlrededor p ++ mueveGato t ps
    | otherwise = (t,p) : mueveGato t ps
        where
            validas = casillasAlrededorFicha t p
            gatosAlrededor = filter (\v -> (t ! v) `elem` ["G1","G3","G5","G7"]) validas

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

origenMinitableros :: Float
origenMinitableros = ancho/2.0

diferenciaParaMiniCasillas :: Float
diferenciaParaMiniCasillas = origenMinitableros/tamMatriz

miniAncho :: Float
miniAncho = origenMinitableros/2

miniAjusteInicial :: Float
miniAjusteInicial = ancho/tamMatriz

matrizPosiciones :: Matrix Point
matrizPosiciones = matrix t t $ \p -> fst (head (filter (\(cas,pos) -> pos==p) relacion))
    where
        t = round tamMatriz
        ps = [(i,j) |i<-[1..t],j<-[1..t]]
        relacion = zip casillasBlancas ps

distribucionOpciones :: Point
distribucionOpciones = (-15.0,5.0)

alturasCasillas :: [Float]
alturasCasillas = [ancho,ancho-diferencia..0.0]
    where
        diferencia = ancho/5

casillasNegras :: [Point]
casillasNegras = cuadradosImpares ++ cuadradosPares
    where
        d = diferenciaParaCasillas
        a = (-ancho) + ajusteInicial
        impares = [a,a+(2*d)..a+(6*d)]
        pares = [a+d,a+(3*d)..a+(7*d)]
        cuadradosImpares = [(i, j) | j <- impares, i <- impares]
        cuadradosPares = [(i, j) | j <- pares, i <- pares]

casillasBlancas :: [Point]
casillasBlancas = cuadradosImpares ++ cuadradosPares
    where
        d = diferenciaParaCasillas
        a = (-ancho) + ajusteInicial
        impares = [a+d,a+(3*d)..a+(7*d)]
        pares = [a,a+(2*d)..a+(6*d)]
        cuadradosImpares = [(i, j) | j <- pares, i <- impares]
        cuadradosPares = [(i, j) | j <- impares, i <- pares]

alturaTablero :: Float
alturaTablero = -150.0

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

-- -----------------------------------------------------------------------------------------------------------------------
cambiaOpcion :: Point -> Mundo -> Int -> String -> Mundo
cambiaOpcion raton mundo@(mov@(estado,pos),juego,dif,prof,marca,turno,seleccionado,esMaquina) nivel opcion
    | nivel == 0 = (mov,juego,traduceDif opcion,prof,marca,turno,seleccionado,esMaquina)
    | nivel == 1 = (mov,juego,dif,prof,opcion,t,seleccionado,esMaquina)
    | nivel == 99 = cambiaMiniTablero raton mundo
    | otherwise = error "El nivel de opciones especificado para la función cambiaOpción del juego del gato no existe."
        where
            e = "El turno y marca que llegan a la función cambiaOpción del juego del gato no existe."
            t | opcion == "R" = 1 | opcion == "G" = 2 | otherwise = error e

-- Aux
traduceDif :: String -> Int
traduceDif dif
    | dif == "Aleatoria" = 0
    | dif == "Mínima" = 1
    | dif == "Fácil" = 2
    | dif == "Normal" = 3
    | otherwise  = 4

cambiaMiniTablero :: Point -> Mundo -> Mundo
cambiaMiniTablero raton mundo@(mov@(estado,pos),juego,dif,prof,marca,turno,seleccionado,esMaquina)
    | not (null pulsadas) = calculaMundo (head pulsadas) mundo
    | otherwise = mundo
        where
            t = round tamMatriz
            posPosibles = [(f,c) | f<-[1..t], c<-[1..t]]
            relacion = zip (toList matrizPosiciones) posPosibles
            pulsadas = map snd $ filter (\(c,p) -> c==raton) relacion
-- -----------------------------------------------------------------------------------------------------------------------

creaTableroConOpciones :: Mundo -> Mundo
creaTableroConOpciones mundo@(mov@(estado,pos),juego,dif,prof,marca,turno,seleccionado,esMaquina)
    | marca == "R" = (inicial (6,3),juego,dif,prof,marca,turno,seleccionado,esMaquina)
    | marca == "G" = mundo
    | otherwise = error "La marca que llega a la función creaTableroConOpciones del juego del gato no existe."

-- -----------------------------------------------------------------------------------------------------------------------
calculaNuevoEstado :: Point -> Mundo -> Mundo
calculaNuevoEstado casilla mundo@(mov@(estado,pos),juego,dif,prof,marca,turno,seleccionado,esMaquina)
    | (seleccionado `elem` ["R","G1","G3","G5","G7"]) && (casilla `elem` casillasVacias) = calculaMundo posNueva mundo
    | (marca == "R") && (el == "R") = ((estado,pos),juego,dif,prof,marca,turno,el,False)
    | (marca == "G") && (el `elem` ["G1","G3","G5","G7"]) = ((estado,pos),juego,dif,prof,marca,turno,el,False)
    | otherwise = mundo
        where
            t = round tamMatriz
            posPosibles = [(f,c) | f<-[1..t], c<-[1..t]]
            relacion = zip (toList matrizPosiciones) posPosibles
            posNueva = snd $ head $ filter (\(c,p) -> c==casilla) relacion
            el = estado ! posNueva
            casillasVacias = map (matrizPosiciones !) (casillasVaciasRaton estado)

-- Aux
calculaMundo :: Pos -> Mundo -> Mundo
calculaMundo casilla ((estado,pos),juego,dif,prof,marca,turno,seleccionado,esMaquina) = ((nuevoEstado,casilla),juego,dif,prof,marca,turno,"",True)
    where
        posAntigua = buscaPieza estado seleccionado
        nuevoEstado = intercambiaPieza estado seleccionado casilla posAntigua
-- -----------------------------------------------------------------------------------------------------------------------

posBoton :: (Float, Float)
posBoton = (ancho - ajusteInicial, (-ancho) + ajusteInicial)

-- -----------------------------------------------------------------------------------------------------------------------
pintaComienzoTablero :: Movimiento -> Picture
pintaComienzoTablero (estado,pos) = translate 0 origenMinitableros $ pictures [borde,cuadradosDibujados,estadoDibujado]
    where
        borde = rectangleWire origenMinitableros origenMinitableros
        casNegra = color black $ rectangleSolid diferenciaParaMiniCasillas diferenciaParaMiniCasillas
        cuadradosDibujados = pictures [translate i j casNegra | (i, j) <- miniCasillasNegras]
        tam = round tamMatriz
        cAjdrz = [1 .. tam]
        posiciones = [(i, j) | i <- cAjdrz, j <- cAjdrz]
        marcasDibujadas = map (\pos -> pintaMarca pos estado matrizMiniPosiciones) posiciones
        estadoDibujado = pictures marcasDibujadas

-- Aux
matrizMiniPosiciones :: Matrix Point
matrizMiniPosiciones = matrix t t $ \p -> fst (head (filter (\(cas,pos) -> pos==p) relacion))
    where
        t = round tamMatriz
        ps = [(i,j) |i<-[1..t],j<-[1..t]]
        relacion = zip miniCasillasBlancas ps

-- Aux
miniCasillasNegras :: [Point]
miniCasillasNegras = cuadradosImpares ++ cuadradosPares
    where
        d = diferenciaParaMiniCasillas
        a = (-miniAncho) + miniAjusteInicial
        impares = [a,a+(2*d)..a+(6*d)]
        pares = [a+d,a+(3*d)..a+(7*d)]
        cuadradosImpares = [(i, j) | j <- impares, i <- impares]
        cuadradosPares = [(i, j) | j <- pares, i <- pares]

-- Aux
miniCasillasBlancas :: [Point]
miniCasillasBlancas = cuadradosImpares ++ cuadradosPares
    where
        d = diferenciaParaMiniCasillas
        a = (-miniAncho) + miniAjusteInicial
        impares = [a+d,a+(3*d)..a+(7*d)]
        pares = [a,a+(2*d)..a+(6*d)]
        cuadradosImpares = [(i, j) | j <- pares, i <- impares]
        cuadradosPares = [(i, j) | j <- impares, i <- pares]
-- -----------------------------------------------------------------------------------------------------------------------

-- -----------------------------------------------------------------------------------------------------------------------
pintaMarca :: Pos -> Tablero -> Matrix Point -> Picture
pintaMarca pos estado posiciones
    | marca == "R" = pintaRaton lugar
    | marca == "G" = pintaGato lugar
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
        circulo = translate 0.0 10.0 $ circleSolid 30.0
        triangulo = polygon [(0.0,10.0),(-10.0,0.0),(10.0,0.0)]
-- -----------------------------------------------------------------------------------------------------------------------

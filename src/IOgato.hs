module IOgato (
        interactuaGato
    ) where

import Data.Matrix
import Data.List
import Tipos
import Utiles
import FuncionesGato
import MiniMax

interactuaGato :: IO()
interactuaGato = do
    partidaNueva
    putStrLn "Fin del juego"

partidaNueva :: IO ()
partidaNueva = do
    dif <- escogeDificultad
    (turno, marca) <- escogeTurno
    ini <- falsoInicio marca
    putStrLn "¿Quiere establecer un nivel de profundidad distinto al por defecto?"
    putStrLn "Escriba 's' para sí."
    res <- getLine
    if res == "s"
        then do
            prof <- leeDigito "Introduzca la nueva profundidad por favor."
            juegoMedio ini turno dif prof marca
        else do
            let prof = inicializaProfundidadSegunDificultad dif marca
            juegoMedio ini turno dif prof marca

juegoMedio :: Movimiento -> Int -> Int -> Int -> String -> IO()
juegoMedio m@(t,p) turno dif prof marca = do
    putStrLn "Estado del juego:\n"
    print t
    let movimientosPosibles = movsGato t marca
    let casillasValidas = map snd movimientosPosibles
    if turno == 1
        then do
            putStrLn "-Le toca al jugador"
            putStrLn "-Escoja entre las siguientes casillas: "
            (posNueva, posAntigua) <- escogeCasilla t casillasValidas marca
            let tn = intercambiaPieza t marca posNueva posAntigua
            gestionaTurno (tn, posNueva) turno dif prof marca
        else do
            putStrLn "-Le toca a la máquina"
            let marcaMaquina = marcaDeLaMaquina marca "gato"
            mn <- trataDificultad m dif prof marcaMaquina
            gestionaTurno mn turno dif prof marca

gestionaTurno :: Movimiento -> Int -> Int -> Int -> String -> IO()
gestionaTurno m@(t,pos) turno dif prof marca = do
    if esEstadoFinal t "gato"
        then do
            print t
            if (ratonEncerrado t && marca == "G") || (ratonEscapado t && marca == "R")
                then putStrLn "¡Has ganado!"
                else putStrLn "La máquina gana..."
        else do
            let jn = siguiente turno
            juegoMedio m jn dif prof marca

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones de utilidad para comenzar partida nueva
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

escogeDificultad :: IO Int
escogeDificultad = do
    putStrLn "Escoja la dificultad del juego por favor."
    putStrLn "Para ello escriba un número del 0 al 3 (si fuera mayor que 3 se escogerá la dificultad máxima)."
    putStrLn "(0-Aleatorio, 1-Básico, 2-Normal, 3-Tramposo)"
    leeDigito "Escriba el nivel de dificultad: "

escogeTurno :: IO (Int, String)
escogeTurno = do
    putStrLn "Escoja si quiere jugar como ratón o como los gatos."
    putStrLn "Para ello escriba 1 para ratón o 2 para gatos."
    putStrLn "Si el número dado fuera distinto de estos dos, se escogerá aleatoriamente."
    digito <- leeDigito "Escriba un número: "
    if digito == 1 || digito == 2
        then
            if digito == 1
                then return (digito, "R")
                else return (digito, "G")
        else do
            momento <- now
            let turno = mod momento 2
            if turno == 1
                then return (turno, "R")
                else return (turno, "G")

falsoInicio :: String -> IO Movimiento
falsoInicio marca
    | marca == "R" = do
        putStrLn "Si quiere jugar como ratón debe escoger primero la casilla en la que empezará."
        putStrLn "Escoja entre las siguientes casillas: "
        let t = falsoInicial
        let casillasValidas = [c | c<-casillasVacias t, c `notElem` posicionesInicialesGatos]
        (posNueva, posAntigua) <- escogeCasilla t casillasValidas marca
        let ini = inicial posNueva
        return ini
    | otherwise = do
        let falsoInicial = inicial (6,5)
        usaNegamax falsoInicial 2 10 "R" "gato"

escogeCasilla :: Tablero -> [Pos] -> String -> IO (Pos,Pos)
escogeCasilla t casillasValidas marca = do
    pintaCasillas casillasValidas
    putStrLn "Escoge la casilla hacia la que vas a hacer el movimiento"
    posNueva <- revisaIn casillasValidas
    let casillasAlrededor = casillasAlrededorFicha t posNueva
    let posPieza = filter (\p -> (t ! p) == marca) casillasAlrededor
    if marca == "R"
        then do
            let posRaton = head posPieza
            return (posNueva, posRaton)
        else do
            if length posPieza == 1
                then do
                    let posAntigua = head posPieza
                    return (posNueva, posAntigua)
                else do
                    let posEnumeradas = zip posPieza [1..]
                    putStrLn "Ahora escoge el gato que se va a mover de entre los que están cerca de la posición escogida"
                    gato <- escogeGato posEnumeradas
                    let posGato = fst $ head $ filter (\p -> snd p == gato) posEnumeradas
                    return (posNueva, posGato)

escogeGato :: [(Pos, Int)] -> IO Int
escogeGato posEnumeradas = do
    putStrLn "Escriba el número del gato que desea mover."
    putStrLn "Recuerde que están enumerados de izquierda a derecha con sus respectivas posiciones."
    print posEnumeradas
    gato <- leeDigito "Su elección: "
    let esValido = gato `elem` map snd posEnumeradas
    if esValido
        then return gato
        else do
            putStrLn "Ese número de gato no existe."
            escogeGato posEnumeradas

pintaCasillas :: [Pos] -> IO()
pintaCasillas [] = do nuevaLinea
pintaCasillas (c:cs) = do
    putStr $ show c ++ " - "
    pintaCasillas cs

inicializaProfundidadSegunDificultad :: Int -> String -> Int
inicializaProfundidadSegunDificultad dif marca
    | dif == 0 = 0
    | dif == 1 = if marca == "R" then 5 else 7
    | otherwise = if marca == "R" then 30 else 11

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones de utilidad para juegoMedio
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

trataDificultad :: Movimiento -> Int -> Int -> String -> IO Movimiento
trataDificultad m@(t,pos) dif prof marca
    | dif == 0 = ponAleatorio t marca pos
    | otherwise = usaNegamax m dif prof marca "gato"

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones de uso de algoritmo
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

ponAleatorio :: Tablero -> String -> Pos -> IO Movimiento
ponAleatorio t marca p = do
    al <- now
    let movimientosPosibles = movsGato t marca
    let limiteValidas = length movimientosPosibles
    let a = mod al limiteValidas
    let mov = movimientosPosibles !! a
    return mov
        where
            listaVacios = casillasVacias t
            limiteValidos = length listaVacios

usaNegamax :: Movimiento -> Int -> Int -> String -> String -> IO Movimiento
usaNegamax m dif prof marca juego = do
    mejorTablero <- negamax m dif prof marca juego
    let mejorMovimiento = (fst mejorTablero, snd m)
    {- putStrLn "Mejor puntuación para la máquina en el tablero dado"
    print mejorMovimiento -}
    return mejorMovimiento

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones de utilidad para todo el documento
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

representaMovimientos :: Movimientos -> IO()
representaMovimientos [] = do putStrLn "Fin de la representación de tableros."
representaMovimientos (t:ts) = do
    print $ fst t
    nuevaLinea
    representaMovimientos ts

revisaIn :: [Pos] -> IO Pos
revisaIn casillasValidas = do
    f <- leeDigito "-Primero indica la fila: "
    c <- leeDigito "-Ahora indica la columna: "
    if (f, c) `elem` casillasValidas
        then return (f, c)
        else do
            putStrLn "¡La posición señalada no es válida! Inténtelo de nuevo."
            revisaIn casillasValidas
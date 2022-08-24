module IO3enRaya (
        interactua3enRaya
    ) where

import Data.Matrix
import Data.List
import Tipos
import Utiles
import Funciones3enRaya
import MiniMax

interactua3enRaya :: IO()
interactua3enRaya = do
    partidaNueva
    putStrLn "Fin del juego"

partidaNueva :: IO ()
partidaNueva = do
    dif <- escogeDificultad
    turno <- escogeTurno
    marca <- escogeMarca
    putStrLn "¿Quiere establecer un nivel de profundidad distinto al por defecto?"
    putStrLn "Escriba 's' para sí."
    res <- getLine
    if res == "s"
        then do
            prof <- leeDigito "Introduzca la nueva profundidad por favor."
            juegoMedio inicial turno dif prof marca
        else do
            let prof = inicializaProfundidadSegunDificultad dif
            juegoMedio inicial turno dif prof marca

juegoMedio :: Movimiento -> Int -> Int -> Int -> String -> IO()
juegoMedio m@(t,_) turno dif prof marca = do
    putStrLn "Estado del juego:\n"
    representaTablero t
    let (menor, mayor) = rangos t
    if turno == 1
        then do
            putStrLn "-Le toca al jugador"
            putStr "-Para escoger casilla recuerda que los números que puedes escoger oscilan entre "
            putStrLn $ show menor ++ " y " ++ show mayor ++ "."
            nuevaLinea
            pos <- revisaIn (menor,mayor)
            tn <- jugada t pos marca
            gestionaTurno (tn, pos) turno dif prof marca
        else do
            putStrLn "-Le toca a la máquina"
            let marcaMaquina = marcaDeLaMaquina marca "3enRaya"
            let profDinamica = length $ casillasVacias t
            mn <- trataDificultad m dif profDinamica marcaMaquina
            gestionaTurno mn turno dif profDinamica marca

gestionaTurno :: Movimiento -> Int -> Int -> Int -> String -> IO()
gestionaTurno m@(t,pos) j dif prof marca = do
    if finalizado t
        then do
            representaTablero t
            if hay3EnRaya t
                then
                    if j == 1
                        then putStrLn "¡Has ganado!"
                        else putStrLn "La máquina gana..."
                else putStrLn "Empate..."
        else do
            let jn = siguiente j
            juegoMedio m jn dif prof marca

jugada :: Tablero -> Pos -> String -> IO Tablero
jugada t pos v
    | valido pos t = do
        let tn = setElem v pos t
        return tn
    | otherwise = do
        putStrLn "Jugada no válida, vuelva a intentarlo."
        putStrLn "-Recuerde que no puede pintar casillas ya ocupadas."
        let (menor, mayor) = rangos t
        putStr "-Para escoger casilla recuerda que los números que puedes escoger oscilan entre "
        putStrLn $ show menor ++ " y " ++ show mayor ++ "."
        (fil,col) <- revisaIn (menor,mayor)
        jugada t (fil,col) v

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones de utilidad para comenzar partida nueva
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

escogeDificultad :: IO Int
escogeDificultad = do
    putStrLn "Escoja la dificultad del juego por favor."
    putStrLn "Para ello escriba un número del 0 al 3 (si fuera mayor que 3 se escogerá la dificultad máxima)."
    putStrLn "(0-Aleatorio, 1-Básico, 2-Normal, 3-Tramposo)"
    leeDigito "Escriba el nivel de dificultad: "

escogeTurno :: IO Int
escogeTurno = do
    putStrLn "Escoja si quiere ir primero o segundo por favor."
    putStrLn "Para ello escriba 1 o 2 (si fuera distinto de estos se escogerá aleatoriamente)."
    digito <- leeDigito "Escriba su turno: "
    if digito == 1 || digito == 2
        then return digito
        else do
            momento <- now
            let turno = mod momento 2
            return turno

escogeMarca :: IO String
escogeMarca = do
    putStrLn "Escoja si quiere escribir cruces o círculos por favor."
    putStrLn "Para ello escriba X o O (si fuera distinto de estos se escogerá aleatoriamente)."
    putStr "Escriba su marca: "
    cadena <- getLine
    if cadena == "X" || cadena == "O"
        then return cadena
        else do
            momento <- now
            let aleatorio = mod momento 2
            let marcas = ["X","O"]
            let marca = marcas !! aleatorio
            return marca

inicializaProfundidadSegunDificultad :: Int -> Int
inicializaProfundidadSegunDificultad dif
    | dif == 0 = 0
    | otherwise = 9

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones de utilidad para juegoMedio
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

trataDificultad :: Movimiento -> Int -> Int -> String -> IO Movimiento
trataDificultad m@(t,pos) dif prof marca
    | dif == 0 = ponAleatorio t marca
    | otherwise = usaNegamax m dif prof marca "3enRaya"

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones de uso de algoritmo
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

ponAleatorio :: Tablero -> String -> IO Movimiento
ponAleatorio t marca = do
    al <- now
    let a = mod al limiteValidos
    let pos = listaVacios !! a
    let nuevoT = setElem marca pos t
    return (nuevoT,pos)
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
    representaTablero $ fst t
    representaMovimientos ts

representaTablero :: Tablero -> IO()
representaTablero t = do
    let fs = map concat $ toLists t
    let tablero = escribeTablero fs
    putStrLn $ "\n" ++ tablero ++ "\n"

revisaIn :: Pos -> IO Pos
revisaIn (i,j) = do
    f <- leeDigito "-Primero indica la fila: "
    c <- leeDigito "-Ahora indica la columna: "
    if (f>=i && f<=j) && (c>=i && c<=j)
        then return (f, c)
        else do
            putStrLn "¡Fila o columna fuera del tablero. Vuelva a escoger!"
            revisaIn (i,j)
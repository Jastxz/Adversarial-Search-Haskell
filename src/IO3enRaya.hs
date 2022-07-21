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
    putStrLn "Escoja la dificultad del juego por favor."
    putStrLn "Para ello escriba un número del 0 al 3 (si fuera mayor que 3 no se tendrá en cuenta)."
    putStrLn "(0-Aleatorio, 1-Básico, 2-Normal, 3-Tramposo)"
    dif<-leeDigito "Escriba el nivel de dificultad: "
    putStrLn "¿Quiere establecer un nivel de profundidad distinto al por defecto?"
    putStrLn "Escriba 's' para sí."
    res <- getLine
    if res == "s"
        then do
            prof <- leeDigito "Introduzca la nueva profundidad por favor."
            juegoMedio inicial 1 dif prof
        else do
            inicializaProfundidadSegunDificultad dif

juegoMedio :: Tablero -> Int -> Int -> Int -> IO()
juegoMedio t j dif prof = do
    putStrLn "Estado del juego:\n"
    representaTablero t
    let rangosT = rangos t
    let menor = fst rangosT
    let mayor = snd rangosT
    if j == 1
        then do
            putStrLn "-Le toca al jugador"
            putStr "-Para escoger casilla recuerda que los números que puedes escoger oscilan entre "
            putStrLn $ show menor ++ " y " ++ show mayor ++ "."
            nuevaLinea
            (fil,col) <- revisaIn (menor,mayor)
            tn <- jugada t (fil,col) "X"
            gestionaTurno tn j dif prof
        else do
            putStrLn "-Le toca a la máquina"
            tn <- trataDificultad t dif prof
            gestionaTurno tn j dif prof

gestionaTurno :: Tablero -> Int -> Int -> Int -> IO()
gestionaTurno t j dif prof = do
    if finalizado t
        then if hay3EnRaya t
            then do
                representaTablero t
                if j == 1
                    then putStrLn "¡Has ganado!"
                    else putStrLn "La máquina gana..."
            else putStrLn "Empate..."
        else do
            let jn = siguiente j
            representaTablero t
            juegoMedio t jn dif prof

jugada :: Tablero -> (Int,Int) -> String -> IO Tablero
jugada t (i,j) v
    | valido (i,j) t = do
        let tn = setElem v (i,j) t
        return tn
    | otherwise = do
        putStrLn "Jugada no válida, vuelva a intentarlo."
        putStrLn "-Recuerde que no puede pintar casillas ya ocupadas."
        let rangosT = rangos t
        let menor = fst rangosT
        let mayor = snd rangosT
        putStr "-Para escoger casilla recuerda que los números que puedes escoger oscilan entre "
        putStrLn $ show menor ++ " y " ++ show mayor ++ "."
        (fil,col) <- revisaIn (menor,mayor)
        jugada t (fil,col) v

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones de utilidad para el resto del documento
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

inicializaProfundidadSegunDificultad :: Int -> IO()
inicializaProfundidadSegunDificultad dif
    | dif == 0 = juegoMedio inicial 1 dif 0
    | dif == 1 = juegoMedio inicial 1 dif 1
    | dif == 2 = juegoMedio inicial 1 dif 5
    | otherwise = juegoMedio inicial 1 dif 7

trataDificultad :: Tablero -> Int -> Int -> IO Tablero
trataDificultad t dif prof
    | dif == 0 = ponAleatorio t
    | otherwise = usaNegamax t dif prof "3enRaya"

ponAleatorio :: Tablero -> IO Tablero
ponAleatorio t = do
    al <- now
    let a = mod al limiteValidos
    let pos = listaVacios !! a
    let nuevoT = setElem "O" pos t
    return nuevoT
        where
            listaVacios = casillasVacias t
            limiteValidos = length listaVacios

usaNegamax :: Tablero -> Int -> Int -> String -> IO Tablero
usaNegamax t dif prof juego = do
    puntuaciones <- sacaPuntuacionesDeIO puntuacionesIO
    let tablerosPuntuados = [tp | tp<-zip movimientos puntuaciones]
    let tablerosOrdenados = sortOn snd tablerosPuntuados
    let mejorTablero = last tablerosOrdenados
    return $ fst mejorTablero 
    where
        puntuacionesIO = map (\tn -> negamax tn dif prof juego) movimientos
        movimientos = map (\pos -> setElem "O" pos t) listaVacias
        listaVacias = casillasVacias t

representaMovimientos :: Movimientos -> IO()
representaMovimientos [] = do putStrLn "Fin de la representación de tableros."
representaMovimientos (t:ts) = do
    representaTablero t
    representaMovimientos ts

representaTablero :: Tablero -> IO()
representaTablero t = do
    let fs = map concat $ toLists t
    let tablero = escribeTablero fs
    putStrLn $ "\n" ++ tablero ++ "\n"

revisaIn :: (Int,Int) -> IO (Int,Int)
revisaIn (i,j) = do
    f <- leeDigito "-Primero indica la fila: "
    c <- leeDigito "-Ahora indica la columna: "
    if (f>=i && f<=j) && (c>=i && c<=j)
        then return (f, c)
        else do
            putStrLn "¡Fila o columna fuera del tablero. Vuelva a escoger!"
            revisaIn (i,j)
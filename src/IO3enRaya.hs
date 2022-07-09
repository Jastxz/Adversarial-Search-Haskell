module IO3enRaya (
        interactua3enRaya
    ) where

import Data.Matrix
import Utiles
import Funciones3enRaya

interactua3enRaya :: IO()
interactua3enRaya = do
    partidaNueva
    print "Fin del juego"

partidaNueva :: IO ()
partidaNueva = do
    juegoMedio inicial 1 1
    {- putStrLn "¿Quiere empezar el jugador con X o con O?"
    putStrLn "Escriba 'O' o 'X' por favor."
    eleccion <- getChar
    if (eleccion=='X') || (eleccion=='O')
        then if eleccion == 'X'
            then do
                let j = 1
                juegoMedio inicial j 1
            else do
                let j = 2
                juegoMedio inicial j 1
        else do
            putStrLn "Carácter inválido. Vuelva a intentarlo."
            partidaNueva -}

juegoMedio :: Tablero -> Int -> Int -> IO()
juegoMedio t j dif = do
    putStrLn "Estado del juego:\n"
    representaTablero t
    let rangosT = rangos t
    let menor = fst rangosT
    let mayor = snd rangosT
    let fichaJugador = 'X'
    if j == 1
        then do
            putStrLn "-Le toca al jugador"
            putStr "-Para escoger casilla recuerda que los números que puedes escoger oscilan entre "
            putStrLn $ show menor ++ " y " ++ show mayor ++ "."
            nuevaLinea
            (fil,col) <- revisaIn (menor,mayor)
            tn <- jugada t (fil,col) fichaJugador
            gestionaTurno tn j dif
        else do
            putStrLn "-Le toca a la máquina"
            tn <- ponAleatorio t
            gestionaTurno tn j dif

gestionaTurno :: Tablero -> Int -> Int -> IO()
gestionaTurno t j dif = do
    if finalizado t
        then if hay3EnRaya t
            then do
                representaTablero t
                if j == 1
                    then putStrLn "¡El jugador ha ganado!"
                    else putStrLn "La máquina gana..."
            else putStrLn "Empate..."
        else do
            let jn = siguiente j
            representaTablero t
            juegoMedio t jn dif

jugada :: Tablero -> (Int,Int) -> Char -> IO Tablero
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

ponAleatorio :: Tablero -> IO Tablero
ponAleatorio t = do
    al <- now
    let a = mod al limiteValidos
    let pos = validos !! a
    let nuevoT = setElem 'O' pos t
    return nuevoT
        where
            listaVacios = casillasVacias t
            validos = [x | x<-listaVacios, valido x t]
            limiteValidos = length validos

representaTablero :: Tablero -> IO()
representaTablero t = do
    let fs = toLists t
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
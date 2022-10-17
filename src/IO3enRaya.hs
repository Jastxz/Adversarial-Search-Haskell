module IO3enRaya
  ( -- Funciones IO normales
    interactua3enRaya,
    -- Funciones gráficas
    pintaOpciones3enRaya,
    manejaOpciones3enRaya,
    pintaJuego3enRaya,
    hazMovimiento3enRaya,
    mueveMaquina3enRaya,
  )
where

import Data.List
import Data.Matrix
import Funciones3enRaya
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import MiniMax
import Tipos
import Utiles
import UtilesGraficos

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones IO para ejecutar el programa en consola
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

interactua3enRaya :: IO ()
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

juegoMedio :: Movimiento -> Int -> Int -> Int -> String -> IO ()
juegoMedio m@(t, _) turno dif prof marca = do
  putStrLn "Estado del juego:\n"
  representaTablero t
  let (menor, mayor) = rangos t
  if turno == 1
    then do
      putStrLn "-Le toca al jugador"
      putStr "-Para escoger casilla recuerda que los números que puedes escoger oscilan entre "
      putStrLn $ show menor ++ " y " ++ show mayor ++ "."
      nuevaLinea
      pos <- revisaIn (menor, mayor)
      tn <- jugada t pos marca
      gestionaTurno (tn, pos) turno dif prof marca
    else do
      putStrLn "-Le toca a la máquina"
      let marcaMaquina = marcaDeLaMaquina marca "3enRaya"
      let profDinamica = length $ casillasVacias t
      mn <- trataDificultad m dif profDinamica marcaMaquina
      gestionaTurno mn turno dif profDinamica marca

gestionaTurno :: Movimiento -> Int -> Int -> Int -> String -> IO ()
gestionaTurno m@(t, pos) j dif prof marca = do
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
    (fil, col) <- revisaIn (menor, mayor)
    jugada t (fil, col) v

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
      let marcas = ["X", "O"]
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
trataDificultad m@(t, pos) dif prof marca
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
  return (nuevoT, pos)
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
Funciones IO para ejecutar el programa con gráficos
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

{- Funciones de las opciones -}
pintaOpciones3enRaya :: Mundo -> IO Picture
pintaOpciones3enRaya mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina) = do
  -- Valores de separación entre las casillas de las opciones
  let inicioCasillas = fst distribucionOpciones
  let evolucionCasillas = snd distribucionOpciones
  -- Receptáculo para mostrar las opciones
  let borde = rectangleWire 1000 500
  -- Dibujando los niveles de dificultad
  let tituloDif = translate inicioCasillas (head alturasCasillas) $ texto "Dificultad"
  let nivelesDif = head infoEstatica
  let niveles = translate 0 (alturasCasillas !! 1) $ pictures $ listaTextos nivelesDif 'X' inicioCasillas evolucionCasillas False
  let lNiveles = length nivelesDif
  let cbx1 = pictures $ dibujaCheckbox (lNiveles - 1) dif 'X' inicioCasillas evolucionCasillas
  let checkboxNiveles = translate 0 (alturasCasillas !! 2) cbx1
  -- Dibujando los turnos a escoger
  let tituloTurno = translate inicioCasillas (alturasCasillas !! 3) $ texto "Turno"
  let turnosPosibles = infoEstatica !! 1
  let turnos = translate 0 (alturasCasillas !! 4) $ pictures $ listaTextos turnosPosibles 'X' inicioCasillas evolucionCasillas False
  let lTurnos = length turnosPosibles
  let tur | turno <= 0 = 0
        | otherwise = turno - 1
  let cbx2 = pictures $ dibujaCheckbox (lTurnos - 1) tur 'X' inicioCasillas evolucionCasillas
  let checkboxTurnos = translate 0 (alturasCasillas !! 5) cbx2
  -- Dibujando las marcas posibles
  let tituloMarca = translate inicioCasillas (alturasCasillas !! 6) $ texto "Signo"
  let marcasPosibles = infoEstatica !! 2
  let marcas = translate 0 (alturasCasillas !! 7) $ pictures $ listaTextos marcasPosibles 'X' inicioCasillas evolucionCasillas False
  let numMarca
        | marca == "X" = 0
        | otherwise = 1
  let cbx3 = pictures $ dibujaCheckbox (lTurnos - 1) numMarca 'X' inicioCasillas evolucionCasillas
  let checkboxMarcas = translate 0 (alturasCasillas !! 8) cbx3
  -- Preparamos el botón y la lista para crear la imagen
  let (bX, bY) = posBoton
  let btn = translate bX bY $ boton "Comenzar" anchoBoton altoBoton
  let listaRes = [borde, tituloDif, niveles, checkboxNiveles, tituloTurno, turnos, checkboxTurnos, tituloMarca, marcas, checkboxMarcas, btn]
  -- Resultado
  let res = pictures listaRes
  return res

manejaOpciones3enRaya :: Point -> Mundo -> IO Mundo
manejaOpciones3enRaya (x, y) mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina) = do
  -- Valores de separación entre las casillas de las opciones
  let iC = fst distribucionOpciones
  let eC = snd distribucionOpciones
  -- Buscando la casilla en cuestión
  let indice = minimum [if cercaCasilla y altura then p else 99 | (altura, p) <- zip alturasEstaticas [0 ..]]
  let fila | indice == 99 = head infoEstatica
        | otherwise = infoEstatica !! indice
  let limite = length fila
  let indice2 = minimum [if cercaCasilla x longitud then p else 99 | (longitud, p) <- zip [iC, iC + eC ..] [0 .. (limite - 1)]]
  let columna | indice2 == 99 = head fila
        | otherwise = fila !! indice2
  let comenzar | indice == 99 = pulsaCerca (x, y) posBoton
        | otherwise = False
  -- Cambiamos la información del juego a ejecutar y preparamos el tablero inicial
  let nuevoMundo | indice == 99 || indice2 == 99 = mundo
        | otherwise = cambiaOpcion mundo indice columna
  let mundoAejecutar | comenzar = creaTableroConOpciones nuevoMundo
        | otherwise = nuevoMundo
  return mundoAejecutar

{- Funciones intrínsecas del juego -}
pintaJuego3enRaya :: Mundo -> IO Picture
pintaJuego3enRaya mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina) = do
  let alturaMensajes = ancho + 50
  -- Texto de turno
  let mensajeTurno
        | esMaquina = "Le toca a la máquina"
        | otherwise = "Tu turno"
  let turno = translate (-correccionPosicion2 ancho) alturaMensajes $ texto mensajeTurno
  -- Dibujo del tablero
  let borde = rectangleWire tamTablero tamTablero
  let casilla = rectangleWire diferenciaParaCasillas diferenciaParaCasillas
  let disCasillas = toList matrizPosiciones
  let casillas = pictures [translate x y casilla | (x, y) <- disCasillas]
  -- Dibujo del estado
  let t = round tamMatriz
  let posiciones = [(i, j) | i <- [1 .. t], j <- [1 .. t]]
  let marcasDibujadas = map (\pos -> pintaMarca pos estado matrizPosiciones) posiciones
  let estadoDibujado = pictures marcasDibujadas
  -- Texto indicativo
  let mensajeIndicativo
        | esMaquina = "Espere un momento..."
        | otherwise = "Pulse en una casilla vacía para realizar su turno"
  let indicacion = translate (-correccionPosicion (1.25*tamTablero)) (-alturaMensajes) $ texto mensajeIndicativo
  -- Resultado
  let res = pictures [turno, casillas, estadoDibujado, indicacion]
  return res

hazMovimiento3enRaya :: Point -> Mundo -> IO Mundo
hazMovimiento3enRaya raton mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina) = do
  -- Casillas donde puede haber pulsado el jugador para interaccionar con el juego
  let posCasillas = toList matrizPosiciones
  -- Comprobamos si ha pulsado cerca de alguna casilla para realizar una acción de juego
  let pulsadas = [casilla | casilla <- posCasillas, pulsaCasilla casilla raton]
  let accion = head pulsadas
  let posiblesAcciones = map (matrizPosiciones !) (casillasVacias estado)
  -- Finalmente realizamos la acción en caso de que la hubiera y fuera realizable ó simplemente no devolvemos nada nuevo
  if not (null pulsadas) && (accion `elem` posiblesAcciones)
    then do
      let t = round tamMatriz
      let posPosibles = [(f, c) | f <- [1 .. t], c <- [1 .. t]]
      let relacion = zip (toList matrizPosiciones) posPosibles
      let posNueva = snd $ head $ filter (\(c, p) -> c == accion) relacion
      let nuevoEstado = setElem marca posNueva estado
      return ((nuevoEstado, posNueva), juego, dif, prof, marca, turno, "", True)
    else return mundo

{- Función para el turno de la máquina -}
mueveMaquina3enRaya :: Mundo -> IO Mundo
mueveMaquina3enRaya mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina) = do
  let marcaMaquina = marcaDeLaMaquina marca juego
  mn <- trataDificultad mov dif prof marcaMaquina
  let nuevoMundo = (mn, juego, dif, prof, marca, turno, seleccionado, False)
  return nuevoMundo

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones de utilidad para todo el documento
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

representaMovimientos :: Movimientos -> IO ()
representaMovimientos [] = do putStrLn "Fin de la representación de tableros."
representaMovimientos (t : ts) = do
  representaTablero $ fst t
  representaMovimientos ts

representaTablero :: Tablero -> IO ()
representaTablero t = do
  let fs = map concat $ toLists t
  let tablero = escribeTablero fs
  putStrLn $ "\n" ++ tablero ++ "\n"

revisaIn :: Pos -> IO Pos
revisaIn (i, j) = do
  f <- leeDigito "-Primero indica la fila: "
  c <- leeDigito "-Ahora indica la columna: "
  if (f >= i && f <= j) && (c >= i && c <= j)
    then return (f, c)
    else do
      putStrLn "¡Fila o columna fuera del tablero. Vuelva a escoger!"
      revisaIn (i, j)
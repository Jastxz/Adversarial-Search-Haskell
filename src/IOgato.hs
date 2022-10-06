module IOgato
  ( -- Funciones IO normales
    interactuaGato,
    -- Funciones gráficas
    pintaOpcionesGato,
    manejaOpcionesGato,
    pintaJuegoGato,
    hazMovimientoGato,
    mueveMaquinaGato
  )
where

import Data.List
import Data.Matrix
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import FuncionesGato
import MiniMax
import Tipos
import Utiles
import UtilesGraficos
import Control.Exception (throw)

interactuaGato :: IO ()
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

juegoMedio :: Movimiento -> Int -> Int -> Int -> String -> IO ()
juegoMedio m@(t, p) turno dif prof marca = do
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

gestionaTurno :: Movimiento -> Int -> Int -> Int -> String -> IO ()
gestionaTurno m@(t, pos) turno dif prof marca = do
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
    let casillasValidas = [c | c <- casillasVacias t, c `notElem` posicionesInicialesGatos]
    (posNueva, posAntigua) <- escogeCasilla t casillasValidas marca
    let ini = inicial posNueva
    return ini
  | otherwise = do
    let falsoInicial = inicial (6, 5)
    usaNegamax falsoInicial 2 10 "R" "gato"

escogeCasilla :: Tablero -> [Pos] -> String -> IO (Pos, Pos)
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
          let posEnumeradas = zip posPieza [1 ..]
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

pintaCasillas :: [Pos] -> IO ()
pintaCasillas [] = do nuevaLinea
pintaCasillas (c : cs) = do
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
trataDificultad m@(t, pos) dif prof marca
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
Funciones IO para ejecutar el programa con gráficos
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

{- Funciones de las opciones -}
pintaOpcionesGato :: Mundo -> IO Picture
pintaOpcionesGato mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina) = do
  -- Valores de separación entre las casillas de las opciones
  let inicioCasillas = fst distribucionOpciones
  let evolucionCasillas = snd distribucionOpciones
  -- Receptáculo para mostrar las opciones
  let borde = rectangleWire 1000 500
  -- Dibujando los niveles de dificultad
  let tituloDif = translate 0 (head alturasCasillas) $ texto "Dificultad"
  let nivelesDif = head infoEstatica
  let niveles = translate 0 (alturasCasillas !! 1) $ pictures $ listaTextos nivelesDif 'X' inicioCasillas evolucionCasillas
  let lNiveles = length nivelesDif
  let cbx1 = pictures $ dibujaCheckbox (lNiveles - 1) dif 'X' inicioCasillas evolucionCasillas
  let checkboxNiveles = translate 0 (alturasCasillas !! 2) cbx1
  -- Dibujando los turnos y las marcas
  let tituloMarca = translate 0 (alturasCasillas !! 3) $ texto "Jugar como"
  let marcasPosibles = infoEstatica !! 1
  let marcas = translate 0 (alturasCasillas !! 4) $ pictures $ listaTextos marcasPosibles 'X' inicioCasillas evolucionCasillas
  let lMarcas = length marcasPosibles
  let cbx2 = pictures $ dibujaCheckbox (lMarcas - 1) turno 'X' inicioCasillas evolucionCasillas
  let checkboxMarcas = translate 0 (alturasCasillas !! 5) cbx2
  let tableroMostrado = pintaComienzoTablero mov
  let tableroSituado = translate 0 alturaTablero tableroMostrado
  -- Preparamos el botón y la lista para crear la imagen
  let (bX, bY) = posBoton
  let btn = translate bX bY $ boton "Comenzar"
  let listaRes = [borde, tituloDif, niveles, checkboxNiveles, tituloMarca, marcas, checkboxMarcas, tableroSituado, btn]
  -- Resultado
  let res = pictures listaRes
  return res

manejaOpcionesGato :: Point -> Mundo -> IO Mundo
manejaOpcionesGato raton@(x, y) mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina) = do
  -- Valores de separación entre las casillas de las opciones
  let iC = fst distribucionOpciones
  let eC = snd distribucionOpciones
  -- Buscando la casilla en cuestión
  let indice = minimum [if cercaCasilla y altura then p else 99 | (altura, p) <- zip alturasEstaticas [0 ..]]
  let fila | indice == 99 = head infoEstatica | otherwise = infoEstatica !! indice
  let indice2 = minimum [if cercaCasilla x longitud then p else 99 | (longitud, p) <- zip [iC, iC + eC ..] [0 ..]]
  let columna | indice == 99 = head fila | otherwise =  fila !! indice2
  -- Preparamos las variables para el caso de que empiece la máquina en el primer turno
  let falsoInicial = inicial (6, 5)
  movMaquina <- usaNegamax falsoInicial 2 10 "R" "gato"
  -- Cambiamos la información del juego a ejecutar y preparamos el tablero inicial
  let nuevoMundo@(m, j, d, p, ma, t, s, e) = cambiaOpcion raton mundo indice columna
  let mundoMaquina = (movMaquina, j, d, p, ma, t, s, e)
  let mundoAejecutar | ma == "R" = creaTableroConOpciones nuevoMundo
        | otherwise = creaTableroConOpciones mundoMaquina
  return mundoAejecutar

{- Funciones intrínsecas del juego -}
pintaJuegoGato :: Mundo -> IO Picture
pintaJuegoGato mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina) = do
  -- Texto de turno
  let mensajeTurno
        | esMaquina = "Le toca a la máquina"
        | otherwise = "Tu turno"
  let turno = translate 0 300 $ texto mensajeTurno
  -- Dibujo del tablero
  let borde = rectangleWire tamTablero tamTablero
  let casNegra = color black $ rectangleSolid diferenciaParaCasillas diferenciaParaCasillas
  let cuadradosDibujados = pictures [translate i j casNegra | (i, j) <- casillasNegras]
  -- Dibujo del estado
  let tam = round tamMatriz
  let cAjdrz = [1 .. tam]
  let posiciones = [(i, j) | i <- cAjdrz, j <- cAjdrz]
  let marcasDibujadas = map (\pos -> pintaMarca pos estado matrizPosiciones) posiciones
  let estadoDibujado = pictures marcasDibujadas
  -- Texto indicativo
  let mensajeIndicativo
        | esMaquina = "Espere un momento..."
        | seleccionado == "" = "Pulse en una casilla vacía para mover la ficha"
        | otherwise = "Pulse en una ficha para seleccionarla"
  let indicacion = translate 0 (-300) $ texto mensajeIndicativo
  -- Resultado
  let res = pictures [turno, cuadradosDibujados, estadoDibujado, indicacion]
  return res

hazMovimientoGato :: Point -> Mundo -> IO Mundo
hazMovimientoGato raton mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina) = do
  -- Casillas donde puede haber pulsado el jugador para interaccionar con el juego
  let posCasillas = casillasBlancas
  -- Comprobamos si ha pulsado cerca de alguna casilla para realizar una acción de juego
  let pulsadas = [casilla | casilla <- posCasillas, pulsaCasilla casilla raton]
  let accion = head pulsadas
  -- Finalmente realizamos la acción en caso de que la hubiera y fuera realizable ó simplemente no devolvemos nada nuevo
  if not (null pulsadas)
    then do
      let nuevoMundo = calculaNuevoEstado accion mundo
      return nuevoMundo
    else return mundo

{- Función para el turno de la máquina -}
mueveMaquinaGato :: Mundo -> IO Mundo
mueveMaquinaGato mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina) = do
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
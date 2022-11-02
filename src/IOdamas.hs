module IOdamas
  ( -- Funciones gráficas
    pintaOpcionesDamas,
    manejaOpcionesDamas,
    pintaJuegoDamas,
    hazMovimientoDamas,
    mueveMaquinaDamas
  )
where

import Data.List
import Data.Matrix
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import FuncionesDamas
import MiniMax
import Tipos
import Utiles
import UtilesGraficos

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones de uso de algoritmo
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

trataDificultad :: Movimiento -> Int -> Int -> String -> IO Movimiento
trataDificultad m@(t, pos) dif prof marca
  | dif == 0 = ponAleatorio t marca pos
  | otherwise = usaNegamax m dif prof marca "damas"

ponAleatorio :: Tablero -> String -> Pos -> IO Movimiento
ponAleatorio t marca p = do
  al <- now
  let vivas = piezasVivas t
  let movimientosPosibles = movsDamas t marca vivas
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
pintaOpcionesDamas :: Mundo -> IO Picture
pintaOpcionesDamas mundo@(m@(e,p), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional) = do
  let mov | rangos e == (1,1) = inicial (turnoApos turno) | otherwise = m
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
  -- Dibujando los turnos y las marcas
  let tituloMarca = translate inicioCasillas (alturasCasillas !! 3) $ texto "Jugar como"
  let marcasPosibles = infoEstatica !! 1
  let marcas = translate 0 (alturasCasillas !! 4) $ pictures $ listaTextos marcasPosibles 'X' inicioCasillas evolucionCasillas False
  let lMarcas = length marcasPosibles
  let mrc | marca == "B" = 0 | otherwise = 1
  let cbx2 = pictures $ dibujaCheckbox (lMarcas - 1) mrc 'X' inicioCasillas evolucionCasillas
  let checkboxMarcas = translate 0 (alturasCasillas !! 5) cbx2
  -- Preparamos el botón y la lista para crear la imagen
  let (bX, bY) = posBoton
  let btn = translate bX bY $ boton "Comenzar" anchoBoton altoBoton
  let listaRes = [borde, tituloDif, niveles, checkboxNiveles, tituloMarca, marcas, checkboxMarcas, btn]
  -- Resultado
  let res = pictures listaRes
  return res

manejaOpcionesDamas :: Point -> Mundo -> IO Mundo
manejaOpcionesDamas raton@(x, y) mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional) = do
  -- Valores de separación entre las casillas de las opciones
  let iC = fst distribucionOpciones
  let eC = snd distribucionOpciones
  -- Buscando la casilla en cuestión
  let indice = minimum [if cercaBox y altura then p else 99 | (altura, p) <- zip alturasEstaticas [0 ..]]
  let fila | indice == 99 = head infoEstatica | otherwise = infoEstatica !! indice
  let limite = length fila
  let indice2 = minimum [if cercaBox x longitud then p else 99 | (longitud, p) <- zip [iC, iC + eC ..] [0 .. (limite - 1)]]
  let columna | indice == 99 || indice2 == 99 = head fila | otherwise =  fila !! indice2
  let comenzar = pulsaCerca raton posBoton
  -- Cambiamos la información del juego a ejecutar y preparamos el tablero inicial
  nuevoMundo@(m, j, d, p, ma, t, s, e, ad) <- cambiaOpcion raton mundo indice columna
  let mundoAejecutar | comenzar && ma == "B" = creaTableroConOpciones nuevoMundo
        | otherwise = nuevoMundo
  return mundoAejecutar

{- Funciones intrínsecas del juego -}
pintaJuegoDamas :: Mundo -> IO Picture
pintaJuegoDamas mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional) = do
  -- Texto de turno
  let mensajeTurno
        | esMaquina = "Le toca a la máquina"
        | otherwise = "Tu turno"
  let turno = translate 0 300 $ texto mensajeTurno
  -- Dibujo del tablero
  let borde = rectangleWire tamTablero tamTablero
  let casNegra = color marron $ rectangleSolid diferenciaParaCasillas diferenciaParaCasillas
  let cuadradosDibujados = pictures [translate i j casNegra | (i, j) <- casillasNegras]
  -- Dibujo del estado
  let tam = round tamMatriz
  let cAjdrz = [1 .. tam]
  let posiciones = [(i, j) | i <- cAjdrz, j <- cAjdrz]
  let marcasDibujadas = map (`pintaMarca` estado) posiciones
  let estadoDibujado = pictures marcasDibujadas
  -- Texto indicativo
  let mensajeIndicativo
        | esMaquina = "Espere un momento..."
        | seleccionado /= "" = "Pulse en una casilla vacía válida para mover la ficha"
        | otherwise = "Pulse en una ficha para seleccionarla"
  let indicacion = translate 0 (-300) $ texto mensajeIndicativo
  -- Resultado
  let res = pictures [turno, borde, cuadradosDibujados, estadoDibujado, indicacion]
  return res

hazMovimientoDamas :: Point -> Mundo -> IO Mundo
hazMovimientoDamas raton mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional) = do
  -- Casillas donde puede haber pulsado el jugador para interaccionar con el juego
  let posCasillas = casillasBlancas
  -- Comprobamos si ha pulsado cerca de alguna casilla para realizar una acción de juego
  let pulsadas = [casilla | casilla <- posCasillas, pulsaCasilla casilla raton]
  -- Finalmente realizamos la acción en caso de que la hubiera y fuera realizable ó simplemente no devolvemos nada nuevo
  if not (null pulsadas)
    then do
      let accion = head pulsadas
      calculaNuevoEstado accion mundo
    else return mundo

{- Función para el turno de la máquina -}
mueveMaquinaDamas :: Mundo -> IO Mundo
mueveMaquinaDamas mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional) = do
  let marcaMaquina = marcaDeLaMaquina marca juego
  mn <- trataDificultad mov dif prof marcaMaquina
  let nuevoMundo = (mn, juego, dif, prof, marca, turno, seleccionado, False, adicional)
  return nuevoMundo
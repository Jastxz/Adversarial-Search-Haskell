module IOdamas
  ( -- Funciones gráficas
    pintaOpcionesDamas,
    manejaOpcionesDamas,
    pintaJuegoDamas,
    hazMovimientoDamas,
    mueveMaquinaDamas,
  )
where

import Data.List
import Data.Matrix
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Tipos
import Utiles
import UtilesGraficos
import GuardarCargar
import Interconexion
import MiniMax
import FuncionesDamas

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
  let movimientosPosibles = movsDamas t marca
  let mov = aleatorio al movimientosPosibles
  print "Datos del movimiento aleatorio"
  print al
  print $ length movimientosPosibles
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
pintaOpcionesDamas mundo@(m@(e, p), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional) = do
  let mov
        | rangos e == (1, 1) = inicial (turnoApos turno)
        | otherwise = m
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
  let mrc
        | marca == "B" = 0
        | otherwise = 1
  let cbx2 = pictures $ dibujaCheckbox (lMarcas - 1) mrc 'X' inicioCasillas evolucionCasillas
  let checkboxMarcas = translate 0 (alturasCasillas !! 5) cbx2
  -- Preparamos los botones y la lista para crear la imagen
  let (cX, cY) = posCargar
  let cargar = translate cX cY $ boton "Cargar" anchoBoton altoBoton
  let (bX, bY) = posBoton
  let btn = translate bX bY $ boton "Comenzar" anchoBoton altoBoton
  let listaRes1 = [borde, tituloDif, niveles, checkboxNiveles, tituloMarca]
  let listaRes2 = [marcas, checkboxMarcas, cargar, btn]
  let listaRes = listaRes1 ++ listaRes2
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
  let fila
        | indice == 99 = head infoEstatica
        | otherwise = infoEstatica !! indice
  let limite = length fila
  let indice2 = minimum [if cercaBox x longitud then p else 99 | (longitud, p) <- zip [iC, iC + eC ..] [0 .. (limite - 1)]]
  let columna
        | indice == 99 || indice2 == 99 = head fila
        | otherwise = fila !! indice2
  let cargar = pulsaCerca raton posCargar
  let comenzar = pulsaCerca raton posBoton
  -- Cambiamos la información del juego a ejecutar y preparamos el tablero inicial
  nuevoMundo@(m, j, d, p, ma, t, s, e, ad) <- cambiaOpcion raton mundo indice columna
  let mundoAejecutar
        | cargar = menuCargarPartida
        | comenzar = creaTableroConOpciones nuevoMundo
        | otherwise = nuevoMundo
  return mundoAejecutar

{- Funciones intrínsecas del juego -}
pintaJuegoDamas :: Mundo -> IO Picture
pintaJuegoDamas mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional) = do
  let alturaMensajes = ancho + 50
  -- Texto de turno
  let mensajeTurno
        | esMaquina = "Le toca a la maquina"
        | otherwise = "Tu turno"
  let turno = translate (- correccionPosicion2 ancho) alturaMensajes $ texto mensajeTurno
  -- Dibujo del tablero
  let borde = rectangleWire tamTablero tamTablero
  let casPosible = color green $ rectangleSolid diferenciaParaCasillas diferenciaParaCasillas
  let cab
        | null seleccionado = ' '
        | otherwise = cabeza "pintaJuegoDamas" seleccionado
  let (validasDamas, validasReinas) = casillasDisponiblesParaElJugador mundo
  let posicionesValidas
        | cab == 'R' = validasReinas
        | cab == 'B' || cab == 'N' = validasDamas
        | otherwise = []
  let casillasPosibles = map (matrizPosiciones !) posicionesValidas
  let csPosibles = pictures [translate i j casPosible | (i, j) <- casillasPosibles]
  let casNegra = color marron $ rectangleSolid diferenciaParaCasillas diferenciaParaCasillas
  let csNegras = pictures [translate i j casNegra | (i, j) <- casillasNegras]
  let cuadradosDibujados = pictures [csNegras, csPosibles]
  -- Dibujo del estado
  let tam = round tamMatriz
  let cAjdrz = [1 .. tam]
  let posiciones = [(i, j) | i <- cAjdrz, j <- cAjdrz]
  let marcasDibujadas = map (`pintaMarca` estado) posiciones
  let estadoDibujado = pictures marcasDibujadas
  -- Texto indicativo
  let mensajeIndicativo
        | esMaquina = "Espere un momento..."
        | seleccionado /= "" && null posicionesValidas = "No puede mover esta ficha si hay otra que puede atacar"
        | seleccionado /= "" = "Pulse en una casilla vacia valida para mover la ficha"
        | otherwise = "Pulse en una ficha para seleccionarla"
  let indicacion = translate (- correccionPosicion (1.25 * tamTablero)) (- alturaMensajes) $ texto mensajeIndicativo
  -- Botones para guardar y cargar partidas
  let (cX, cY) = posCargarJuego
  let cargar = translate cX cY $ boton "Cargar" anchoBoton altoBoton
  let (gX, gY) = posGuardarJuego
  let guardar = translate gX gY $ boton "Guardar" anchoBoton altoBoton
  -- Resultado
  let res = pictures [turno, borde, cuadradosDibujados, estadoDibujado, indicacion, cargar, guardar]
  return res

hazMovimientoDamas :: Point -> Mundo -> IO Mundo
hazMovimientoDamas raton mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional) = do
  -- Casillas donde puede haber pulsado el jugador para interaccionar con el juego
  let posCasillas = casillasBlancas
  -- Comprobamos si ha pulsado cerca de alguna casilla para realizar una acción de juego
  let pulsadas = [casilla | casilla <- posCasillas, pulsaCasilla casilla raton]
  let cargar = pulsaCerca raton posCargarJuego
  let guardar = pulsaCerca raton posGuardarJuego
  -- Finalmente realizamos la acción en caso de que la hubiera y fuera realizable ó simplemente no devolvemos nada nuevo
  if not (null pulsadas)
    then do
      let accion = head pulsadas
      calculaNuevoEstado accion mundo
    else
      if cargar || guardar
        then do
          partidaGuardada <- guardarPartida mundo
          let guardaOcarga | cargar = menuCargarPartida
                | guardar = partidaGuardada
                | otherwise = mundo
          return guardaOcarga
        else return mundo

{- Función para el turno de la máquina -}
mueveMaquinaDamas :: Mundo -> IO Mundo
mueveMaquinaDamas mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional) = do
  let marcaMaquina = marcaDeLaMaquina marca juego
  mn@(e, _) <- trataDificultad mov dif prof marcaMaquina
  let vivas = piezasVivas e
  let deLaMaquina | marcaMaquina == "B" = cabeza "mueveMaquinaDamas" vivas ++ (vivas !! 1)
        | otherwise = (vivas !! 2) ++ (vivas !! 3)
  let pos = buscaPieza e $ cabeza "mueveMaquinaDamas" deLaMaquina
  let punt = puntuaDamas e pos
  let ad
        | marcaMaquina == "B" && punt > 0 = vivas ++ [["maquina"]]
        | marcaMaquina == "N" && punt > 0 = vivas ++ [["maquina"]]
        | marcaMaquina == "B" && punt < 0 = ad ++ [["humano"]]
        | marcaMaquina == "N" && punt < 0 = ad ++ [["humano"]]
        | otherwise = vivas ++ [["empate"]]
  let adic | finDamas e = ad
        | otherwise = adicional
  let nuevoMundo = (mn, juego, dif, prof, marca, turno, seleccionado, False, adic)
  return nuevoMundo
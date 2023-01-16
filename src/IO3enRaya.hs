module IO3enRaya
  ( -- Funciones gráficas
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
import GuardarCargar
import Interconexion
import MiniMax
import Tipos
import Utiles
import UtilesGraficos

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones de uso de algoritmo
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

trataDificultad :: Movimiento -> Int -> Int -> String -> IO Movimiento
trataDificultad m@(t, pos) dif prof marca
  | dif == 0 = ponAleatorio t marca
  | otherwise = usaNegamax m dif prof marca "3enRaya"

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
  return mejorMovimiento

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones IO para ejecutar el programa con gráficos
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

{- Funciones de las opciones -}
pintaOpciones3enRaya :: Mundo -> IO Picture
pintaOpciones3enRaya mundo = do
  -- Inicializando algunas variables necesarias
  let [infoDif, infoTurnos, infoMarcas] = infoEstatica
  let inicioCasillas = fst distribucionOpciones
  let evolucionCasillas = snd distribucionOpciones
  -- Receptáculo para mostrar las opciones
  let borde = uncurry rectangleWire tamañoRectangulo
  -- Dibujando los niveles de dificultad
  let alturasDif = [head alturasCasillas, alturasCasillas !! 1, alturasCasillas !! 2]
  bloqueDif <- creaBloque alturasDif etiquetaNivel infoDif (dameDificultad mundo) distribucionOpciones
  -- Dibujando los turnos a escoger
  let alturasTurnos = [alturasCasillas !! 3, alturasCasillas !! 4, alturasCasillas !! 5]
  let turno = dameTurno mundo
  let tur
        | turno <= 0 = 0
        | otherwise = turno - 1
  bloqueTurnos <- creaBloque alturasTurnos etiquetaTurno infoTurnos tur distribucionOpciones
  -- Dibujando las marcas posibles
  let alturasMarcas = [alturasCasillas !! 6, alturasCasillas !! 7, alturasCasillas !! 8]
  let marca = dameMarca mundo
  let numMarca
        | marca == "X" = 0
        | otherwise = 1
  bloqueMarcas <- creaBloque alturasMarcas etiquetaMarca infoMarcas numMarca distribucionOpciones
  -- Preparamos los botones
  menu <- creaBoton posMenu etiquetaMenu
  cargar <- creaBoton posCargar etiquetaCargar
  comenzar <- creaBoton posBoton etiquetaComenzar
  -- Devolvemos la imagen
  return $ pictures [borde, bloqueDif, bloqueTurnos, bloqueMarcas, menu, cargar, comenzar]

manejaOpciones3enRaya :: Point -> Mundo -> IO Mundo
manejaOpciones3enRaya raton mundo = do
  -- Buscando la casilla en cuestión
  let (indice, columna) = uncurry (opcionPulsada raton alturasEstaticas infoEstatica) distribucionOpciones
  let menu = pulsaCerca raton posMenu
  let cargar = pulsaCerca raton posCargar
  let comenzar = pulsaCerca raton posBoton
  -- Cambiamos la información del juego a ejecutar y preparamos el tablero inicial
  let nuevoMundo = cambiaOpcion mundo indice columna
  let mundoAejecutar
        | menu = menuInicial
        | cargar = menuCargarPartida
        | comenzar = creaTableroConOpciones nuevoMundo
        | otherwise = nuevoMundo
  return mundoAejecutar

{- Funciones intrínsecas del juego -}
pintaJuego3enRaya :: Mundo -> IO Picture
pintaJuego3enRaya mundo = do
  let mov = dameMovimiento mundo
  let estado = fst mov
  let esMaquina = dameEsMaquina mundo
  let alturaMensajes = ancho + 50
  -- Texto de turno
  let turno = translate (- correccionPosicion2 ancho) alturaMensajes $ texto $ dameMensajeTurno esMaquina
  -- Dibujo del tablero
  let borde = rectangleWire tamTablero tamTablero
  let casilla = rectangleWire diferenciaParaCasillas diferenciaParaCasillas
  let casillas = pictures [translate x y casilla | (x, y) <- toList matrizPosiciones]
  -- Dibujo del estado
  let t = round tamMatriz
  let marcasDibujadas = map (\pos -> pintaMarca pos estado matrizPosiciones) $ [(i, j) | i <- [1 .. t], j <- [1 .. t]]
  let estadoDibujado = pictures marcasDibujadas
  -- Texto indicativo
  let mensajeIndicativo
        | esMaquina = "Wait a moment please..."
        | otherwise = "Click on an empty box to take your turn"
  let indicacion = translate (- correccionPosicion (1.25 * tamTablero)) (- alturaMensajes) $ texto mensajeIndicativo
  -- Botones para guardar y cargar partidas
  opciones <- creaBoton posOpciones etiquetaOpciones
  cargar <- creaBoton posCargarJuego etiquetaCargar
  guardar <- creaBoton posGuardarJuego etiquetaGuardar
  volver <- creaBoton posVolver etiquetaVolver
  -- Resultado
  return $ pictures [turno, casillas, estadoDibujado, indicacion, opciones, cargar, guardar, volver]

hazMovimiento3enRaya :: Point -> Mundo -> IO Mundo
hazMovimiento3enRaya raton mundo = do
  -- Preparamos el acceso al archivo temporal
  temporal <- caminoTemporal
  -- Casillas donde puede haber pulsado el jugador para interaccionar con el juego
  let posCasillas = toList matrizPosiciones
  -- Comprobamos si ha pulsado cerca de alguna casilla para realizar una acción de juego
  let posBotones = [posOpciones, posCargarJuego, posGuardarJuego, posVolver]
  let pulsacion = devuelvePulsacion raton posCasillas posBotones
  let posiblesAcciones = map (matrizPosiciones !) $ (casillasVacias . fst . dameMovimiento) mundo
  -- Finalmente realizamos la acción en caso de que la hubiera y fuera realizable ó simplemente no devolvemos nada nuevo
  accionRealizada mundo pulsacion temporal posiblesAcciones

{- Función para el turno de la máquina -}
mueveMaquina3enRaya :: Mundo -> IO Mundo
mueveMaquina3enRaya mundo = do
  let juego = dameJuego mundo
  let dif = dameDificultad mundo
  let prof = dameProfundidad mundo
  let marca = dameMarca mundo
  mn@(e, p) <- trataDificultad (dameMovimiento mundo) dif prof (marcaDeLaMaquina marca juego)
  let ad
        | lleno e && (not . hay3EnRaya) e = [["empate"]]
        | hay3EnRaya e = [["maquina"]]
        | otherwise = dameAdicional mundo
  return (mn, juego, dif, prof, marca, dameTurno mundo, dameSeleccionado mundo, False, ad)

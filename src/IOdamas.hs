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
  return mov
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
pintaOpcionesDamas :: Mundo -> IO Picture
pintaOpcionesDamas mundo = do
  -- Inicializando algunas variables necesarias
  let [infoDif, infoMarcas] = infoEstatica
  let inicioCasillas = fst distribucionOpciones
  let evolucionCasillas = snd distribucionOpciones
  let m = dameMovimiento mundo
  let e = fst m
  let turno = dameTurno mundo
  let mov | rangos e == (1, 1) = inicial (turnoApos turno)
        | otherwise = m
  -- Receptáculo para mostrar las opciones
  let borde = uncurry rectangleWire tamañoRectangulo
  -- Dibujando los niveles de dificultad
  let alturasDif = [head alturasCasillas, alturasCasillas !! 1, alturasCasillas !! 2]
  bloqueDif <- creaBloque alturasDif etiquetaNivel infoDif (dameDificultad mundo) distribucionOpciones
  -- Dibujando los turnos, las marcas y el mini tablero
  let alturasMarcas = [alturasCasillas !! 3, alturasCasillas !! 4, alturasCasillas !! 5]
  let marca = dameMarca mundo
  let mrc | marca == "B" = 0
        | otherwise = 1
  bloqueMarcas <- creaBloque alturasMarcas etiquetaBando infoMarcas mrc distribucionOpciones
  -- Preparamos los botones
  menu <- creaBoton posMenu etiquetaMenu
  cargar <- creaBoton posCargar etiquetaCargar
  comenzar <- creaBoton posBoton etiquetaComenzar
  -- Devolvemos la imagen
  return $ pictures [borde, bloqueDif, bloqueMarcas, menu, cargar, comenzar]

manejaOpcionesDamas :: Point -> Mundo -> IO Mundo
manejaOpcionesDamas raton mundo = do
  -- Buscando la casilla en cuestión
  let (indice, columna) = uncurry (opcionPulsada raton alturasEstaticas infoEstatica) distribucionOpciones
  let menu = pulsaCerca raton posMenu
  let cargar = pulsaCerca raton posCargar
  let comenzar = pulsaCerca raton posBoton
  -- Cambiamos la información del juego a ejecutar y preparamos el tablero inicial
  nuevoMundo@(m, j, d, p, ma, t, s, e, ad) <- cambiaOpcion raton mundo indice columna
  let mundoAejecutar
        | menu = menuInicial
        | cargar = menuCargarPartida
        | comenzar = creaTableroConOpciones nuevoMundo
        | otherwise = nuevoMundo
  return mundoAejecutar

{- Funciones intrínsecas del juego -}
pintaJuegoDamas :: Mundo -> IO Picture
pintaJuegoDamas mundo = do
  let estado = (fst . dameMovimiento) mundo
  let seleccionado = dameSeleccionado mundo
  let esMaquina = dameEsMaquina mundo
  let alturaMensajes = ancho + 50
  -- Texto de turno
  let turno = translate (- correccionPosicion2 ancho) alturaMensajes $ texto $ dameMensajeTurno esMaquina
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
        | esMaquina = "Wait a moment..."
        | seleccionado /= "" && null posicionesValidas = "You can not move this tab if exist another that can atack"
        | seleccionado /= "" = "Click on a valid empty box to move the tab"
        | otherwise = "Click a tab to select it"
  let indicacion = translate (- correccionPosicion (1.25 * tamTablero)) (- alturaMensajes) $ texto mensajeIndicativo
  -- Botones para guardar y cargar partidas
  opciones <- creaBoton posOpciones etiquetaOpciones
  cargar <- creaBoton posCargarJuego etiquetaCargar
  guardar <- creaBoton posGuardarJuego etiquetaGuardar
  volver <- creaBoton posVolver etiquetaVolver
  -- Resultado
  return $ pictures [turno, borde, cuadradosDibujados, estadoDibujado, indicacion, opciones, cargar, guardar, volver]

hazMovimientoDamas :: Point -> Mundo -> IO Mundo
hazMovimientoDamas raton mundo = do
  -- Preparamos el acceso al archivo temporal
  temporal <- caminoTemporal
  -- Comprobamos si hay partidas guardadas
  hayPartidas <- hayPartidasGuardadas
  -- Casillas donde puede haber pulsado el jugador para interaccionar con el juego
  let posCasillas = casillasBlancas
  -- Comprobamos si ha pulsado cerca de alguna casilla para realizar una acción de juego  
  let posBotones = [posOpciones, posCargarJuego, posGuardarJuego, posVolver]
  let pulsacion = devuelvePulsacion raton casillasBlancas posBotones
  -- Finalmente realizamos la acción en caso de que la hubiera y fuera realizable ó simplemente no devolvemos nada nuevo
  accionRealizada mundo pulsacion temporal hayPartidas

{- Función para el turno de la máquina -}
mueveMaquinaDamas :: Mundo -> IO Mundo
mueveMaquinaDamas mundo = do
  let juego = dameJuego mundo
  let dif = dameDificultad mundo
  let prof = dameProfundidad mundo
  let marca = dameMarca mundo
  let adicional = dameAdicional mundo
  let marcaMaquina = marcaDeLaMaquina marca juego
  mn@(e, _) <- trataDificultad (dameMovimiento mundo) dif prof marcaMaquina
  let vivas = piezasVivas e
  let deLaMaquina | marcaMaquina == "B" = cabeza "mueveMaquinaDamas" vivas ++ (vivas !! 1)
        | otherwise = (vivas !! 2) ++ (vivas !! 3)
  punt <- puntuaDamas e $ buscaPieza e $ cabeza "mueveMaquinaDamas" deLaMaquina
  let ad
        | (marcaMaquina == "B" && punt > 0) || (marcaMaquina == "N" && punt > 0) = vivas ++ [["maquina"]]
        | (marcaMaquina == "B" && punt < 0) || (marcaMaquina == "N" && punt < 0) = adicional ++ [["humano"]]
        | otherwise = vivas ++ [["empate"]]
  let adic | finDamas e = ad
        | otherwise = adicional
  let nuevoMundo = (mn, juego, dif, prof, marca, dameTurno mundo, dameSeleccionado mundo, False, adic)
  return nuevoMundo
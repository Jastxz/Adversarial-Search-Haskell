module IOgato
  ( -- Funciones gráficas
    pintaOpcionesGato,
    manejaOpcionesGato,
    pintaJuegoGato,
    hazMovimientoGato,
    mueveMaquinaGato,
  )
where

import Data.List
import Data.Matrix
import FuncionesGato
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Tipos
import Utiles
import UtilesGraficos
import GuardarCargar
import Interconexion
import MiniMax

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones de uso de algoritmo
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

trataDificultad :: Movimiento -> Int -> Int -> String -> IO Movimiento
trataDificultad m@(t, pos) dif prof marca
  | dif == 0 = ponAleatorio t marca
  | otherwise = usaNegamax m dif prof marca "gato"

ponAleatorio :: Tablero -> String -> IO Movimiento
ponAleatorio t marca = do
  al <- now
  let movimientosPosibles = movsGato t marca
  let limiteValidas = length movimientosPosibles
  let a = mod al limiteValidas
  let mov = movimientosPosibles !! a
  return mov

usaNegamax :: Movimiento -> Int -> Int -> String -> String -> IO Movimiento
usaNegamax m dif prof marca juego = do
  mejorTablero <- negamax m dif prof marca juego
  let mejorMovimiento = (fst mejorTablero, snd m)
  return mejorMovimiento

inicializaTableroParaCasoGato :: Int -> IO Movimiento
inicializaTableroParaCasoGato turno = usaNegamax falsoInicial 3 5 "R" "gato"
  where
    falsoInicial = inicial (turnoApos turno)

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones IO para ejecutar el programa con gráficos
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

{- Funciones de las opciones -}
pintaOpcionesGato :: Mundo -> IO Picture
pintaOpcionesGato mundo = do
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
  let mrc | marca == "R" = 0
        | otherwise = 1
  bloqueMarcas <- creaBloque alturasMarcas etiquetaBando infoMarcas mrc distribucionOpciones
  tableroMostrado <- pintaComienzoTablero mov
  -- Preparamos los botones
  menu <- creaBoton posMenu etiquetaMenu
  cargar <- creaBoton posCargar etiquetaCargar
  comenzar <- creaBoton posBoton etiquetaComenzar
  -- Devolvemos la imagen
  return $ pictures [borde, bloqueDif, bloqueMarcas, tableroMostrado, menu, cargar, comenzar]

manejaOpcionesGato :: Point -> Mundo -> IO Mundo
manejaOpcionesGato raton mundo = do
  let turno = dameTurno mundo
  -- Valores de separación entre las casillas de las opciones
  let iC = fst distribucionOpciones
  let eC = snd distribucionOpciones
  -- Buscando la casilla en cuestión
  let (indice, columna) = opcionPulsada raton alturasEstaticas infoEstatica iC eC
  let menu = pulsaCerca raton posMenu
  let cargar = pulsaCerca raton posCargar
  let comenzar = pulsaCerca raton posBoton
  -- Preparamos las variables para el caso de que empiece la máquina en el primer turno
  movMaquina <- inicializaTableroParaCasoGato turno
  -- Cambiamos la información del juego a ejecutar y preparamos el tablero inicial
  nuevoMundo@(m, j, d, p, ma, t, s, e, ad) <- cambiaOpcion raton mundo indice columna
  let mundoMaquina = (movMaquina, j, d, p, ma, t, s, e, ad)
  let mundoAejecutar
        | menu = menuInicial
        | cargar = menuCargarPartida
        | comenzar && ma == "R" = creaTableroConOpciones nuevoMundo
        | comenzar && ma /= "R" = creaTableroConOpciones mundoMaquina
        | otherwise = nuevoMundo
  return mundoAejecutar

{- Funciones intrínsecas del juego -}
pintaJuegoGato :: Mundo -> IO Picture
pintaJuegoGato mundo = do
  let estado = (fst . dameMovimiento) mundo
  let seleccionado = dameSeleccionado mundo
  let esMaquina = dameEsMaquina mundo
  let alturaMensajes = ancho + 50
  -- Texto de turno
  let turno = translate (- correccionPosicion2 ancho) alturaMensajes $ texto $ dameMensajeTurno esMaquina
  -- Dibujo del tablero
  let borde = rectangleWire tamTablero tamTablero
  let casPosible = color green $ rectangleSolid diferenciaParaCasillas diferenciaParaCasillas
  let posRaton = buscaPieza estado "R"
  let posGato = buscaPieza estado seleccionado
  let vaciasRaton = casillasVaciasRaton estado posRaton
  let vaciasGatos = casillasVaciasGatos estado posGato
  let posicionesValidas
        | seleccionado == "R" = vaciasRaton
        | seleccionado `elem` nombresGatos = vaciasGatos
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

hazMovimientoGato :: Point -> Mundo -> IO Mundo
hazMovimientoGato raton mundo = do
  -- Preparamos el acceso al archivo temporal
  temporal <- caminoTemporal
  -- Comprobamos si ha pulsado cerca de alguna casilla para realizar una acción de juego  
  let posBotones = [posOpciones, posCargarJuego, posGuardarJuego, posVolver]
  let pulsacion = devuelvePulsacion raton casillasBlancas posBotones
  -- Finalmente realizamos la acción en caso de que la hubiera y fuera realizable ó simplemente no devolvemos nada nuevo
  accionRealizada mundo pulsacion temporal

{- Función para el turno de la máquina -}
mueveMaquinaGato :: Mundo -> IO Mundo
mueveMaquinaGato mundo = do
  let juego = dameJuego mundo
  let dif = dameDificultad mundo
  let prof = dameProfundidad mundo
  let marca = dameMarca mundo
  mn@(e,p) <- trataDificultad (dameMovimiento mundo) dif prof (marcaDeLaMaquina marca juego)
  let posRaton = buscaPieza e "R"
  let posGatos = [buscaPieza e m | m <- nombresGatos]
  let ad | (marca == "R") && ratonEscapado e posRaton posGatos = [["humano"]]
        | (marca == "G") && ratonEncerrado e posRaton = [["humano"]]
        | (marca == "G") && ratonEscapado e posRaton posGatos = [["maquina"]]
        | (marca == "R") && ratonEncerrado e posRaton = [["maquina"]]
        | otherwise = dameAdicional mundo
  return (mn, juego, dif, prof, marca, dameTurno mundo, dameSeleccionado mundo, False, ad)
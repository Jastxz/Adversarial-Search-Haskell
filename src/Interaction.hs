module Interaction
  ( mainInteraction,
  )
where

import Data.Matrix
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import GuardarCargar
import IO3enRaya
import IOdamas
import IOgato
import Interconexion
import Tipos
import Utiles
import UtilesGraficos

mainInteraction :: IO ()
mainInteraction = do
  -- Ejecución del programa con gráficos
  playIO ventanaJuego fondo tasaDeRefresco menuInicial dibujaMundo manejaEntrada actualiza

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones para los gráficos
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

{- Componentes de la función principal -}
-- ---------------------------------------------------------------------------------
ventanaJuego :: Display
ventanaJuego = InWindow "TFG" tamañoVentana (80, 70)

fondo :: Color
fondo = light orange

-- Cuantas veces se actualiza el dibujo cada segundo
tasaDeRefresco :: Int
tasaDeRefresco = 1

dibujaMundo :: Mundo -> IO Picture
dibujaMundo mundo = case dameClave mundo of
  "menu" -> pintaMenu
  "opciones" -> pintaOpciones juego mundo
  "cargar" -> pintaMenuCarga
  _ -> pintaJuego juego mundo
  where
    juego = dameJuego mundo

manejaEntrada :: Event -> Mundo -> IO Mundo
manejaEntrada evento mundo
  | EventKey (MouseButton LeftButton) Up _ raton <- evento = hazAccion raton mundo
  | otherwise = return mundo

-- Función de distribución de casos similar a dibujaMundo
hazAccion :: Point -> Mundo -> IO Mundo
hazAccion raton mundo = case dameClave mundo of
  "menu" -> seleccionaJuego raton mundo
  "opciones" -> escogeOpcion raton mundo
  "cargar" -> escogePartida raton mundo
  _ -> hazMovimiento raton mundo

actualiza :: Float -> Mundo -> IO Mundo
actualiza _ mundo
  | esEstadoFinal estado juego || dameClave mundo `elem` ["menu", "opciones", "cargar"] = return mundo
  | esMaquina = mueveMaquina mundo
  | otherwise = return mundo
  where
    juego = dameJuego mundo
    estado = fst $ dameMovimiento mundo
    esMaquina = dameEsMaquina mundo

-- ---------------------------------------------------------------------------------

{- Gráficos menú -}
-- ---------------------------------------------------------------------------------
pintaMenu :: IO Picture
pintaMenu = do
  let borde = rectangleWire 300 500
  let comienzoLista = fst ordenJuegos
  let evolucionLista = snd ordenJuegos
  let titulos = pictures $ listaTextos listaDeJuegos 'Y' comienzoLista evolucionLista True
  let res = pictures [borde, titulos]
  return res

-- ---------------------------------------------------------------------------------

{- Eventos menú -}
-- ---------------------------------------------------------------------------------
seleccionaJuego :: Point -> Mundo -> IO Mundo
seleccionaJuego raton mundo
  | sel == cabeza "seleccionaJuego" listaDeJuegos = return $ iniciaOpciones "3enRaya"
  | sel == listaDeJuegos !! 1 = return $ iniciaOpciones "gato"
  | sel == listaDeJuegos !! 2 = return $ iniciaOpciones "damas"
  | otherwise = return mundo
  where
    origen = fst ordenJuegos
    modificador = snd ordenJuegos
    relacion = zip listaDeJuegos [(0.0, y) | y <- [origen, origen + modificador ..]]
    tituloSeleccionado = filter (\(_, p) -> pulsaCerca raton p) relacion
    sel | null tituloSeleccionado = "ninguno"
      | otherwise = (fst . cabeza "seleccionaJuego") tituloSeleccionado

-- ---------------------------------------------------------------------------------

{- Distribuidor de gráficos de las opciones -}
-- ---------------------------------------------------------------------------------
pintaOpciones :: String -> Mundo -> IO Picture
pintaOpciones juego mundo = case juego of
  "3enRaya" -> pintaOpciones3enRaya mundo
  "gato" -> pintaOpcionesGato mundo
  "damas" -> pintaOpcionesDamas mundo
  _ -> pintaError 1

-- ---------------------------------------------------------------------------------

{- Eventos opciones -}
-- ---------------------------------------------------------------------------------
escogeOpcion :: Point -> Mundo -> IO Mundo
escogeOpcion raton mundo
  | juego == "3enRaya" = manejaOpciones3enRaya raton mundo'
  | juego == "gato" = manejaOpcionesGato raton mundo'
  | juego == "damas" = manejaOpcionesDamas raton mundo'
  | otherwise = error $ parte1 ++ show juego ++ parte2
  where
    parte1 = "Ha ocurrido un error en la función escogeOpción. El juego "
    parte2 = " no se valora como opcion posible."
    clave = dameClave mundo
    juego = dameJuego mundo
    mundo'
      | clave == "opciones" = mundo
      | otherwise = juegoIni
    juegoIni = case juego of
      "3enRaya" -> (tableroVacio "opciones", "3enRaya", 0, 0, "O", 0, "", False, [["nada"]])
      "gato" -> (tableroVacio "opciones", "gato", 0, 0, "R", 11, "", False, [["nada"]])
      "damas" -> (tableroVacio "opciones", "damas", 0, 0, "B", 11, "", False, [["nada"]])
      _ -> mundo

-- ---------------------------------------------------------------------------------

{- Distribuidor de gráficos de los juegos -}
-- ---------------------------------------------------------------------------------
pintaJuego :: String -> Mundo -> IO Picture
pintaJuego juego mundo
  | esEstadoFinal estado juego = pintaFin mundo
  | otherwise = case juego of
    "3enRaya" -> pintaJuego3enRaya mundo
    "gato" -> pintaJuegoGato mundo
    "damas" -> pintaJuegoDamas mundo
    _ -> pintaError 2
  where
    estado = (fst . dameMovimiento) mundo

-- ---------------------------------------------------------------------------------

{- Eventos juegos -}
-- ---------------------------------------------------------------------------------
hazMovimiento :: Point -> Mundo -> IO Mundo
hazMovimiento raton mundo
  | esEstadoFinal estado juego = return menuInicial
  | esMaquina = return mundo
  | juego == "3enRaya" = hazMovimiento3enRaya raton mundo
  | juego == "gato" = hazMovimientoGato raton mundo
  | juego == "damas" = hazMovimientoDamas raton mundo
  | otherwise = error $ parte1 ++ show juego ++ parte2
  where
    estado = fst $ dameMovimiento mundo
    juego = dameJuego mundo
    esMaquina = dameEsMaquina mundo
    parte1 = "Ha ocurrido un error en la función hazMovimiento. El juego "
    parte2 = " no se valora como opción posible."

-- ---------------------------------------------------------------------------------

{- Actualización de la máquina de forma automática -}
-- ---------------------------------------------------------------------------------
mueveMaquina :: Mundo -> IO Mundo
mueveMaquina mundo
  | juego == "3enRaya" = mueveMaquina3enRaya mundo
  | juego == "gato" = mueveMaquinaGato mundo
  | juego == "damas" = mueveMaquinaDamas mundo
  | otherwise = error $ parte1 ++ show juego ++ parte2
  where
    juego = dameJuego mundo
    parte1 = "Ha ocurrido un error en la función mueveMaquina. El juego "
    parte2 = " no se valora como opción posible."

-- ---------------------------------------------------------------------------------

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones auxiliares
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
dameClave :: Mundo -> String
dameClave ((estado, pos), _, _, _, _, _, _, _, _) = estado ! pos
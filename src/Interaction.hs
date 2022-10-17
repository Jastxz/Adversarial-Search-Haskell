module Interaction
  ( mainInteraction,
  )
where

import Data.Matrix
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import IO3enRaya
import IOgato
import Tipos
import Utiles
import UtilesGraficos

mainInteraction :: IO ()
mainInteraction = do
  -- Ejecución del programa con IO básico
  {- putStrLn "Escoja el juego al que desea jugar escribiendo el número correspondiente."
  putStrLn "0 para el 3 en raya. 1 para el gato y el ratón."
  juego <- leeDigito "Introduzca el número: "
  juegoAlanzar juego -}

  -- Ejecución del programa con gráficos
  putStrLn "Cargando..."
  playIO ventanaJuego fondo tasaDeRefresco inicial dibujaMundo manejaEntrada actualiza
  putStrLn "¡Gracias por jugar!"

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones para el programa IO básico
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

juegoAlanzar :: Int -> IO ()
juegoAlanzar juego
  | juego == 0 = interactua3enRaya
  | otherwise = interactuaGato

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones para los gráficos
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

{- Componentes de la función principal -}
-- ---------------------------------------------------------------------------------
ventanaJuego :: Display
ventanaJuego = InWindow "TFG" (1400, 700) (50, 40)

fondo :: Color
fondo = dark yellow

-- Cuantas veces se actualiza el dibujo cada segundo
tasaDeRefresco :: Int
tasaDeRefresco = 1

inicial :: Mundo
inicial = (tableroVacio "menu", "menu", 0, 0, "menu", 0, "", False)

dibujaMundo :: Mundo -> IO Picture
dibujaMundo mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina)
  | clave == "menu" = pintaMenu
  | clave == "opciones" = pintaOpciones juego mundo
  | otherwise = pintaJuego juego mundo
  where
    clave = estado ! pos

manejaEntrada :: Event -> Mundo -> IO Mundo
manejaEntrada evento mundo
  | EventKey (MouseButton LeftButton) Up _ raton <- evento = hazAccion raton mundo
  | otherwise = return mundo

actualiza :: Float -> Mundo -> IO Mundo
actualiza _ mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina)
  | esEstadoFinal estado juego = return mundo
  | clave == "menu" = return mundo
  | clave == "opciones" = return mundo
  | esMaquina = mueveMaquina mundo
  | otherwise = return mundo
  where
    clave = estado ! pos

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
seleccionaJuego raton mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina)
  | tresEnRaya = return $ iniciaOpciones "3enRaya"
  | gato = return $ iniciaOpciones "gato"
  | otherwise = return mundo
  where
    origen = fst ordenJuegos
    modificador = snd ordenJuegos
    relacion = zip [origen, origen + modificador ..] [0 .. (length listaDeJuegos - 1)]
    posiciones = [(0.0, y) | (y, _) <- relacion]
    pos3enRaya = head posiciones
    posGato = posiciones !! 1
    tresEnRaya = pulsaCerca raton pos3enRaya
    gato = pulsaCerca raton posGato

-- ---------------------------------------------------------------------------------

{- Distribuidor de gráficos de las opciones -}
-- ---------------------------------------------------------------------------------
pintaOpciones :: String -> Mundo -> IO Picture
pintaOpciones juego mundo
  | juego == "3enRaya" = pintaOpciones3enRaya mundo
  | juego == "gato" = pintaOpcionesGato mundo
  | otherwise = pintaError 1

-- ---------------------------------------------------------------------------------

{- Eventos opciones -}
-- ---------------------------------------------------------------------------------
escogeOpcion :: Point -> Mundo -> IO Mundo
escogeOpcion raton mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina)
  | juego == "3enRaya" = manejaOpciones3enRaya raton mundo3
  | juego == "gato" = manejaOpcionesGato raton mundoG
  | otherwise = error $ parte1 ++ show juego ++ parte2
    where
      parte1 = "Ha ocurrido un error en la función escogeOpción. El juego "
      parte2 = " no se valora como opcion posible."
      mundo3 | (estado ! (1,1)) == "opciones" = mundo | otherwise = (tableroVacio "opciones", "3enRaya", 0, 0, "O", 0, "", False)
      mundoG | (estado ! (1,1)) == "opciones" = mundo | otherwise = (tableroVacio "opciones", "gato", 0, 0, "R", 11, "", False)

-- ---------------------------------------------------------------------------------

{- Distribuidor de gráficos de los juegos -}
-- ---------------------------------------------------------------------------------
pintaJuego :: String -> Mundo -> IO Picture
pintaJuego juego mundo@(mov@(estado, pos), j, dif, prof, marca, turno, seleccionado, esMaquina)
  | esEstadoFinal estado juego = pintaFin mundo
  | juego == "3enRaya" = pintaJuego3enRaya mundo
  | juego == "gato" = pintaJuegoGato mundo
  | otherwise = pintaError 2

-- ---------------------------------------------------------------------------------

{- Eventos juegos -}
-- ---------------------------------------------------------------------------------
hazMovimiento :: Point -> Mundo -> IO Mundo
hazMovimiento raton mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina)
  | esEstadoFinal estado juego = return inicial
  | juego == "3enRaya" = hazMovimiento3enRaya raton mundo
  | juego == "gato" = hazMovimientoGato raton mundo
  | otherwise = error $ parte1 ++ show juego ++ parte2
  where
    parte1 = "Ha ocurrido un error en la función hazMovimiento. El juego "
    parte2 = " no se valora como opción posible."

-- ---------------------------------------------------------------------------------

{- Actualización de la máquina de forma automática -}
-- ---------------------------------------------------------------------------------
mueveMaquina :: Mundo -> IO Mundo
mueveMaquina mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina)
  | juego == "3enRaya" = mueveMaquina3enRaya mundo
  | juego == "gato" = mueveMaquinaGato mundo
  | otherwise = error $ parte1 ++ show juego ++ parte2
  where
    parte1 = "Ha ocurrido un error en la función mueveMaquina. El juego "
    parte2 = " no se valora como opción posible."

-- ---------------------------------------------------------------------------------

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones auxiliares de los gráficos
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

-- Lista de los juegos aceptados
listaDeJuegos :: [String]
listaDeJuegos = ["3 En Raya", "El gato y el raton", "Damas"]

ordenJuegos :: Point
ordenJuegos = (220.0, -40.0)

-- Movimiento inicial vacio para el menú
tableroVacio :: String -> Movimiento
tableroVacio nombre = (tab, pos)
  where
    tab = matrix 1 1 $ \(i, j) -> nombre
    pos = (1, 1)

-- Función de distribución de casos similar a dibujaMundo
hazAccion :: Point -> Mundo -> IO Mundo
hazAccion raton mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina)
  | clave == "menu" = seleccionaJuego raton mundo
  | clave == "opciones" = escogeOpcion raton mundo
  | otherwise = hazMovimiento raton mundo
  where
    clave = estado ! pos

-- Mundo para cambiar a las opciones de un juego cuando este se ha seleccionado
iniciaOpciones :: String -> Mundo
iniciaOpciones juego = (tableroVacio "opciones", juego, 0, 0, "opciones", 0, "", False)
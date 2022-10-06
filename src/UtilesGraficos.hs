module UtilesGraficos
  ( 
    posListaDeJuegos,
    texto,
    listaTextos,
    dibujaCheckbox,
    boton,
    pulsaCerca,
    pulsaCasilla,
    cercaCasilla,
    pintaError
  )
where

import Data.Matrix
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

posListaDeJuegos :: Float
posListaDeJuegos = -140.0

texto :: String -> Picture
texto = scale 0.2 0.2 . color black . text

listaTextos :: [String] -> Char -> Float -> Float -> [Picture]
listaTextos [] _ _ _ = []
listaTextos (t:ts) eje actual modificador
    | eje == 'X' || eje == 'x' = translate actual posListaDeJuegos tx : siguiente
    | eje == 'Y' || eje == 'y' = translate posListaDeJuegos actual tx : siguiente
    | otherwise = error "El eje especificado a la función listaTextos no es correcto"
      where
        siguiente = listaTextos ts eje (actual + modificador) modificador
        tx = texto t

dibujaCheckbox :: Int -> Int -> Char -> Float -> Float -> [Picture]
dibujaCheckbox total elegido eje actual modificador
    | total < 0 = []
    | eje == 'X' || eje == 'x' = translate actual 0 checkbox : siguiente
    | eje == 'Y' || eje == 'y' = translate 0 actual checkbox : siguiente
    | otherwise = error "El eje especificado a la función dibujaCheckbox no es correcto"
        where
            checkbox | total == elegido = cuadroRelleno | otherwise = cuadroVacio
            siguiente = dibujaCheckbox (total - 1) elegido eje (actual + modificador) modificador

boton :: String -> Picture
boton palabra = pictures [fondo, tx]
    where
        fondo = color (dark green) (rectangleSolid 30 15)
        tx = color white (texto palabra)

pulsaCerca :: Point -> Point -> Bool
pulsaCerca (x,y) (i,j)
  | cercaX x i && cercaY y j = True
  | otherwise = False

cercaX :: Float -> Float -> Bool
cercaX a b
  | resta <= 100.0 = True
  | otherwise = False
    where
      resta = abs $ abs a - abs b

cercaY :: Float -> Float -> Bool
cercaY a b
  | resta <= 40.0 = True
  | otherwise = False
    where
      resta = abs $ abs a - abs b

pulsaCasilla :: Point -> Point -> Bool
pulsaCasilla (x,y) (i,j)
  | cercaCasilla x i && cercaCasilla y j = True
  | otherwise = False

cercaCasilla :: Float -> Float -> Bool
cercaCasilla a b
  | resta <= 60.0 = True
  | otherwise = False
    where
      resta = abs $ abs a - abs b

{- Códigos de error
    1 - Error al cargar el dibujo de las opciones de los juegos. Juego no encontrado.
    2 - Error al cargar el dibujo de los juegos. Juego no encontrado.
 -}
pintaError :: Int -> IO Picture
pintaError codigo = do
  let borde = rectangleWire 700 500
  let error | codigo == 1 = "Error en pintaOpciones, parece que el nombre del juego seleccionado no está disponible"
        | codigo == 2 = "Error en pintaJuegos, parece que el nombre del juego seleccionado no está disponible"
        | otherwise = "Error desconocido"
  let mensaje = texto error
  let res = pictures [borde,mensaje]
  return res

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones auxiliares
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
cuadroRelleno :: Picture
cuadroRelleno = color black $ rectangleSolid 10 10

cuadroVacio :: Picture
cuadroVacio = rectangleWire 10 10
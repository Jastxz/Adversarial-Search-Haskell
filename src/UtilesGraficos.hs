module UtilesGraficos
  ( 
    -- 'Constantes' y colores
    posListaDeJuegos,
    tamCheckbox,
    correccionPosicion,
    correccionPosicion2,
    texto,
    marron,
    -- Funciones
    listaTextos,
    dibujaCheckbox,
    boton,
    pulsaBox,
    cercaBox,
    pulsaCerca,
    pulsaCercaMini,
    pulsaCasilla,
    cercaCasilla,
    pintaFin,
    pintaError
  )
where

import Data.Matrix
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Tipos
import Utiles

posListaDeJuegos :: Float
posListaDeJuegos = -140.0

tamCheckbox :: Float
tamCheckbox = 10.0

correccionPosicion :: Float -> Float
correccionPosicion ancho = ancho/2.0

correccionPosicion2 :: Float -> Float
correccionPosicion2 ancho = ancho/4.0

texto :: String -> Picture
texto = scale 0.2 0.2 . color black . text

marron :: Color
marron = makeColorI 140 76 0 255

listaTextos :: [String] -> Char -> Float -> Float -> Bool -> [Picture]
listaTextos [] _ _ _ _ = []
listaTextos (t:ts) eje actual modificador menu
    | eje == 'X' || eje == 'x' = translate actual constante tx : siguiente
    | eje == 'Y' || eje == 'y' = translate constante actual tx : siguiente
    | otherwise = error "El eje especificado a la función listaTextos no es correcto"
      where
        siguiente = listaTextos ts eje (actual + modificador) modificador menu
        tx = texto t
        constante | menu = posListaDeJuegos | otherwise = 0

dibujaCheckbox :: Int -> Int -> Char -> Float -> Float -> [Picture]
dibujaCheckbox total elegido eje actual modificador = dibujaCheckbox' total elegido eje actual modificador 0
dibujaCheckbox' total elegido eje actual modificador acum
    | acum > total = []
    | eje == 'X' || eje == 'x' = translate actual 0 checkbox : siguiente
    | eje == 'Y' || eje == 'y' = translate 0 actual checkbox : siguiente
    | otherwise = error "El eje especificado a la función dibujaCheckbox no es correcto"
        where
            checkbox | acum == elegido = cuadroRelleno | otherwise = cuadroVacio
            siguiente = dibujaCheckbox' total elegido eje (actual + modificador) modificador (acum + 1)

boton :: String -> Float -> Float -> Picture
boton palabra an al = pictures [fondo, tx]
    where
        fondo = color (dark green) (rectangleSolid an al)
        tx = translate (-correccionPosicion an) (-correccionPosicion2 al) $ color white (texto palabra)

-- -----------------------------------------------------------------------------------------------------------------------
pulsaBox :: Point -> Point -> Bool
pulsaBox (x,y) (i,j)
  | cercaBox x i && cercaBox y j = True
  | otherwise = False

-- Aux
cercaBox :: Float -> Float -> Bool
cercaBox a b
  | resta <= 10.0 = True
  | otherwise = False
    where
      resta = distanciaEuclidea a b
-- -----------------------------------------------------------------------------------------------------------------------
-- -----------------------------------------------------------------------------------------------------------------------
pulsaCerca :: Point -> Point -> Bool
pulsaCerca (x,y) (i,j)
  | cercaX x i && cercaY y j = True
  | otherwise = False

-- Aux
cercaX :: Float -> Float -> Bool
cercaX a b
  | resta <= 150.0 = True
  | otherwise = False
    where
      resta = distanciaEuclidea a b

-- Aux
cercaY :: Float -> Float -> Bool
cercaY a b
  | resta <= 20.0 = True
  | otherwise = False
    where
      resta = distanciaEuclidea a b
-- -----------------------------------------------------------------------------------------------------------------------
-- -----------------------------------------------------------------------------------------------------------------------
pulsaCercaMini :: Point -> Point -> Bool
pulsaCercaMini (x,y) (i,j)
  | cercaXMini x i && cercaYMini y j = True
  | otherwise = False

-- Aux
cercaXMini :: Float -> Float -> Bool
cercaXMini a b
  | resta <= 15.0 = True
  | otherwise = False
    where
      resta = distanciaEuclidea a b

-- Aux
cercaYMini :: Float -> Float -> Bool
cercaYMini a b
  | resta <= 15.0 = True
  | otherwise = False
    where
      resta = distanciaEuclidea a b
-- -----------------------------------------------------------------------------------------------------------------------
-- -----------------------------------------------------------------------------------------------------------------------
pulsaCasilla :: Point -> Point -> Bool
pulsaCasilla (x,y) (i,j)
  | cercaCasilla x i && cercaCasilla y j = True
  | otherwise = False

-- Aux
cercaCasilla :: Float -> Float -> Bool
cercaCasilla a b
  | resta <= 50.0 = True
  | otherwise = False
    where
      resta = distanciaEuclidea a b
-- -----------------------------------------------------------------------------------------------------------------------

pintaFin :: Mundo -> IO Picture
pintaFin mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina) = do
  let borde = translate 0 10 $ color white $ rectangleSolid 350 50
  let tx  | seleccionado == "empate" = "Habeis empatado..." 
        | esMaquina = "Enhorabuena, has ganado."
        | otherwise = "La maquina ha ganado..."
  let mensaje = translate (-150) 0 $ texto tx
  let res = pictures [borde,mensaje]
  return res

{- Códigos de error
    1 - Error al cargar el dibujo de las opciones de los juegos. Juego no encontrado.
    2 - Error al cargar el dibujo de los juegos. Juego no encontrado.
    3 - Error al cargar el dibujo de los juegos. Juego no encontrado.
 -}
pintaError :: Int -> IO Picture
pintaError codigo = do
  let borde = rectangleWire 700 500
  let error | codigo == 1 = "Error en pintaOpciones, parece que el nombre del juego seleccionado no está disponible"
        | codigo == 2 = "Error en pintaJuegos, parece que el nombre del juego seleccionado no está disponible"
        | codigo == 3 = "Error en pintaPantallaFinal, parece que el nombre del juego seleccionado no está disponible"
        | otherwise = "Error desconocido"
  let mensaje = texto error
  let res = pictures [borde,mensaje]
  return res

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones auxiliares
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
cuadroRelleno :: Picture
cuadroRelleno = color black $ rectangleSolid tamCheckbox tamCheckbox

cuadroVacio :: Picture
cuadroVacio = rectangleWire tamCheckbox tamCheckbox
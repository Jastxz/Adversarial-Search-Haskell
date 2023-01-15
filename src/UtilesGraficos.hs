module UtilesGraficos
  ( -- 'Constantes' y colores
    listaDeJuegos,
    ordenJuegos,
    posListaDeJuegos,
    tamCheckbox,
    anchoBoton,
    altoBoton,
    correccionPosicion,
    correccionPosicion2,
    texto,
    marron,
    tamañoVentana,
    tamañoRectangulo,
    menuInicial,
    horizontal,
    vertical,
    etiquetaBando,
    etiquetaCargar,
    etiquetaComenzar,
    etiquetaGuardar,
    etiquetaMarca,
    etiquetaMenu,
    etiquetaNivel,
    etiquetaOpciones,
    etiquetaTurno,
    etiquetaVolver,
    -- Funciones
    dameMovimiento,
    dameJuego,
    dameDificultad,
    dameProfundidad,
    dameMarca,
    dameTurno,
    dameSeleccionado,
    dameEsMaquina,
    dameAdicional,
    ponMovimiento,
    ponJuego,
    ponDificultad,
    ponProfundidad,
    ponMarca,
    ponTurno,
    ponSeleccionado,
    ponEsMaquina,
    ponAdicional,
    iniciaOpciones,
    creaBloque,
    creaBoton,
    dameMensajeTurno,
    listaTextos,
    dibujaCheckbox,
    boton,
    opcionPulsada,
    devuelvePulsacion,
    pulsaBox,
    cercaBox,
    pulsaCerca,
    pulsaCercaMini,
    pulsaCasilla,
    cercaCasilla,
    pintaFin,
    pintaError,
  )
where

import Data.Matrix
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Tipos
import Utiles

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Constantes
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
-- Lista de los juegos aceptados
listaDeJuegos :: [String]
listaDeJuegos = ["Tic-tac-toe", "Cats VS Mouse", "Checkers"]

ordenJuegos :: Point
ordenJuegos = (220.0, -40.0)

posListaDeJuegos :: Float
posListaDeJuegos = -140.0

tamCheckbox :: Float
tamCheckbox = 10.0

anchoBoton :: Float
anchoBoton = 150.0

altoBoton :: Float
altoBoton = 40.0

correccionPosicion :: Float -> Float
correccionPosicion ancho = ancho / 2.0

correccionPosicion2 :: Float -> Float
correccionPosicion2 ancho = ancho / 4.0

texto :: String -> Picture
texto = uncurry scale tamañoTexto . color black . text

marron :: Color
marron = makeColorI 140 76 0 255

menuInicial :: Mundo
menuInicial = (tableroVacio "menu", "menu", 0, 0, "menu", 0, "", False, [["nada"]])

horizontal :: Char
horizontal = 'X'

vertical :: Char
vertical = 'Y'

tamañoVentana :: Pos
tamañoVentana = (1400, 700)

tamañoRectangulo :: Point
tamañoRectangulo = (1000, 500)

etiquetaNivel :: String
etiquetaNivel = "Level"

etiquetaTurno :: String
etiquetaTurno = "Turn"

etiquetaMarca :: String
etiquetaMarca = "Mark"

etiquetaBando :: String
etiquetaBando = "Play as"

etiquetaMenu :: String
etiquetaMenu = "Main menu"

etiquetaCargar :: String
etiquetaCargar = "Load game"

etiquetaGuardar :: String
etiquetaGuardar = "Save game"

etiquetaOpciones :: String
etiquetaOpciones = "Options"

etiquetaComenzar :: String
etiquetaComenzar = "Start"

etiquetaVolver :: String
etiquetaVolver = "Back"

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

dameMovimiento :: Mundo -> Movimiento
dameMovimiento (mov, _, _, _, _, _, _, _, _) = mov

dameJuego :: Mundo -> String
dameJuego (_, juego, _, _, _, _, _, _, _) = juego

dameDificultad :: Mundo -> Int
dameDificultad (_, _, dif, _, _, _, _, _, _) = dif

dameProfundidad :: Mundo -> Int
dameProfundidad (_, _, _, prof, _, _, _, _, _) = prof

dameMarca :: Mundo -> String
dameMarca (_, _, _, _, marca, _, _, _, _) = marca

dameTurno :: Mundo -> Int
dameTurno (_, _, _, _, _, turno, _, _, _) = turno

dameSeleccionado :: Mundo -> String
dameSeleccionado (_, _, _, _, _, _, sel, _, _) = sel

dameEsMaquina :: Mundo -> Bool
dameEsMaquina (_, _, _, _, _, _, _, maq, _) = maq

dameAdicional :: Mundo -> [[String]]
dameAdicional (_, _, _, _, _, _, _, _, ad) = ad

ponMovimiento :: Mundo -> Movimiento -> Mundo
ponMovimiento (_, juego, dif, prof, marca, turno, sel, maq, ad) m = (m, juego, dif, prof, marca, turno, sel, maq, ad)

ponJuego :: Mundo -> String -> Mundo
ponJuego (mov, _, dif, prof, marca, turno, sel, maq, ad) j = (mov, j, dif, prof, marca, turno, sel, maq, ad)

ponDificultad :: Mundo -> Int -> Mundo
ponDificultad (mov, juego, _, prof, marca, turno, sel, maq, ad) d = (mov, juego, d, prof, marca, turno, sel, maq, ad)

ponProfundidad :: Mundo -> Int -> Mundo
ponProfundidad (mov, juego, dif, _, marca, turno, sel, maq, ad) p = (mov, juego, dif, p, marca, turno, sel, maq, ad)

ponMarca :: Mundo -> String -> Mundo
ponMarca (mov, juego, dif, prof, _, turno, sel, maq, ad) m = (mov, juego, dif, prof, m, turno, sel, maq, ad)

ponTurno :: Mundo -> Int -> Mundo
ponTurno (mov, juego, dif, prof, marca, _, sel, maq, ad) t = (mov, juego, dif, prof, marca, t, sel, maq, ad)

ponSeleccionado :: Mundo -> String -> Mundo
ponSeleccionado (mov, juego, dif, prof, marca, turno, _, maq, ad) s = (mov, juego, dif, prof, marca, turno, s, maq, ad)

ponEsMaquina :: Mundo -> Bool -> Mundo
ponEsMaquina (mov, juego, dif, prof, marca, turno, sel, _, ad) m = (mov, juego, dif, prof, marca, turno, sel, m, ad)

ponAdicional :: Mundo -> [[String]] -> Mundo
ponAdicional (mov, juego, dif, prof, marca, turno, sel, maq, _) a = (mov, juego, dif, prof, marca, turno, sel, maq, a)

-- Mundo para cambiar a las opciones de un juego cuando este se ha seleccionado
iniciaOpciones :: String -> Mundo
iniciaOpciones juego = (tableroVacio "opciones", juego, 0, 0, "opciones", 0, "", False, [["nada"]])

creaBloque :: [Float] -> String -> [String] -> Int -> Point -> IO Picture
creaBloque alturas titulo info caja (inicioCasillas, evolucionCasillas) = do
  let [a1, a2, a3] = alturas
  let pintaTitulo = translate inicioCasillas a1 $ texto titulo
  let pintaInfo = translate 0 a2 $ pictures $ listaTextos info horizontal inicioCasillas evolucionCasillas False
  let lInfo = length info
  let cbx = pictures $ dibujaCheckbox (lInfo - 1) caja horizontal inicioCasillas evolucionCasillas
  let checkbox = translate 0 a3 cbx
  return $ pictures [pintaTitulo, pintaInfo, checkbox]

creaBoton :: Point -> String -> IO Picture
creaBoton (x, y) etiqueta = return $ translate x y $ boton etiqueta anchoBoton altoBoton

dameMensajeTurno :: Bool -> String
dameMensajeTurno esMaquina
  | esMaquina = "Machine's turn"
  | otherwise = "Your turn"

listaTextos :: [String] -> Char -> Float -> Float -> Bool -> [Picture]
listaTextos [] _ _ _ _ = []
listaTextos (t : ts) eje actual modificador menu
  | eje == 'X' || eje == 'x' = translate actual constante tx : siguiente
  | eje == 'Y' || eje == 'y' = translate constante actual tx : siguiente
  | otherwise = error "El eje especificado a la función listaTextos no es correcto"
  where
    siguiente = listaTextos ts eje (actual + modificador) modificador menu
    tx = texto t
    constante
      | menu = posListaDeJuegos
      | otherwise = 0

dibujaCheckbox :: Int -> Int -> Char -> Float -> Float -> [Picture]
dibujaCheckbox total elegido eje actual modificador = dibujaCheckbox' total elegido eje actual modificador 0

dibujaCheckbox' :: Int -> Int -> Char -> Float -> Float -> Int -> [Picture]
dibujaCheckbox' total elegido eje actual modificador acum
  | acum > total = []
  | eje == 'X' || eje == 'x' = translate actual 0 checkbox : siguiente
  | eje == 'Y' || eje == 'y' = translate 0 actual checkbox : siguiente
  | otherwise = error "El eje especificado a la función dibujaCheckbox no es correcto"
  where
    checkbox
      | acum == elegido = cuadroRelleno
      | otherwise = cuadroVacio
    siguiente = dibujaCheckbox' total elegido eje (actual + modificador) modificador (acum + 1)

boton :: String -> Float -> Float -> Picture
boton palabra an al = pictures [fondo, tx]
  where
    fondo = color azure (rectangleSolid an al)
    tx = translate (- correccionPosicion an) (- correccionPosicion2 al) $ textoBoton palabra

opcionPulsada :: Point -> [Float] -> [[String]] -> Float -> Float -> (Int, String)
opcionPulsada (x, y) alturasEstaticas infoEstatica iC eC = (ind, columna)
  where
    indice = filter (\(a, _) -> cercaBox y a) $ zip alturasEstaticas [0 ..]
    ind
      | null indice = 99
      | otherwise = (snd . cabeza "opcionPulsada") indice
    fila
      | null indice = cabeza "opcionPulsada" infoEstatica
      | otherwise = infoEstatica !! ind
    limite = length fila
    indice2 = filter (\(l, _) -> cercaBox x l) $ zip [iC, iC + eC ..] [0 .. (limite - 1)]
    columna
      | null indice2 = cabeza "opcionPulsada" fila
      | otherwise = fila !! (snd . cabeza "opcionPulsada") indice2

devuelvePulsacion :: Point -> [Point] -> [Point] -> (String, Point)
devuelvePulsacion raton posCasillas posBotones
  | pulsaCerca raton posOpciones = (etiquetaOpciones, posOpciones)
  | pulsaCerca raton posCargarJuego = (etiquetaCargar, posCargarJuego)
  | pulsaCerca raton posGuardarJuego = (etiquetaGuardar, posGuardarJuego)
  | pulsaCerca raton posVolver = (etiquetaVolver, posVolver)
  | (not . null) pulsadas = ("accion", accion)
  | otherwise = ("nada", raton)
  where
    pulsadas = [casilla | casilla <- posCasillas, pulsaCasilla casilla raton]
    accion = cabeza "devuelvePulsacion" pulsadas
    posOpciones = cabeza "devuelvePulsacion" posBotones
    posCargarJuego = posBotones !! 1
    posGuardarJuego = posBotones !! 2
    posVolver = posBotones !! 3

-- -----------------------------------------------------------------------------------------------------------------------
pulsaBox :: Point -> Point -> Bool
pulsaBox (x, y) (i, j)
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
pulsaCerca (x, y) (i, j)
  | cercaX x i && cercaY y j = True
  | otherwise = False

-- Aux
cercaX :: Float -> Float -> Bool
cercaX a b
  | resta <= 90.0 = True
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
pulsaCercaMini (x, y) (i, j)
  | cercaMini x i && cercaMini y j = True
  | otherwise = False

-- Aux
cercaMini :: Float -> Float -> Bool
cercaMini a b
  | resta <= 15.0 = True
  | otherwise = False
  where
    resta = distanciaEuclidea a b

-- -----------------------------------------------------------------------------------------------------------------------
-- -----------------------------------------------------------------------------------------------------------------------
pulsaCasilla :: Point -> Point -> Bool
pulsaCasilla (x, y) (i, j)
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
pintaFin mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional) = do
  let borde = translate 0 10 $ color white $ rectangleSolid 350 50
  let textoFinPartida = cabeza "pintaFin" $ last adicional
  let tx
        | textoFinPartida == "empate" = "Habeis empatado..."
        | textoFinPartida == "humano" = "Enhorabuena, has ganado."
        | textoFinPartida == "maquina" = "La maquina ha ganado..."
        | otherwise = "Algo raro esta pasando en la funcion pintaFin. \nSi no eres el creador del programa entonces avisalo."
  let mensaje = translate (-150) 0 $ texto tx
  let res = pictures [borde, mensaje]
  return res

{- Códigos de error
    1 - Error al cargar el dibujo de las opciones de los juegos. Juego no encontrado.
    2 - Error al cargar el dibujo de los juegos. Juego no encontrado.
    3 - Error al cargar el dibujo de los juegos. Juego no encontrado.
 -}
pintaError :: Int -> IO Picture
pintaError codigo = do
  let borde = rectangleWire 700 500
  let error
        | codigo == 1 = "Error en pintaOpciones, parece que el nombre del juego seleccionado no está disponible"
        | codigo == 2 = "Error en pintaJuegos, parece que el nombre del juego seleccionado no está disponible"
        | codigo == 3 = "Error en pintaPantallaFinal, parece que el nombre del juego seleccionado no está disponible"
        | otherwise = "Error desconocido"
  let mensaje = texto error
  let res = pictures [borde, mensaje]
  return res

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones auxiliares
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

tamañoTexto :: Point
tamañoTexto = (0.2,0.2)

cuadroRelleno :: Picture
cuadroRelleno = color black $ rectangleSolid tamCheckbox tamCheckbox

cuadroVacio :: Picture
cuadroVacio = rectangleWire tamCheckbox tamCheckbox

textoBoton :: String -> Picture
textoBoton = uncurry scale tamañoTexto . color white . text
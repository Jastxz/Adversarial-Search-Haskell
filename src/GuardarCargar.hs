module GuardarCargar
  ( menuCargarPartida,
    cargarPartida,
    guardarPartida,
    caminoTemporal,
    temporalPartida,
    pintaMenuCarga,
    escogePartida,
    directorioPartidas,
  )
where

import Data.List
import Data.Matrix
import Graphics.Gloss
import System.Directory
import Tipos
import Utiles
import UtilesGraficos
import Control.Exception (evaluate)
import Control.DeepSeq (force)

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones constantes
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

menuCargarPartida :: Mundo
menuCargarPartida = (tableroVacio "cargar", "", 0, 0, "", 0, "", False, [["nada"]])

ordenArchivos :: Point
ordenArchivos = (220.0, -40.0)

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones de trabajo
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

cargarPartida :: String -> IO Mundo
cargarPartida caminoArchivo = do
  existe <- doesFileExist caminoArchivo
  if existe
    then do
      contenido <- readFile caminoArchivo
      evaluate (force contenido)
      let lineas = lines contenido
      if null lineas
        then error $ "Archivo pasado a la funcion cargaPartida vacio. Path: " ++ caminoArchivo
        else sacaContenido lineas
    else return menuInicial

guardarPartida :: Mundo -> IO Mundo
guardarPartida mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional) = do
  preparaDirectorios
  caminoPartidas <- directorioPartidas
  archivos <- listDirectory caminoPartidas
  let numArchivos = length [archivo | archivo <- archivos, juego `isPrefixOf` archivo]
  let numArchivo = numArchivos + 1
  let nombreArchivo = caminoPartidas ++ "/" ++ juego ++ "_" ++ show numArchivo ++ ".txt"
  let contenidoArchivo = creaContenido mundo
  writeFile nombreArchivo contenidoArchivo
  seHaCreado <- doesFileExist nombreArchivo
  if seHaCreado
    then do
      print "Save file successfully"
      return mundo
    else do
      let parte1 = "No se ha podido crear el guardado de partida con nombre: "
      let parte2 = nombreArchivo ++ " y contenido: " ++ contenidoArchivo
      error $ parte1 ++ parte2

caminoTemporal :: IO String
caminoTemporal = do
  caminoPartidas <- getTemporaryDirectory
  let nombreArchivo = caminoPartidas ++ "/" ++ "temporalTFG.txt"
  return nombreArchivo

temporalPartida :: Mundo -> IO()
temporalPartida mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional) = do
  nombreArchivo <- caminoTemporal
  let contenidoArchivo = creaContenido mundo
  existe <- doesFileExist nombreArchivo
  if existe
    then do
      removeFile nombreArchivo
      writeFile nombreArchivo contenidoArchivo
    else do
      writeFile nombreArchivo contenidoArchivo

creaContenido :: Mundo -> String
creaContenido mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional) = contenido
  where
    estadoForm = show $ map (cambiaComas . show) (toLists estado)
    posForm = show pos
    juegoForm = juego
    difForm = show dif
    profForm = show prof
    marcaForm = marca
    turnoForm = show turno
    selForm
      | null seleccionado = "nada"
      | otherwise = seleccionado
    maqForm = show esMaquina
    adForm
      | null adicional = "[]"
      | otherwise = show adicional
    contenido = unlines [estadoForm, posForm, juegoForm, difForm, profForm, marcaForm, turnoForm, selForm, maqForm, adForm]

sacaContenido :: [String] -> IO Mundo
sacaContenido contenido = do
  let estadoForm = eliminaBarrasDobles $ cabeza "sacaContenido" contenido
  let posForm = contenido !! 1
  let juego = contenido !! 2
  let difForm = contenido !! 3
  let profForm = contenido !! 4
  let marca = contenido !! 5
  let turnoForm = contenido !! 6
  let selForm = contenido !! 7
  let maqForm = contenido !! 8
  let adForm = eliminaBarrasDobles $ contenido !! 9
  let estado = stringToEstado estadoForm
  let pos = stringToPos posForm
  let dif = stringToInt difForm
  let prof = stringToInt profForm
  let turno = stringToInt turnoForm
  let seleccionado
        | selForm == "nada" = ""
        | otherwise = selForm
  let esMaquina
        | maqForm == "True" = True
        | otherwise = False
  let adicional = stringToLista adForm
  let mundo = ((estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional)
  return mundo

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones gráficos
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

pintaMenuCarga :: IO Picture
pintaMenuCarga = do
  caminoPartidas <- directorioPartidas
  archivos <- listDirectory caminoPartidas
  let borde = rectangleWire 300 500
  let comienzoLista = fst ordenArchivos
  let evolucionLista = snd ordenArchivos
  let titulos = pictures $ listaTextos archivos 'Y' comienzoLista evolucionLista True
  let res = pictures [borde, titulos]
  return res

escogePartida :: Point -> Mundo -> IO Mundo
escogePartida raton mundo = do
  caminoPartidas <- directorioPartidas
  archivos <- listDirectory caminoPartidas
  let origen = fst ordenArchivos
  let modificador = snd ordenArchivos
  let relacion = zip [origen, origen + modificador ..] [0 .. (length archivos - 1)]
  let posiciones = [(0.0, y) | (y, _) <- relacion]
  let arPos = zip archivos posiciones
  let listaConSeleccionado = filter (\(ar, pos) -> pulsaCerca raton pos) arPos
  if null listaConSeleccionado
    then return mundo
    else do
      let archivo = fst $ cabeza "escogePartida" listaConSeleccionado
      let caminoArchivo = caminoPartidas ++ "/" ++ archivo
      cargarPartida caminoArchivo

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones auxiliares
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

directorioPartidas :: IO String
directorioPartidas = do
  directorioActual <- getCurrentDirectory
  return $ directorioActual ++ "/Partidas"

preparaDirectorios :: IO ()
preparaDirectorios = do
  caminoPartidas <- directorioPartidas
  createDirectoryIfMissing False caminoPartidas

cambiaComas :: String -> String
cambiaComas "" = ""
cambiaComas (x:xs)
  | x == ',' = ';' : cambiaComas xs
  | otherwise = x : cambiaComas xs

eliminaBarrasDobles :: String -> String
eliminaBarrasDobles "" = ""
eliminaBarrasDobles (x:xs)
  | x == '\\' = ' ' : eliminaBarrasDobles xs
  | x == '\"' = eliminaBarrasDobles xs
  | otherwise = x : eliminaBarrasDobles xs

stringToEstado :: String -> Tablero
stringToEstado = fromLists . stringToLista

stringToPos :: String -> Pos
stringToPos p
  | cumplePatron = (p1, p2)
  | otherwise = error msj
  where
    cab = cabeza "stringToPos" p
    entero1 = esInt $ takeWhile (/= ',') $ tail p
    parte2 = dropWhile (/= ',') p
    medio = cabeza "stringToPos" parte2
    entero2 = esInt $ takeWhile (/= ')') $ tail parte2
    final = last p
    cumplePatron = cab == '(' && entero1 && medio == ',' && entero2 && final == ')'
    msj = "En la funcion stringToPos esta entrando una posicion invalida. Concretamente: " ++ p
    p1
      | entero1 = stringToInt $ takeWhile (/= ',') $ tail p
      | otherwise = error msj
    p2
      | entero2 = stringToInt $ takeWhile (/= ')') $ tail parte2
      | otherwise = error msj

stringToLista :: String -> [[String]]
stringToLista "" = []
stringToLista "[[\"nada\"]]" = [["nada"]]
stringToLista (x : xs)
  | x == '[' && cab == '[' = stringToLista' sublista : stringToLista resto
  | x == ',' = stringToLista' sublista : stringToLista resto
  | x == ']' = []
  | otherwise = []
  where
    cab = cabeza "stringToLista" xs
    sublista = takeWhile (/= ',') xs
    resto = dropWhile (/= ',') xs

stringToLista' :: String -> [String]
stringToLista' "" = []
stringToLista' (x : xs)
  | x == '[' = stringToLista' xs
  | x == ']' = []
  | x == ';' = stringToLista' xs
  | otherwise = palabra : stringToLista' resto
  where
    palabra = depuraPalabra $ x : takeWhile (/= ';') xs
    resto = dropWhile (/= ';') xs

depuraPalabra :: String -> String
depuraPalabra "" = ""
depuraPalabra (x:xs)
  | x `elem` alfabeto = x : depuraPalabra xs
  | otherwise = depuraPalabra xs
    where
      alfabeto = ['a'..'z'] ++ ['A'..'Z'] ++ ['1'..'9']

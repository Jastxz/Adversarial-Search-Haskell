module GuardarCargar
  ( menuCargarPartida,
    cargarPartida,
    guardarPartida,
    caminoTemporal,
    temporalPartida,
    pintaMenuCarga,
    escogePartida,
    directorioPartidas,
    hayPartidasGuardadas,
  )
where

import Data.List
import Data.List.Split (splitOn)
import Data.Matrix
import Text.Regex (mkRegex, matchRegex)
import Graphics.Gloss
import System.Directory
import System.Directory.Internal.Prelude (when)
import System.FilePath((</>))
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
guardarPartida mundo = do
  preparaDirectorios
  caminoPartidas <- directorioPartidas
  archivos <- listDirectory caminoPartidas
  let juego = dameJuego mundo
  let numArchivo = length [archivo | archivo <- archivos, juego `isPrefixOf` archivo] + 1
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
  return (caminoPartidas </> "temporalTFG.txt")

temporalPartida :: Mundo -> IO()
temporalPartida mundo = do
  nombreArchivo <- caminoTemporal
  let contenidoArchivo = creaContenido mundo
  existe <- doesFileExist nombreArchivo
  when existe (removeFile nombreArchivo)
  writeFile nombreArchivo contenidoArchivo

creaContenido :: Mundo -> String
creaContenido mundo@(mov@(estado, pos), juego, dif, prof, marca, turno, seleccionado, esMaquina, adicional) = contenido
  where
    estadoForm = show $ map (cambiaComas . show) (toLists estado)
    selForm
      | null seleccionado = "nada"
      | otherwise = seleccionado
    adForm
      | null adicional = "[]"
      | otherwise = show adicional
    lista1 = [estadoForm, show pos, juego, show dif, show prof]
    lista2 = [ marca, show turno, selForm, show esMaquina, adForm]
    contenido = unlines $ lista1 ++ lista2

sacaContenido :: [String] -> IO Mundo
sacaContenido contenido = do
  let [estadoForm, posForm, juego, difForm, profForm, marca, turnoForm, selForm, maqForm, adForm] = contenido
  let estado = stringToEstado $ eliminaBarrasDobles estadoForm
  let pos = stringToPos posForm
  let dif = stringToInt difForm
  let prof = stringToInt profForm
  let turno = stringToInt turnoForm
  let seleccionado
        | selForm == "nada" = ""
        | otherwise = selForm
  let esMaquina = maqForm == "True"
  let adicional = stringToLista $ eliminaBarrasDobles adForm
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
  let relacion = zip archivos [(0.0,y) | y<-[origen, origen + modificador ..]]
  let listaConSeleccionado = filter (\(ar, pos) -> pulsaCerca raton pos) relacion
  if null listaConSeleccionado
    then return mundo
    else do
      let archivo = fst $ cabeza "escogePartida" listaConSeleccionado
      cargarPartida $ caminoPartidas </> archivo

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones auxiliares
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

directorioPartidas :: IO String
directorioPartidas = do
  directorioActual <- getCurrentDirectory
  return $ directorioActual </> "Partidas"

preparaDirectorios :: IO ()
preparaDirectorios = do
  caminoPartidas <- directorioPartidas
  createDirectoryIfMissing False caminoPartidas

hayPartidasGuardadas :: IO Bool
hayPartidasGuardadas = do
  caminoPartidas <- directorioPartidas
  archivos <- listDirectory caminoPartidas
  return $ not $ null archivos

cambiaComas :: String -> String
cambiaComas = map (\c -> if c == ',' then ';' else c)

eliminaBarrasDobles :: String -> String
eliminaBarrasDobles = map (\c -> if c == '\\' then ' ' else c) . filter (/='\"')

stringToEstado :: String -> Tablero
stringToEstado = fromLists . stringToLista

stringToPos :: String -> Pos
stringToPos p = case matchRegex (mkRegex "\\(([0-9]+),([0-9]+)\\)") p of
    Just [p1, p2] -> (read p1, read p2)
    _             -> error $ "Entrada inválida en stringToPos: " ++ p

stringToLista :: String -> [[String]]
stringToLista = map (map depuraPalabra . splitOn ";") . filter (not . null) . splitOn "," . tail . init

depuraPalabra :: String -> String
depuraPalabra = filter (`elem` alfabeto)
    where alfabeto = ['a'..'z'] ++ ['A'..'Z'] ++ ['1'..'9']
module Interconexion
  ( movimientosPosibles,
    esEstadoFinal,
    puntuaEstado,
    siguiente,
    marcaDeLaMaquina,
    umbralSegunJuego,
    margenUtilidadSegunJuego,
    umbralMovimientoMagnificoSegunJuego,
  )
where

import Funciones3enRaya
import FuncionesDamas
import FuncionesGato
import Tipos
import Utiles

movimientosPosibles :: Tablero -> Int -> String -> String -> Movimientos
movimientosPosibles estado quienJuega marcaMaquina juego
  | juego == "3enRaya" = movs3enRaya estado marcaMaquina
  | juego == "gato" = movsGato estado marcaMaquina
  | juego == "damas" = movsDamas estado marcaMaquina
  | otherwise = error $ "Error en la función movimientosPosibles, el juego especificado no existe. Juego: " ++ show juego

esEstadoFinal :: Tablero -> String -> Bool
esEstadoFinal t juego
  | juego == "3enRaya" = fin3enRaya t
  | juego == "gato" = finGato t
  | juego == "damas" = finDamas t
  | otherwise = False

puntuaEstado :: Tablero -> Pos -> String -> IO Double
puntuaEstado t pos juego
  | juego == "3enRaya" = puntua3enRaya t pos
  | juego == "gato" = puntuaGato t pos
  | juego == "damas" = puntuaDamas t pos
  | otherwise = return 0.0

siguiente :: Int -> Int
siguiente j = if j == 1 then 2 else 1

marcaDeLaMaquina :: String -> String -> String
marcaDeLaMaquina marca juego
  | juego == "3enRaya" = marcaMaquina3enRaya marca
  | juego == "gato" = marcaMaquinaGato marca
  | juego == "damas" = marcaMaquinaDamas marca
  | otherwise = " "

umbralSegunJuego :: String -> Double
umbralSegunJuego juego
  | juego == "3enRaya" = 5.0
  | juego == "gato" = 0.0
  | juego == "damas" = 5.0
  | otherwise = 0.0

margenUtilidadSegunJuego :: String -> Double
margenUtilidadSegunJuego juego
  | juego == "3enRaya" = 0.0
  | juego == "gato" = 1.0
  | juego == "damas" = 1.0
  | otherwise = 0.0

umbralMovimientoMagnificoSegunJuego :: String -> Double
umbralMovimientoMagnificoSegunJuego juego
  | juego == "3enRaya" = 10.0
  | juego == "gato" = 30.0
  | juego == "damas" = 30.0
  | otherwise = 0.0

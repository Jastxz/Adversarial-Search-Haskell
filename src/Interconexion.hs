module Interconexion
  ( movimientosPosibles,
    esEstadoFinal,
    puntuaEstado,
    siguiente,
    marcaDeLaMaquina,
    umbralSegunJuego,
    umbralMovimientoMagnificoSegunJuego,
  )
where

import Funciones3enRaya
import FuncionesDamas
import FuncionesGato
import Tipos
import Utiles

movimientosPosibles :: Tablero -> String -> String -> Movimientos
movimientosPosibles estado marcaMaquina juego = case juego of
  "3enRaya" -> movs3enRaya estado marcaMaquina
  "gato" -> movsGato estado marcaMaquina
  "damas" -> movsDamas estado marcaMaquina
  _ -> error $ "Error en la función movimientosPosibles, el juego especificado no existe. Juego: " ++ show juego

esEstadoFinal :: Tablero -> String -> Bool
esEstadoFinal t juego = case juego of
  "3enRaya" -> fin3enRaya t
  "gato" -> finGato t
  "damas" -> finDamas t
  _ -> False

puntuaEstado :: Tablero -> Pos -> String -> IO Double
puntuaEstado t pos juego = case juego of
  "3enRaya" -> puntua3enRaya t pos
  "gato" -> puntuaGato t pos
  "damas" -> puntuaDamas t pos
  _ -> return 0.0

siguiente :: Int -> Int
siguiente j = if j == 1 then 2 else 1

marcaDeLaMaquina :: String -> String -> String
marcaDeLaMaquina marca juego = case juego of
  "3enRaya" -> marcaMaquina3enRaya marca
  "gato" -> marcaMaquinaGato marca
  "damas" -> marcaMaquinaDamas marca
  _ -> " "

umbralSegunJuego :: String -> Double
umbralSegunJuego juego = case juego of
  "3enRaya" -> 5.0
  "gato" -> 2.0
  "damas" -> 37.6
  _ -> 0.0

umbralMovimientoMagnificoSegunJuego :: String -> Double
umbralMovimientoMagnificoSegunJuego juego = case juego of
  "3enRaya" -> 10.0
  "gato" -> 30.0
  "damas" -> 1000.0
  _ -> 0.0

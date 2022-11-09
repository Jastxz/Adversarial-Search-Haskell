module Interconexion
  ( movimientosPosibles,
    esEstadoFinal,
    puntuaEstado,
    siguiente,
    marcaDeLaMaquina,
  )
where

import Tipos
import Utiles
import Funciones3enRaya
import FuncionesGato
import FuncionesDamas

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

puntuaEstado :: Tablero -> Pos -> String -> Double
puntuaEstado t pos juego
    | juego == "3enRaya" = puntua3enRaya t pos
    | juego == "gato" = puntuaGato t pos
    | juego == "damas" = puntuaDamas t pos
    | otherwise = 0.0

siguiente :: Int -> Int
siguiente j = if j == 1 then 2 else 1

marcaDeLaMaquina :: String -> String -> String
marcaDeLaMaquina marca juego
    | juego == "3enRaya" = marcaMaquina3enRaya marca
    | juego == "gato" = marcaMaquinaGato marca
    | juego == "damas" = marcaMaquinaDamas marca
    | otherwise = " "
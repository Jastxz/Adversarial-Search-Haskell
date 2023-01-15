module MiniMax
  ( negamax,
  )
where

import Control.Concurrent
import Control.Concurrent.Async
import Data.List
import Interconexion
import System.Directory.Internal.Prelude (on)
import Tipos
import Utiles

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Negamax básico
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
negamax :: Movimiento -> Int -> Int -> String -> String -> IO TableroPuntuado
negamax mov dificultad profundidad marcaMaquina juego
  -- Según la dificultad escogemos el estilo del negamax
  | dificultad == 3 = negamaxConPoda mov profundidad marcaMaquina juego puntuacion (- puntuacion) 2
  | dificultad >= 4 = negamaxCompleto mov profundidad marcaMaquina juego puntuacion (- puntuacion) 2
  | otherwise = do
    iteraNegamax mov profundidad marcaMaquina juego puntuacion 2
  where
    puntuacion = (-1) / 0

iteraNegamax :: Movimiento -> Int -> String -> String -> Double -> Int -> IO TableroPuntuado
iteraNegamax (estado, pos) profundidad marcaMaquina juego referencia quienJuega = do
  let esFinal = esEstadoFinal estado juego
  puntuacion <- puntuaEstado estado pos juego
  --   Obtenemos los movimientos de la máquina o humano del nivel actual
  let movsPosibles = movimientosPosibles estado quienJuega marcaMaquina juego
  if esFinal || profundidad <= 0
    then do
      return (estado, puntuacion)
    else do
      let sigMarca = marcaDeLaMaquina marcaMaquina juego
      iteraciones <- realizaIteraciones movsPosibles (profundidad - 1) sigMarca juego referencia (siguiente quienJuega)
      al <- now
      return $ aleatorio al iteraciones

realizaIteraciones :: Movimientos -> Int -> String -> String -> Double -> Int -> IO TablerosPuntuados
realizaIteraciones [] _ _ _ _ _ = return []
realizaIteraciones ms prof marcaMaquina juego referencia quienJuega = do
  tablerosPuntuados <- mapConcurrently (\m -> iteraNegamax m prof marcaMaquina juego referencia quienJuega) ms
  let tablerosConValor = zipWith (curry (\((e, _), (_, puntuacion)) -> (e, - puntuacion))) ms tablerosPuntuados
  let minimo = snd $ minimumBy (compare `on` snd) tablerosConValor
  return $ filter (\(_, v) -> v == minimo) tablerosConValor

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Negamax con poda
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
negamaxConPoda :: Movimiento -> Int -> String -> String -> Double -> Double -> Int -> IO TableroPuntuado
negamaxConPoda mov@(estado, pos) profundidad marcaMaquina juego alfa beta quienJuega = do
  let esFinal = esEstadoFinal estado juego
  puntuacion <- puntuaEstado estado pos juego
  --   Obtenemos los movimientos de la máquina o humano del nivel actual
  let movsPosibles = movimientosPosibles estado quienJuega marcaMaquina juego
  if esFinal || profundidad <= 0
    then do
      return (estado, puntuacion)
    else do
      let sigMarca = marcaDeLaMaquina marcaMaquina juego
      prob <- time
      evalParc <- evaluaMovsParc movsPosibles profundidad juego
      aleatorios <- escogeAleatorios prob movsPosibles
      let movimientosIterar
            | not (null evalParc) && profundidad == 1 = evalParc
            | not (null aleatorios) = aleatorios
            | otherwise = movsPosibles
      iteraciones <- withAsync (iteraPoda movimientosIterar (profundidad - 1) sigMarca juego alfa beta (siguiente quienJuega)) (
        \iteracionesAsync -> do
        wait iteracionesAsync)
      al <- now
      if puntuacion >= umbralMovimientoMagnificoSegunJuego juego
        then return (fst mov, puntuacion)
        else do
          return $ aleatorio al iteraciones

iteraPoda :: Movimientos -> Int -> String -> String -> Double -> Double -> Int -> IO TablerosPuntuados
iteraPoda [] _ _ _ _ _ _ = return []
iteraPoda (m : ms) prof marcaMaquina juego alfa beta quienJuega = do
  (iteracion, v) <- negamaxConPoda m prof marcaMaquina juego (- beta) (- alfa) quienJuega
  let tabFinal = (fst m, - v)
  -- Minimizamos la puntuación del humano
  let nuevoBeta = min beta (- v)
  if alfa > nuevoBeta
    then do
      return [tabFinal]
    else do
      iteraciones <- iteraPoda ms prof marcaMaquina juego alfa nuevoBeta quienJuega
      let mejorTablero
            | null iteraciones = [tabFinal]
            | (-v) < (snd . cabeza "iteraPoda") iteraciones = [tabFinal]
            | (-v) > (snd . cabeza "iteraPoda") iteraciones = iteraciones
            | otherwise = tabFinal : iteraciones
      return mejorTablero

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Negamax completo
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
negamaxCompleto :: Movimiento -> Int -> String -> String -> Double -> Double -> Int -> IO TableroPuntuado
negamaxCompleto mov@(estado, pos) profundidad marcaMaquina juego alfa beta quienJuega = do
  let esFinal = esEstadoFinal estado juego
  puntuacion <- puntuaEstado estado pos juego
  --   Obtenemos los movimientos de la máquina o humano del nivel actual
  let movsPosibles = movimientosPosibles estado quienJuega marcaMaquina juego
  if esFinal || (profundidad <= 98 && profundidad >= 10)
    then do
      return (estado, puntuacion)
    else do
      if profundidad <= 0
        then do
          reposo <- hayReposo movsPosibles juego
          if reposo
            then do
              return (estado, puntuacion)
            else do
              negamaxCompleto (estado, pos) 100 marcaMaquina juego alfa beta quienJuega
        else do
          let sig = siguiente quienJuega
          let sigMarca = marcaDeLaMaquina marcaMaquina juego
          prob <- time
          evalParc <- evaluaMovsParc movsPosibles profundidad juego
          aleatorios <- escogeAleatorios prob movsPosibles
          let movimientosIterar
                | not (null evalParc) && profundidad == 1 = evalParc
                | not (null aleatorios) = aleatorios
                | otherwise = movsPosibles
          iteraciones <- withAsync (
            iteraCompleto movimientosIterar (profundidad - 1) sigMarca juego alfa beta (siguiente quienJuega)
            ) (
            \iteracionesAsync -> do
            wait iteracionesAsync)
          al <- now
          let umbral = umbralMovimientoMagnificoSegunJuego juego
          if even profundidad && puntuacion >= umbral
            then return (fst mov, puntuacion)
            else do
              let minimo = minimum $ map snd iteraciones
              return $ aleatorio al $ filter (\(_, p) -> p <= minimo) iteraciones

iteraCompleto :: Movimientos -> Int -> String -> String -> Double -> Double -> Int -> IO TablerosPuntuados
iteraCompleto [] _ _ _ _ _ _ = return []
iteraCompleto (m : ms) prof marcaMaquina juego alfa beta quienJuega = do
  (iteracion, v) <- negamaxCompleto m prof marcaMaquina juego (- beta) (- alfa) quienJuega
  let tabFinal = (fst m, - v)
  -- Minimizamos la puntuación del humano
  let nuevoBeta = min beta (- v)
  if alfa > nuevoBeta
    then do
      return [tabFinal]
    else do
      iteraciones <- iteraCompleto ms prof marcaMaquina juego alfa nuevoBeta quienJuega
      let mejorTablero
            | null iteraciones = [tabFinal]
            | (-v) < (snd . cabeza "iteraPoda") iteraciones = [tabFinal]
            | (-v) > (snd . cabeza "iteraPoda") iteraciones = iteraciones
            | otherwise = tabFinal : iteraciones
      return mejorTablero

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Auxiliares
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

evaluaMovsParc :: Movimientos -> Int -> String -> IO Movimientos
evaluaMovsParc [] _ _ = return []
evaluaMovsParc (m : ms) prof juego = do
  let (estado, pos) = m
  alfa <- puntuaEstado estado pos juego
  let umbral = umbralSegunJuego juego
  let margen = margenUtilidadSegunJuego juego
  let alfaConMargen = alfa + margen
  evaluados <- evaluaMovsParc ms prof juego
  if (alfaConMargen <= umbral) && (prof == 1) && not (esEstadoFinal estado juego)
    then return evaluados
    else return $ m : evaluados

hayReposo :: Movimientos -> String -> IO Bool
hayReposo [] _ = return True
hayReposo (m : ms) juego = do
  let (estado, pos) = m
  valor <- puntuaEstado estado pos juego
  let umbral = umbralMovimientoMagnificoSegunJuego juego
  if valor >= umbral
    then return False
    else hayReposo ms juego

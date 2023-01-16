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
  | dificultad == 3 = negamaxConPoda mov profundidad marcaMaquina juego puntuacion (- puntuacion)
  | dificultad >= 4 = negamaxCompleto mov profundidad marcaMaquina juego puntuacion (- puntuacion)
  | otherwise = iteraNegamax mov profundidad marcaMaquina juego
  where
    puntuacion = (-1) / 0

iteraNegamax :: Movimiento -> Int -> String -> String -> IO TableroPuntuado
iteraNegamax (estado, pos) profundidad marcaMaquina juego = do
  let esFinal = esEstadoFinal estado juego
  puntuacion <- puntuaEstado estado pos juego
  --   Obtenemos los movimientos de la máquina o humano del nivel actual
  let movsPosibles = movimientosPosibles estado marcaMaquina juego
  if esFinal || profundidad <= 0
    then do
      return (estado, puntuacion)
    else do
      let sigMarca = marcaDeLaMaquina marcaMaquina juego
      iteraciones <- realizaIteraciones movsPosibles (profundidad - 1) sigMarca juego
      al <- now
      return $ aleatorio al iteraciones

realizaIteraciones :: Movimientos -> Int -> String -> String -> IO TablerosPuntuados
realizaIteraciones ms prof marcaMaquina juego = do
  tablerosPuntuados <- mapConcurrently (\m -> iteraNegamax m prof marcaMaquina juego) ms
  let tablerosConValor = zipWith (curry (\((e, _), (_, puntuacion)) -> (e, - puntuacion))) ms tablerosPuntuados
  let minimo = snd $ minimumBy (compare `on` snd) tablerosConValor
  return $ filter (\(_, v) -> v == minimo) tablerosConValor

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Negamax con poda
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
negamaxConPoda :: Movimiento -> Int -> String -> String -> Double -> Double -> IO TableroPuntuado
negamaxConPoda (estado, pos) profundidad marcaMaquina juego alfa beta = do
  let esFinal = esEstadoFinal estado juego
  puntuacion <- puntuaEstado estado pos juego
  --   Obtenemos los movimientos de la máquina o humano del nivel actual
  let movsPosibles = movimientosPosibles estado marcaMaquina juego
  if esFinal || profundidad <= 0
    then do
      return (estado, puntuacion)
    else do
      let sigMarca = marcaDeLaMaquina marcaMaquina juego
      prob <- time
      evalParc <- evaluaMovsParc movsPosibles profundidad juego
      aleatorios <- escogeAleatorios prob movsPosibles
      let movimientosIterar
            | profundidad == 1 && not (null evalParc) = evalParc
            | not (null aleatorios) = aleatorios
            | otherwise = movsPosibles
      iteraciones <- withAsync (iteraPoda movimientosIterar (profundidad - 1) sigMarca juego alfa beta) (
        \iteracionesAsync -> do
        wait iteracionesAsync)
      if puntuacion >= umbralMovimientoMagnificoSegunJuego juego
        then return (estado, puntuacion)
        else do
          al <- now
          return $ aleatorio al iteraciones

iteraPoda :: Movimientos -> Int -> String -> String -> Double -> Double -> IO TablerosPuntuados
iteraPoda [] _ _ _ _ _ = return []
iteraPoda (m : ms) prof marcaMaquina juego alfa beta = do
  (iteracion, v) <- negamaxConPoda m prof marcaMaquina juego (- beta) (- alfa)
  let tabFinal = (fst m, - v)
  -- Minimizamos la puntuación del humano
  let nuevoBeta = min beta (- v)
  if alfa > nuevoBeta
    then do
      return [tabFinal]
    else do
      iteraciones <- iteraPoda ms prof marcaMaquina juego alfa nuevoBeta
      let mejorTablero
            | null iteraciones = [tabFinal]
            | (-v) < (snd . cabeza "iteraPoda") iteraciones = [tabFinal]
            | (-v) > (snd . cabeza "iteraPoda") iteraciones = iteraciones
            | otherwise = tabFinal : iteraciones
      return mejorTablero

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Negamax completo
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
negamaxCompleto :: Movimiento -> Int -> String -> String -> Double -> Double -> IO TableroPuntuado
negamaxCompleto (estado, pos) profundidad marcaMaquina juego alfa beta = do
  let esFinal = esEstadoFinal estado juego
  puntuacion <- puntuaEstado estado pos juego
  --   Obtenemos los movimientos de la máquina o humano del nivel actual
  let movsPosibles = movimientosPosibles estado marcaMaquina juego
  if esFinal || profundidad < (-1)
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
              actNegmxCompl (estado, pos) movsPosibles puntuacion profundidad marcaMaquina juego alfa beta
        else do
          actNegmxCompl (estado, pos) movsPosibles puntuacion profundidad marcaMaquina juego alfa beta

actNegmxCompl :: Movimiento -> Movimientos -> Double -> Int -> String -> String -> Double -> Double -> IO TableroPuntuado
actNegmxCompl (estado, pos) movsPosibles puntuacion profundidad marcaMaquina juego alfa beta = do
  let sigMarca = marcaDeLaMaquina marcaMaquina juego
  prob <- time
  evalParc <- evaluaMovsParc movsPosibles profundidad juego
  aleatorios <- escogeAleatorios prob movsPosibles
  let movimientosIterar
        | profundidad == 1 && not (null evalParc) = evalParc
        | not (null aleatorios) = aleatorios
        | otherwise = movsPosibles
  iteraciones <- withAsync (
    iteraCompleto movimientosIterar (profundidad - 1) sigMarca juego alfa beta
    ) (
    \iteracionesAsync -> do
    wait iteracionesAsync)
  if puntuacion >= umbralMovimientoMagnificoSegunJuego juego
    then return (estado, puntuacion)
    else do
      al <- now
      return $ aleatorio al iteraciones

iteraCompleto :: Movimientos -> Int -> String -> String -> Double -> Double -> IO TablerosPuntuados
iteraCompleto [] _ _ _ _ _ = return []
iteraCompleto (m : ms) prof marcaMaquina juego alfa beta = do
  (iteracion, v) <- negamaxCompleto m prof marcaMaquina juego (- beta) (- alfa)
  let tabFinal = (fst m, - v)
  -- Minimizamos la puntuación del humano
  let nuevoBeta = min beta (- v)
  if alfa > nuevoBeta
    then do
      return [tabFinal]
    else do
      iteraciones <- iteraCompleto ms prof marcaMaquina juego alfa nuevoBeta
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
  evaluados <- evaluaMovsParc ms prof juego
  if (alfa <= umbral) && (prof == 1) && not (esEstadoFinal estado juego)
    then return evaluados
    else return $ m : evaluados

hayReposo :: Movimientos -> String -> IO Bool
hayReposo [] _ = return True
hayReposo (m : ms) juego = do
  let (estado, pos) = m
  valor <- puntuaEstado estado pos juego
  let umbral = umbralMovimientoMagnificoSegunJuego juego
  if valor < umbral
    then hayReposo ms juego
    else return False

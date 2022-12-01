module MiniMax
  ( negamax,
  )
where

import Data.List
import Interconexion
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
      let sig = siguiente quienJuega
      let sigMarca = marcaDeLaMaquina marcaMaquina juego
      iteraciones <- realizaIteraciones movsPosibles (profundidad - 1) sigMarca juego referencia sig
      al <- now
      let mejor = aleatorio al $ minimoSegundo iteraciones
      return mejor

realizaIteraciones :: Movimientos -> Int -> String -> String -> Double -> Int -> IO TablerosPuntuados
realizaIteraciones [] _ _ _ _ _ = return []
realizaIteraciones (m : ms) prof marcaMaquina juego referencia quienJuega = do
  tableroPuntuado <- iteraNegamax m prof marcaMaquina juego referencia quienJuega
  let (iteracion, v) = tableroPuntuado
  let valor = - v
  let tabFinal = (fst m, valor)
  let mejorReferencia = max referencia valor
  iteraciones <- realizaIteraciones ms prof marcaMaquina juego mejorReferencia quienJuega
  al <- now
  let tabIteraciones
        | null iteraciones = tabFinal
        | otherwise = aleatorio al iteraciones
  -- Minimizamos la puntuación del humano
  let mejorTablero
        | tabFinal == tabIteraciones = [tabFinal]
        | otherwise = tabFinal : iteraciones
  return mejorTablero

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
      let sig = siguiente quienJuega
      let sigMarca = marcaDeLaMaquina marcaMaquina juego
      prob <- time
      evaluadosParcialmente <- evaluaMovimientosParcialmente movsPosibles profundidad juego
      aleatorios <- escogeAleatorios prob movsPosibles
      let movimientosIterar
            | not (null evaluadosParcialmente) && profundidad == 1 = evaluadosParcialmente
            | not (null aleatorios) = aleatorios
            | otherwise = movsPosibles
      iteraciones <- iteraPoda movimientosIterar (profundidad - 1) sigMarca juego alfa beta sig
      al <- now
      let umbral = umbralMovimientoMagnificoSegunJuego juego
      if even profundidad && puntuacion >= umbral
        then return (fst mov, puntuacion)
        else do
          let mejor = aleatorio al $ minimoSegundo iteraciones
          return mejor

iteraPoda :: Movimientos -> Int -> String -> String -> Double -> Double -> Int -> IO TablerosPuntuados
iteraPoda [] _ _ _ _ _ _ = return []
iteraPoda (m : ms) prof marcaMaquina juego alfa beta quienJuega = do
  tableroPuntuado <- negamaxConPoda m prof marcaMaquina juego (- beta) (- alfa) quienJuega
  let (iteracion, v) = tableroPuntuado
  let valor = - v
  let tabFinal = (fst m, valor)
  -- Minimizamos la puntuación del humano
  let nuevoBeta = min beta valor
  if alfa > nuevoBeta
    then do
      return [tabFinal]
    else do
      iteraciones <- iteraPoda ms prof marcaMaquina juego alfa nuevoBeta quienJuega
      al <- now
      let tabIteraciones
            | null iteraciones = tabFinal
            | otherwise = aleatorio al iteraciones
      let mejorTablero
            | tabFinal == tabIteraciones = [tabFinal]
            | otherwise = tabFinal : iteraciones
      return mejorTablero

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Negamax completo
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
negamaxCompleto :: Movimiento -> Int -> String -> String -> Double -> Double -> Int -> IO TableroPuntuado
negamaxCompleto (estado, pos) profundidad marcaMaquina juego alfa beta quienJuega = do
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
          evaluadosParcialmente <- evaluaMovimientosParcialmente movsPosibles profundidad juego
          aleatorios <- escogeAleatorios prob movsPosibles
          let movimientosIterar
                | not (null evaluadosParcialmente) && profundidad == 1 = evaluadosParcialmente
                | not (null aleatorios) = aleatorios
                | otherwise = movsPosibles
          iteraciones <- iteraCompleto movimientosIterar (profundidad - 1) sigMarca juego alfa beta sig
          al <- now
          let mejor = aleatorio al $ minimoSegundo iteraciones
          if profundidad == 1000
            then do
              print mejor
              return mejor
            else return mejor

iteraCompleto :: Movimientos -> Int -> String -> String -> Double -> Double -> Int -> IO TablerosPuntuados
iteraCompleto [] _ _ _ _ _ _ = return []
iteraCompleto (m : ms) prof marcaMaquina juego alfa beta quienJuega = do
  tableroPuntuado <- negamaxCompleto m prof marcaMaquina juego (- beta) (- alfa) quienJuega
  let (iteracion, v) = tableroPuntuado
  let valor = - v
  let tabFinal = (fst m, valor)
  -- Minimizamos la puntuación del humano
  let nuevoBeta = min beta valor
  if alfa > nuevoBeta
    then do
      return [tabFinal]
    else do
      iteraciones <- iteraCompleto ms prof marcaMaquina juego alfa nuevoBeta quienJuega
      al <- now
      let tabIteraciones
            | null iteraciones = tabFinal
            | otherwise = aleatorio al iteraciones
      let mejorTablero
            | tabFinal == tabIteraciones = [tabFinal]
            | otherwise = tabFinal : iteraciones
      return mejorTablero

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Auxiliares
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

evaluaMovimientosParcialmente :: Movimientos -> Int -> String -> IO Movimientos
evaluaMovimientosParcialmente [] _ _ = return []
evaluaMovimientosParcialmente (m : ms) prof juego = do
  let (estado, pos) = m
  alfa <- puntuaEstado estado pos juego
  let umbral = umbralSegunJuego juego
  let margen = margenUtilidadSegunJuego juego
  let alfaConMargen = alfa + margen
  evaluados <- evaluaMovimientosParcialmente ms prof juego
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

minimoSegundo :: TablerosPuntuados -> TablerosPuntuados
minimoSegundo [] = []
minimoSegundo (x:xs)
    | null xs = [x]
    | num < ma = [x]
    | num == ma = x : resto
    | otherwise = resto
    where
      (m,num) = x
      resto = minimoSegundo xs
      (a,ma) = cabeza "minimoSegundo" resto

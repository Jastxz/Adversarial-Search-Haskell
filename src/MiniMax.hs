module MiniMax
  ( negamax,
  )
where

import Data.List

import Tipos
import Utiles

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Negamax básico
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
negamax :: Movimiento -> Int -> Int -> String -> String -> IO TableroPuntuado
negamax mov dificultad profundidad marcaMaquina juego
  -- Según la dificultad escogemos el estilo del negamax
  | dificultad == 2 = negamaxConPoda mov profundidad marcaMaquina juego puntuacion (1 / 0) 2
  | dificultad >= 3 = negamaxCompleto mov profundidad marcaMaquina juego puntuacion (1 / 0) 2
  | otherwise = do
    iteraNegamax mov profundidad marcaMaquina juego puntuacion 2
  where
    puntuacion = (-1) / 0

iteraNegamax :: Movimiento -> Int -> String -> String -> Double -> Int -> IO TableroPuntuado
iteraNegamax (estado, pos) profundidad marcaMaquina juego referencia quienJuega = do
  let esFinal = esEstadoFinal estado juego
  let puntuacion = puntuaEstado estado pos juego
  --   Obtenemos los movimientos de la máquina o humano del nivel actual
  let movsPosibles = movimientosPosibles estado quienJuega marcaMaquina juego
  if esFinal || profundidad <= 0
    then do
      return (estado, puntuacion)
    else do
      let sig = siguiente quienJuega
      let sigMarca = marcaDeLaMaquina marcaMaquina juego
      iteraciones <- realizaIteraciones movsPosibles (profundidad - 1) sigMarca juego referencia sig
      let mejor = head iteraciones
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
  let tabIteraciones
        | null iteraciones = tabFinal
        | otherwise = head iteraciones
  -- Minimizamos la puntuación del humano
  let mejorTablero
        | snd tabIteraciones < valor = tabIteraciones
        | otherwise = tabFinal
  -- Bloque if para realizar seguimientos y pruebas
  {- if prof >= 5
      then do
          putStrLn "================================================================"
          putStrLn "Iteración de realizaIteraciones"
          putStrLn "================================================================"
          putStrLn "Movimiento y su tablero puntuado"
          print m
          putStrLn " "
          print tableroPuntuado
          putStrLn " "
          putStr "Profundidad, marca del turno, mejor referencia actual, v y valor:    "
          putStrLn $ show prof ++ "..." ++ show marcaMaquina ++ "..." ++ show mejorReferencia ++ "..." ++ show valor
          putStrLn " "
          putStrLn "================================================================"
          return [mejorTablero]
      else return [mejorTablero] -}
  return [mejorTablero]

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Negamax con poda
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
negamaxConPoda :: Movimiento -> Int -> String -> String -> Double -> Double -> Int -> IO TableroPuntuado
negamaxConPoda (estado, pos) profundidad marcaMaquina juego alfa beta quienJuega = do
  let esFinal = esEstadoFinal estado juego
  let puntuacion = puntuaEstado estado pos juego
  --   Obtenemos los movimientos de la máquina o humano del nivel actual
  let movsPosibles = movimientosPosibles estado quienJuega marcaMaquina juego
  if esFinal || profundidad <= 0
    then do
      return (estado, alfa)
    else do
      let sig = siguiente quienJuega
      let sigMarca = marcaDeLaMaquina marcaMaquina juego
      evaluadosParcialmente <- evaluaMovimientosParcialmente movsPosibles profundidad sig sigMarca juego
      let movimientosIterar
            | null evaluadosParcialmente = movsPosibles
            | otherwise = evaluadosParcialmente
      iteraciones <- iteraPoda movimientosIterar (profundidad - 1) sigMarca juego alfa beta sig
      let mejor = head iteraciones
      return mejor

iteraPoda :: Movimientos -> Int -> String -> String -> Double -> Double -> Int -> IO TablerosPuntuados
iteraPoda [] _ _ _ _ _ _ = return []
iteraPoda (m : ms) prof marcaMaquina juego alfa beta quienJuega = do
  tableroPuntuado <- negamaxConPoda m prof marcaMaquina juego (- beta) (- alfa) quienJuega
  let (iteracion, v) = tableroPuntuado
  let valor = - v
  let tabFinal = (fst m, valor)
  let nuevoAlfa = max alfa valor
  if nuevoAlfa >= beta
    then return [tabFinal]
    else do
      iteraciones <- iteraPoda ms prof marcaMaquina juego nuevoAlfa beta quienJuega
      let tabIteraciones
            | null iteraciones = tabFinal
            | otherwise = head iteraciones
      -- Maximizamos la puntuación de la máquina
      let mejorTablero
            | snd tabIteraciones >= nuevoAlfa = tabIteraciones
            | otherwise = tabFinal
      -- Bloque if para realizar seguimientos y pruebas
{- if prof >= 5
  then do
    putStrLn "================================================================"
    putStrLn "Iteración de iteraPoda"
    putStrLn "================================================================"
    putStrLn "Movimiento y su tablero puntuado"
    print m
    putStrLn " "
    print tableroPuntuado
    putStrLn " "
    putStr "Profundidad, marca del turno, mejor referencia actual, v y valor:    "
    putStrLn $ show prof ++ "..." ++ show marcaMaquina ++ "..." ++ show mejorReferencia ++ "..." ++ show valor
    putStrLn " "
    putStrLn "================================================================"
    return [mejorTablero]
  else return [mejorTablero] -}
      return [mejorTablero]

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Negamax completo
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
negamaxCompleto :: Movimiento -> Int -> String -> String -> Double -> Double -> Int -> IO TableroPuntuado
negamaxCompleto = undefined

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Auxiliares
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
umbralSegunJuego :: String -> Double
umbralSegunJuego juego
  | juego == "3enRaya" = 10.0
  | juego == "gato" = 10.0
  | otherwise = 0.0

margenUtilidadSegunJuego :: String -> Double
margenUtilidadSegunJuego juego
  | juego == "3enRaya" = 3.0
  | juego == "gato" = 1.0
  | otherwise = 0.0

evaluaMovimientosParcialmente :: Movimientos -> Int -> Int -> String -> String -> IO Movimientos
evaluaMovimientosParcialmente [] _ _ _ _ = return []
evaluaMovimientosParcialmente (m : ms) prof quienJuega marcaMaquina juego = do
  let (estado, pos) = m
  let alfa = puntuaEstado estado pos juego
  let umbral = umbralSegunJuego juego
  let margen = margenUtilidadSegunJuego juego
  let alfaConMargen = alfa + margen
  let movsPosibles = movimientosPosibles estado quienJuega marcaMaquina juego
  let puntuados = map (\(e, p) -> puntuaEstado e p juego) movsPosibles
  {- putStrLn ""
  putStrLn $ "Movimiento original: " ++ show m
  putStrLn "Movimientos generados con sus puntuaciones: "
  print $ zip movsPosibles puntuados
  putStrLn "" -}
  let sonFinales = filter (\p -> (p + margen) >= umbral) puntuados
  evaluados <- evaluaMovimientosParcialmente ms prof quienJuega marcaMaquina juego
  if (alfaConMargen < umbral) && (prof > 1) && null sonFinales
    then return evaluados
    else return $ m : evaluados
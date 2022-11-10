module MiniMax
  ( negamax,
  )
where

import Data.List

import Tipos
import Utiles
import Interconexion

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Negamax básico
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
negamax :: Movimiento -> Int -> Int -> String -> String -> IO TableroPuntuado
negamax mov dificultad profundidad marcaMaquina juego
  -- Según la dificultad escogemos el estilo del negamax
  | dificultad == 3 = negamaxConPoda mov profundidad marcaMaquina juego puntuacion (-puntuacion) 2
  | dificultad >= 4 = negamaxCompleto mov profundidad marcaMaquina juego puntuacion (-puntuacion) 2
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
      return (estado, puntuacion)
    else do
      let sig = siguiente quienJuega
      let sigMarca = marcaDeLaMaquina marcaMaquina juego
      evaluadosParcialmente <- evaluaMovimientosParcialmente movsPosibles profundidad sig sigMarca juego
      let movimientosIterar
            | not (null evaluadosParcialmente) && profundidad == 1 = evaluadosParcialmente
            | otherwise = movsPosibles
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
  let nuevoBeta = min beta valor
  if alfa > nuevoBeta
    then do
      {- putStrLn "================================================================"
      putStrLn "Iteración de iteraPoda"
      putStrLn "================================================================"
      putStrLn "Alfa y nuevo beta"
      putStrLn $ show alfa ++ "..." ++ show nuevoBeta
      putStrLn "================================================================"
      print "Tableros"
      print m
      print tableroPuntuado -}
      return [tabFinal]
    else do
      iteraciones <- iteraPoda ms prof marcaMaquina juego alfa nuevoBeta quienJuega
      let tabIteraciones
            | null iteraciones = tabFinal
            | otherwise = head iteraciones
      -- Minimizamos la puntuación del humano
      let mejorTablero
            | snd tabIteraciones <= nuevoBeta = tabIteraciones
            | otherwise = tabFinal
  -- Bloque if para realizar seguimientos y pruebas
  {- iteraciones <- iteraPoda ms prof marcaMaquina juego alfa nuevoBeta quienJuega
  let tabIteraciones
        | null iteraciones = tabFinal
        | otherwise = head iteraciones
  -- Minimizamos la puntuación del humano
  let mejorTablero
        | snd tabIteraciones <= nuevoBeta = tabIteraciones
        | otherwise = tabFinal -}
  {- if prof >= 3
    then do
      putStrLn "================================================================"
      putStrLn "Iteración de iteraPoda"
      putStrLn "================================================================"
      putStrLn "Movimiento y su tablero puntuado"
      print m
      putStrLn " "
      print tableroPuntuado
      putStrLn " "
      putStrLn "Alfa, Beta, Nuevo beta y mejor valor del resto"
      putStrLn $ show alfa ++ "..." ++ show beta ++ "..." ++ show nuevoBeta ++ "..." ++ show (snd tabIteraciones)
      putStrLn " "
      putStr "Profundidad, marca del turno, v y valor:    "
      putStrLn $ show prof ++ "..." ++ show marcaMaquina ++ "..." ++ show v ++ "..." ++ show valor
      putStrLn " "
      putStrLn "================================================================"
      return [mejorTablero]
    else return [mejorTablero] -}
      return [mejorTablero]
  -- return [mejorTablero]

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Negamax completo
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
negamaxCompleto :: Movimiento -> Int -> String -> String -> Double -> Double -> Int -> IO TableroPuntuado
negamaxCompleto = undefined

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Auxiliares
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

evaluaMovimientosParcialmente :: Movimientos -> Int -> Int -> String -> String -> IO Movimientos
evaluaMovimientosParcialmente [] _ _ _ _ = return []
evaluaMovimientosParcialmente (m : ms) prof quienJuega marcaMaquina juego = do
  let (estado, pos) = m
  let alfa = puntuaEstado estado pos juego
  let umbral = umbralSegunJuego juego
  let margen = margenUtilidadSegunJuego juego
  let alfaConMargen = alfa + margen
  {- putStrLn ""
  putStrLn $ "Movimiento original: " ++ show m
  putStrLn "Movimientos generados con sus puntuaciones: "
  print $ zip movsPosibles puntuados
  putStrLn "" -}
  evaluados <- evaluaMovimientosParcialmente ms prof quienJuega marcaMaquina juego
  if (alfaConMargen <= umbral) && (prof == 1) && not (esEstadoFinal estado juego)
    then return evaluados
    else return $ m : evaluados
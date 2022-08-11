module MiniMax (
    negamax
) where

import Tipos
import Utiles
import Data.List

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Negamax básico
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
negamax :: Movimiento -> Int -> Int -> String -> String -> IO TableroPuntuado
negamax mov dificultad profundidad marcaMaquina juego
    | dificultad == 2 = negamaxConPoda mov profundidad marcaMaquina juego puntuacion (1/0)
    | dificultad >= 3 = negamaxCompleto mov profundidad marcaMaquina juego puntuacion (1/0)
    | otherwise = do
        iteraNegamax mov profundidad marcaMaquina juego puntuacion 2
        where
            puntuacion = (-1)/0

iteraNegamax :: Movimiento -> Int -> String -> String -> Double -> Int -> IO TableroPuntuado
iteraNegamax (estado, pos) profundidad marcaMaquina juego referencia quienJuega = do
    let esFinal = esEstadoFinal estado juego
    let puntuacion = puntuaEstado estado pos juego
    let movsPosibles = movimientosPosibles estado quienJuega marcaMaquina juego
    let mejorReferencia | referencia > puntuacion = referencia | otherwise = puntuacion
    if esFinal
        then
            return (estado,mejorReferencia)
        else do
            if profundidad <= 0
                then do
                    return (estado,mejorReferencia)
                else do
                    let sig = siguiente quienJuega
                    iteraciones <- realizaIteraciones movsPosibles (profundidad - 1) marcaMaquina juego mejorReferencia sig
                    let puntuaciones = map snd iteraciones
                    let movs = map fst movsPosibles
                    let tablerosPuntuados = zip movs puntuaciones
                    let mejor = head $ sortOn snd tablerosPuntuados
                    {- if profundidad >=7
                        then do
                            putStrLn "================================================================"
                            putStrLn "Bloque"
                            putStrLn "================================================================"
                            putStrLn "Movimientos posibles con sus puntuaciones"
                            print tablerosPuntuados
                            putStr "Quien juega en este turno, la mejor puntuación y la profundidad:    "
                            putStrLn $ show quienJuega ++ "......" ++ show mejor ++ "......" ++ show profundidad
                            putStrLn "================================================================"
                            return mejor
                        else return mejor -}
                    return mejor

realizaIteraciones :: Movimientos -> Int -> String -> String -> Double -> Int -> IO TablerosPuntuados
realizaIteraciones [] _ _ _ _ _ = return []
realizaIteraciones (m:ms) prof marcaMaquina juego referencia quienJuega = do
    tableroPuntuado <- iteraNegamax m prof marcaMaquina juego referencia quienJuega
    let (iteracion,v) = tableroPuntuado
    let valor = (iteracion,(-1)*v)
    {- putStr $ show iteracion
    putStr "..."
    putStr $ show valor
    putStrLn " " -}
    iteraciones <- realizaIteraciones ms prof marcaMaquina juego referencia quienJuega
    return $ valor:iteraciones

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Negamax con poda
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
negamaxConPoda :: Movimiento -> Int -> String -> String -> Double -> Double -> IO TableroPuntuado
negamaxConPoda = undefined


{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Negamax completo
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
negamaxCompleto :: Movimiento -> Int -> String -> String -> Double -> Double -> IO TableroPuntuado
negamaxCompleto = undefined
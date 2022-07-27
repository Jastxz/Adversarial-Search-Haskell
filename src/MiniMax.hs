module MiniMax (
    negamax
) where

import Tipos
import Utiles
import Data.List (sortOn)

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Negamax básico
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
negamax :: Movimiento -> Int -> Int -> String -> IO TableroPuntuado
negamax mov dificultad profundidad juego
    | dificultad == 2 = negamaxConPoda mov profundidad juego puntuacion (1/0)
    | dificultad >= 3 = negamaxCompleto mov profundidad juego puntuacion (1/0)
    | otherwise = do
        iteraNegamax mov profundidad juego puntuacion 2
        where
            puntuacion = (-1)/0

iteraNegamax :: Movimiento -> Int -> String -> Double -> Int -> IO TableroPuntuado
iteraNegamax (estado, pos) profundidad juego referencia quienJuega = do
    let esFinal = esEstadoFinal estado juego
    let puntuacion = puntuaEstado estado pos juego
    let movsPosibles = movimientosPosibles estado juego quienJuega
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
                    iteraciones <- realizaIteraciones movsPosibles (profundidad - 1) juego mejorReferencia sig
                    let puntuaciones = map snd iteraciones
                    let movs = map fst movsPosibles
                    let tablerosPuntuados = zip movs puntuaciones
                    let mejor = head $ sortOn snd tablerosPuntuados
                    {- putStrLn "================================================================"
                    putStrLn "Bloque"
                    putStrLn "================================================================"
                    putStrLn "Movimientos posibles con sus puntuaciones"
                    print tablerosPuntuados
                    putStr "Quien juega en este turno, la mejor puntuación y la profundidad:    "
                    putStrLn $ show quienJuega ++ "......" ++ show mejor ++ "......" ++ show profundidad
                    putStrLn "================================================================" -}
                    return mejor

realizaIteraciones :: Movimientos -> Int -> String -> Double -> Int -> IO TablerosPuntuados
realizaIteraciones [] _ _ _ _ = return []
realizaIteraciones (m:ms) prof juego referencia quienJuega = do
    (iteracion,v) <- iteraNegamax m prof juego referencia quienJuega
    let valor = (iteracion,(-1)*v)
    {- putStr $ show iteracion
    putStr "..."
    putStr $ show valor
    putStrLn " " -}
    iteraciones <- realizaIteraciones ms prof juego referencia quienJuega
    return $ valor:iteraciones

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Negamax con poda
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
negamaxConPoda :: Movimiento -> Int -> String -> Double -> Double -> IO TableroPuntuado
negamaxConPoda = undefined


{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Negamax completo
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
negamaxCompleto :: Movimiento -> Int -> String -> Double -> Double -> IO TableroPuntuado
negamaxCompleto = undefined
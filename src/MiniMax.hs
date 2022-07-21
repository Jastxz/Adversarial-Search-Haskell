module MiniMax (
    negamax
) where

import Tipos
import Utiles

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Negamax básico
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
negamax :: Tablero -> Int -> Int -> String -> IO Double
negamax estado dificultad profundidad juego
    | dificultad == 2 = negamaxConPoda estado profundidad juego puntuacion (1/0)
    | dificultad >= 3 = negamaxCompleto estado profundidad juego puntuacion (1/0)
    | otherwise = do
        iteraNegamax estado profundidad juego puntuacion 1
        where
            puntuacion = (-1)/0

iteraNegamax :: Tablero -> Int -> String -> Double -> Int -> IO Double
iteraNegamax estado profundidad juego referencia quienJuega = do
    let esFinal = esEstadoFinal estado juego
    let puntuacion = puntuaEstado estado juego
    let movsPosibles = movimientosPosibles estado juego quienJuega
    let mejorReferencia | referencia > puntuacion = referencia | otherwise = puntuacion
    if esFinal
        then
            return mejorReferencia
        else do
            if profundidad <= 0
                then do
                    return mejorReferencia
                else do
                    let sig = siguiente quienJuega
                    iteraciones <- realizaIteraciones movsPosibles (profundidad - 1) juego puntuacion sig
                    putStrLn "Movimientos posibles"
                    print movsPosibles
                    putStrLn "Resultados iteraciones"
                    print iteraciones
                    let mejor = maximum iteraciones
                    return mejor

realizaIteraciones :: Movimientos -> Int -> String -> Double -> Int -> IO [Double]
realizaIteraciones [] _ _ _ _ = return []
realizaIteraciones (m:ms) prof juego referencia quienJuega = do
    iteracion <- iteraNegamax m prof juego referencia quienJuega
    let valor = (-1)*iteracion
    iteraciones <- realizaIteraciones ms prof juego referencia quienJuega
    return $ valor:iteraciones

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Negamax con poda
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
negamaxConPoda :: Tablero -> Int -> String -> Double -> Double -> IO Double
negamaxConPoda = undefined


{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Negamax completo
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
negamaxCompleto :: Tablero -> Int -> String -> Double -> Double -> IO Double
negamaxCompleto = undefined
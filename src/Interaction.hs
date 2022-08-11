module Interaction (
        mainInteraction
    ) where

import Utiles
import IO3enRaya
import IOgato

mainInteraction :: IO ()
mainInteraction = do
    putStrLn "Escoja el juego al que desea jugar escribiendo el número correspondiente."
    putStrLn "0 para el 3 en raya. 1 para el gato y el ratón."
    juego <- leeDigito "Introduzca el número: "
    juegoAlanzar juego

juegoAlanzar :: Int -> IO()
juegoAlanzar juego 
    | juego == 0 = interactua3enRaya
    | otherwise = interactuaGato
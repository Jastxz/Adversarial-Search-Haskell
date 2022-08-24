module FuncionesGato (
        -- Funciones normales
        posicionesInicialesGatos,
        falsoInicial,
        inicial,
        casillasVaciasRaton,
        ratonEncerrado,
        ratonEscapado,
        movsGato,
        intercambiaPieza
    ) where

import Data.Matrix
import Data.List (nub)
import Tipos
import Utiles

posicionesInicialesGatos = [(8,1),(8,3),(8,5),(8,7)]

falsoInicial :: Tablero
falsoInicial =  matrix 8 8 $ \(i,j) -> añadePiezas (i,j)

inicial :: Pos -> Movimiento
inicial pos = (t,pos)
    where
        t = setElem "R" pos falsoInicial

añadePiezas :: Pos -> String
añadePiezas actual
    | actual `elem` posicionesInicialesGatos = "G"
    | odd suma = " "
    | otherwise = "X"
        where
            suma = uncurry (+) actual

casillasVaciasRaton :: Tablero -> [Pos]
casillasVaciasRaton m = filter (\ c -> (m ! c) == " ") casillasAlrededor
    where
        (min,max) = rangos m
        casillas = [(i,j) | i<-[min..max], j<-[min..max]]
        (f,c) = head $ filter (\pos -> (m ! pos) == "R") casillas
        casillasAlrededor = casillasAlrededorFicha m (f,c)

casillasVaciasGatos :: Tablero -> [Pos]
casillasVaciasGatos m = filter (\ c -> (m ! c) == " ") casillasAlrededor
    where
        (min,max) = rangos m
        casillas = [(i,j) | i<-[min..max], j<-[min..max]]
        casillasGatos = filter (\pos -> (m ! pos) == "G") casillas
        casillasAlrededor = concatMap (casillasValidasGatos m) casillasGatos

casillasValidasGatos :: Tablero -> Pos -> [Pos]
casillasValidasGatos m pos@(f,c) =filter (\(i,j) -> i < f) casillasAlrededor
    where
        casillasAlrededor = casillasAlrededorFicha m pos

ratonEncerrado :: Tablero -> Bool
ratonEncerrado t = null (casillasVaciasRaton t)

ratonEscapado :: Tablero -> Bool
ratonEscapado t = filaRaton >= filaGato
    where
        (rmen, rmay) = rangos t
        posiciones = [(i,j) | i<-[rmen..rmay], j<-[rmen..rmay]]
        filaRaton = fst $ head $ filter (\p -> (t ! p) == "R") posiciones
        filaGato = maximum $ map fst $ filter (\p -> (t ! p) == "G") posiciones

movsGato :: Tablero -> String -> Movimientos
movsGato t marca
    | marca == "R" = nub $ map (mueveRaton t) (casillasVaciasRaton t)
    | otherwise = nub $ mueveGato t (casillasVaciasGatos t)

mueveRaton :: Tablero -> Pos -> Movimiento
mueveRaton t pos
    | length estaAlrededor == 1 = (intercambiaPieza t "R" pos (head estaAlrededor), pos)
    | otherwise = (t,pos)
        where
            validas = casillasAlrededorFicha t pos
            estaAlrededor = filter (\p -> (t ! p) == "R") validas

mueveGato :: Tablero -> [Pos] -> Movimientos
mueveGato _ [] = []
mueveGato t (p:ps)
    | length gatosAlrededor == 1 = (intercambiaPieza t "G" p (head gatosAlrededor), p) : mueveGato t ps
    | length gatosAlrededor > 1 = intercambia2piezas t gatosAlrededor p ++ mueveGato t ps
    | otherwise = (t,p) : mueveGato t ps
        where
            validas = casillasAlrededorFicha t p
            gatosAlrededor = filter (\v -> (t ! v) == "G") validas

intercambiaPieza :: Tablero -> String -> Pos -> Pos -> Tablero
intercambiaPieza t pieza posNueva posAntigua = setElem pieza posNueva $ setElem " " posAntigua t

intercambia2piezas :: Tablero -> [Pos] -> Pos -> Movimientos
intercambia2piezas t gatosAlrededor pos = [(intercambiaPieza t "G" pos g, pos) | g<-gatosAlrededor]

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones que tienen que ver con la escritura y lectura en consola.
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}


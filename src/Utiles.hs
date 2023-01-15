module Utiles
  ( -- Funciones Matrix
    rangos,
    numeroElementosMatriz,
    columnasMatriz,
    diagonalesMatriz,
    tableroVacio,
    valido,
    dentroDelTablero,
    casillaVacia,
    casillasVacias,
    casillasAlrededorFicha,
    fichasAlrededorCasilla,
    casillasSegundoNivel,
    buscaPieza,
    intercambiaPieza,
    eliminaPieza,
    -- Funciones Normales
    cabeza,
    esInt,
    esReal,
    stringToInt,
    stringToDouble,
    stringToFloat,
    listasDePares,
    distanciaEuclidea,
    intercambia,
    introduce,
    elimina,
    eliminaElemento,
    aleatorio,
    escogeAleatorios,
    normaliza,
    -- Funciones IO
    nuevaLinea,
    now,
    time,
  )
where

import Data.Maybe
import Data.Char
import qualified Data.Functor
import Data.List (nub, elemIndex)
import Data.Matrix
import Data.Time.Clock
import System.Random
import Tipos

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones de Matrix en útiles.
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

rangos :: Matrix a -> (Int, Int)
rangos m = (1, nrows m)

numeroElementosMatriz :: Matrix a -> Int
numeroElementosMatriz m = nrows m * ncols m

columnasMatriz :: Matrix a -> [[a]]
columnasMatriz m = toLists $ transpose m

diagonalesMatriz :: Matrix a -> [[a]]
diagonalesMatriz m = [map (\x -> m ! (x,x)) [1..n], map (\x -> m ! (x, n - x + 1)) [1..n]]
  where
    n = nrows m

tableroVacio :: String -> Movimiento
tableroVacio nombre = (tab, pos)
  where
    tab = matrix 1 1 $ \(i, j) -> nombre
    pos = (1, 1)

valido :: Pos -> Tablero -> Bool
valido pos m = (v == " ") && dentroDelTablero pos m
  where
    v = m ! pos

dentroDelTablero :: Pos -> Tablero -> Bool
dentroDelTablero (i, j) m = (i >= rmin && i <= rmax) && (j >= rmin && j <= rmax)
  where
    (rmin, rmax) = rangos m

casillaVacia :: Tablero -> Pos -> Bool
casillaVacia m p = elemento == " " || elemento == ""
  where
    elemento = m ! p

casillasVacias :: Tablero -> [Pos]
casillasVacias m = filter (casillaVacia m) [(i, j) | i <- [min .. max], j <- [min .. max]]
  where
    (min, max) = rangos m

casillasAlrededorFicha :: Tablero -> Pos -> [Pos]
casillasAlrededorFicha m (f, c) = [(i, j) | i <- filas, j <- columnas, auxiliarCasillas m (i, j) && (i, j) `notElem` exluidas]
  where
    filas = [f -1, f, f + 1]
    columnas = [c -1, c, c + 1]
    exluidas = [(f -1, c), (f + 1, c), (f, c -1), (f, c + 1)]

fichasAlrededorCasilla :: Tablero -> Pos -> [Pos]
fichasAlrededorCasilla m (f, c) = [(i, j) | i <- filas, j <- columnas, auxiliarFichas m (i, j) && (i, j) `notElem` exluidas]
  where
    filas = [f -1, f, f + 1]
    columnas = [c -1, c, c + 1]
    exluidas = [(f -1, c), (f + 1, c), (f, c -1), (f, c + 1)]

casillasSegundoNivel :: Tablero -> Pos -> [Pos]
casillasSegundoNivel m (f, c) = [(i, j) | i <- filas, j <- columnas, auxiliarCasillas m (i, j) && (i, j) `notElem` excluidas]
  where
    filas = [f -2, f, f + 2]
    columnas = [c -2, c, c + 2]
    excluidas = [(f -2, c), (f + 2, c), (f, c -2), (f, c + 2)]

buscaPieza :: Tablero -> String -> Pos
buscaPieza m pieza
  | null listaPiezas = (1, 1)
  | otherwise = cabeza "buscaPieza" listaPiezas
  where
    (min, max) = rangos m
    listaPiezas = [(f, c) | f <- [min .. max], c <- [min .. max], (m ! (f, c)) == pieza]

intercambiaPieza :: Tablero -> String -> Pos -> Pos -> Tablero
intercambiaPieza t pieza posNueva posAntigua = setElem pieza posNueva $ setElem " " posAntigua t

eliminaPieza :: Tablero -> Pos -> Tablero
eliminaPieza t p = setElem " " p t

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones normales en útiles.
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

cabeza :: String -> [a] -> a
cabeza funcion lista
  | null lista = error $ "La lista pasada en la funcion " ++ funcion ++ " esta vacia."
  | otherwise = head lista

esInt :: String -> Bool
esInt = foldr ((&&) . isDigit) True

esReal :: String -> Bool
esReal s
  | esInt entera && esInt fraccionaria = True
  | otherwise = False
  where
    entera = takeWhile (/= '.') s
    f = dropWhile (/= '.') s
    fraccionaria = if null f then "" else tail f

stringToInt :: String -> Int
stringToInt [] = 0
stringToInt c@(s : ss)
  | s == '-' = (-1) * stringToInt ss
  | otherwise = (digitToInt s * 10 ^ (length c - 1)) + stringToInt ss

stringToDouble :: String -> Double
stringToDouble [] = 0.0
stringToDouble s = entera + fraccionaria
  where
    e = stringToInt $ takeWhile (/= '.') s
    entera = fromInteger $ toInteger e
    f = tail $ dropWhile (/= '.') s
    fracEnt = fromInteger $ toInteger $ stringToInt f
    fraccionaria = fracEnt / (10 ^ length f)

stringToFloat :: String -> Float
stringToFloat [] = 0.0
stringToFloat s = entera + fraccionaria
  where
    e = stringToInt $ takeWhile (/= '.') s
    entera = fromInteger $ toInteger e
    f = tail $ dropWhile (/= '.') s
    fracEnt = fromInteger $ toInteger $ stringToInt f
    fraccionaria = fracEnt / (10 ^ length f)

listasDePares :: [a] -> [[a]]
listasDePares [] = []
listasDePares (x:y:ls) = [x,y] : listasDePares ls

distanciaEuclidea :: Float -> Float -> Float
distanciaEuclidea a b = sqrt $ (a - b) ** 2

intercambia :: [a] -> a -> Int -> [a]
intercambia xs a p = take (p-1) xs ++ [a] ++ drop (p+1) xs

introduce :: [a] -> a -> Int -> [a]
introduce xs a p = take p xs ++ [a] ++ drop (p+1) xs

elimina :: [a] -> Int -> [a]
elimina xs p = take (p-1) xs ++ drop (p+1) xs

eliminaElemento :: Eq a => [a] -> a -> [a]
eliminaElemento xs a = filter (/=a) xs

aleatorio :: Int -> [a] -> a
aleatorio al xs
  | null xs = error "Lista vacia en funcion aleatorio"
  | otherwise = x
  where
    x = xs !! mod al (length xs)

escogeAleatorios :: Double -> [a] -> IO [a]
escogeAleatorios _ [] = return []
escogeAleatorios al xs = do
  gen <- newStdGen
  let randoms = take (length xs) (randomRs (0,1) gen)
  let seleccion = map snd $ filter (\(r,_) -> r <= al) (zip randoms xs)
  return seleccion

normaliza :: Double -> Double
normaliza d = d - fromIntegral (floor d)

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones de IO en útiles.
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

nuevaLinea :: IO ()
nuevaLinea = do putStrLn ""

now :: IO Int
now = floor . utctDayTime <$> getCurrentTime

time :: IO Double
time = fromRational . toRational . utctDayTime <$> getCurrentTime

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones auxiliares
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

auxiliarCasillas :: Tablero -> Pos -> Bool
auxiliarCasillas m pos = dentroDelTablero pos m && casillaVacia m pos

auxiliarFichas :: Tablero -> Pos -> Bool
auxiliarFichas m pos = dentroDelTablero pos m && not (casillaVacia m pos)
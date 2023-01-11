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
    leeDigito,
    sacaPuntuacionesDeIO,
  )
where

import Data.Char
import qualified Data.Functor
import Data.List (nub)
import Data.Matrix
import Data.Time.Clock
import Tipos

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones de Matrix en útiles.
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

rangos :: Matrix a -> (Int, Int)
rangos m = (1, nrows m)

numeroElementosMatriz :: Matrix a -> Int
numeroElementosMatriz m = length $ toList m

columnasMatriz :: Matrix a -> [[a]]
columnasMatriz m = toLists $ transpose m

diagonalesMatriz :: Matrix a -> [[a]]
diagonalesMatriz m = diagonalPMatriz n m : [diagonalSMatriz n m]
  where
    n = nrows m

diagonalPMatriz :: Int -> Matrix a -> [a]
diagonalPMatriz n m = [m ! (x, x) | x <- [1 .. n]]

diagonalSMatriz :: Int -> Matrix a -> [a]
diagonalSMatriz n m = [m ! (x, n - x + 1) | x <- [1 .. n]]

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
casillaVacia m p
  | espacio || nula = True
  | otherwise = False
  where
    espacio = (m ! p) == " "
    nula = (m ! p) == ""

casillasVacias :: Tablero -> [Pos]
casillasVacias m = filter (casillaVacia m) casillas
  where
    (min, max) = rangos m
    casillas = [(i, j) | i <- [min .. max], j <- [min .. max]]

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
listasDePares lista = par : listasDePares resto
  where
    par = take 2 lista
    resto = drop 2 lista

distanciaEuclidea :: Float -> Float -> Float
distanciaEuclidea a b = sqrt $ (a - b) ** 2

intercambia :: [a] -> a -> Int -> [a]
intercambia (x : xs) a p
  | p == 0 = a : xs
  | null xs = [a]
  | otherwise = x : intercambia xs a (p -1)

introduce :: [a] -> a -> Int -> [a]
introduce (x : xs) a p
  | p == 0 = x : a : xs
  | null xs = x : [a]
  | otherwise = x : introduce xs a (p -1)

elimina :: [a] -> Int -> [a]
elimina (x : xs) p
  | p == 0 = xs
  | null xs = [x]
  | otherwise = x : elimina xs (p -1)

eliminaElemento :: Eq a => [a] -> a -> [a]
eliminaElemento (x : xs) a
  | x == a = xs
  | null xs = [x]
  | otherwise = x : eliminaElemento xs a

aleatorio :: Int -> [a] -> a
aleatorio al xs
  | null xs = error "Lista vacia en funcion aleatorio"
  | otherwise = x
  where
    limite = length xs
    a = mod al limite
    x = xs !! a

escogeAleatorios :: Double -> [a] -> IO [a]
escogeAleatorios _ [] = return []
escogeAleatorios al (x : xs) = do
  let prob = normaliza al
  tiempo <- time
  let contra = normaliza tiempo
  resto <- escogeAleatorios al xs
  if contra > prob
    then return $ x : resto
    else return resto

normaliza :: Double -> Double
normaliza d = d - fromIntegral (floor d)

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones de IO en útiles.
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

nuevaLinea :: IO ()
nuevaLinea = do putStrLn ""

now :: IO Int
now = getCurrentTime Data.Functor.<&> (floor . fromRational . toRational . utctDayTime)

time :: IO Double
time = getCurrentTime Data.Functor.<&> (fromRational . toRational . utctDayTime)

leeDigito :: String -> IO Int
leeDigito c = do
  putStrLn c
  digito <- getLine
  if esInt digito
    then do
      return $ stringToInt digito
    else do
      return 0

sacaPuntuacionesDeIO :: [IO Double] -> IO [Double]
sacaPuntuacionesDeIO [] = return []
sacaPuntuacionesDeIO (p : ps) = do
  np <- p
  nps <- sacaPuntuacionesDeIO ps
  let puntuaciones = np : nps
  return puntuaciones

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones auxiliares
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

auxiliarCasillas :: Tablero -> Pos -> Bool
auxiliarCasillas m pos = dentroDelTablero pos m && casillaVacia m pos

auxiliarFichas :: Tablero -> Pos -> Bool
auxiliarFichas m pos = dentroDelTablero pos m && not (casillaVacia m pos)
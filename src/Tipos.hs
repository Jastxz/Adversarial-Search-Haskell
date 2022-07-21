module Tipos (
    -- Tipos por sinónimos
    Tablero,
    -- Tipos definidos a partir de tipos nuevos
    Movimientos
) where

import Data.Matrix

type Tablero = Matrix String

type Movimientos = [Tablero]
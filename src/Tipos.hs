module Tipos (
    -- Tipos por sinónimos
    Tablero,
    Pos,
    Movimiento,
    TableroPuntuado,
    -- Tipos definidos a partir de tipos nuevos
    Movimientos,
    TablerosPuntuados
) where

import Data.Matrix

type Tablero = Matrix String
type Pos = (Int,Int)
type Movimiento = (Tablero, Pos)
type TableroPuntuado = (Tablero,Double)

type Movimientos = [Movimiento]
type TablerosPuntuados = [TableroPuntuado]
module Data.Matrix.SeitzSymbol (
  fromSeitzSymbol
  ) where

import Text.Parsec
import Data.Matrix.SeitzSymbol.Parser
import Data.Matrix.SymmetryOperationsSymbols.Common (properTbl,hexagonalTbl)

fromSeitzSymbol s = parse (seitzSymbol properTbl) s s

fromSeitzSymbolH s = parse (seitzSymbol hexagonalTbl) s s


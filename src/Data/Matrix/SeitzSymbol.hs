module Data.Matrix.SeitzSymbol (
  fromSeitzSymbol
  ) where

import Text.Parsec
import Data.Matrix.SeitzSymbol.Parser

fromSeitzSymbol s = parse seitzSymbol s s



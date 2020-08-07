module Data.Matrix.SeitzSymbol (
  fromSeitzSymbol
  ) where

import Text.Parsec
import Data.Matrix.SeitzSymbol.Parser

fromSeitzSymbol s = parse parser s s



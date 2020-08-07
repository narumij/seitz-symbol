module Data.Matrix.SeitzSymbol where

import Text.Parsec
import Data.Matrix.SeitzSymbol.Parser

fromSeitzSymbol s = parse parser s s



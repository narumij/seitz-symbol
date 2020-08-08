module Data.Matrix.SeitzSymbol (
  fromSeitzSymbol
  ) where

import Text.Parsec
import Text.Parsec.String (Parser)

import Data.Ratio
import Data.Matrix
import Data.Matrix.AsXYZ
import qualified Data.Matrix.SeitzSymbol.Parser as P (seitzSymbol,toMatrix,toSeitzSymbol,toString)
import Data.Matrix.SymmetryOperationsSymbols.Common (properTbl,hexagonalTbl)

-- | for all lattice type exclude hexagonal
--
-- >>> prettyXYZ <$> fromSeitzSymbol "{ 1 | 0 0 0 }"
-- Right "x,y,z"
-- >>> prettyXYZ <$> fromSeitzSymbol "{ 2 010 | 1/2 1/2 1/2 }"
-- Right "-x+1/2,y+1/2,-z+1/2"
-- >>> prettyXYZ <$> fromSeitzSymbol "{ 3+ 111 | 1/2 1/2 1/2 }"
-- Right "z+1/2,x+1/2,y+1/2"
-- >>> prettyXYZ <$> fromSeitzSymbol "{ -3+ 111 | 1/2 1/2 1/2 }"
-- Right "-z+1/2,-x+1/2,-y+1/2"
--
fromSeitzSymbol s = parse parser s s
  where
    parser :: (Integral a, Read a) => Parser (Matrix (Ratio a))
    parser = do
      s <- P.seitzSymbol
      case P.toMatrix properTbl s of
        Just m -> return m
        Nothing -> parserFail "Matrix not found."

-- | for Hexagonal
fromSeitzSymbolH s = parse parser s s
  where
    parser :: (Integral a, Read a) => Parser (Matrix (Ratio a))
    parser = do
      s <- P.seitzSymbol
      case P.toMatrix hexagonalTbl s of
        Just m -> return m
        Nothing -> parserFail "Matrix not found."

--type Tbl a = (Lattice,Symbol,SymbolLabel,Sense,SymmetryElement,Orientation a,TransformedCoordinate,AxisOrNormal a)

-- |
--
-- >>> toSeitzSymbol . fromXYZ $ "x,y,z"
-- Just "{ 1 | 0 0 0 }"
-- >>> toSeitzSymbol . fromXYZ $ "-x+1/2,y+1/2,-z+1/2"
-- Just "{ 2 010 | 1/2 1/2 1/2 }"
--
toSeitzSymbol :: (Integral a, Show a) => Matrix (Ratio a) -> Maybe String
toSeitzSymbol m = P.toString <$> P.toSeitzSymbol m

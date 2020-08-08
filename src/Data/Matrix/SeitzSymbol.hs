module Data.Matrix.SeitzSymbol (
  fromSeitzSymbol
  ) where

import Text.Parsec
import Text.Parsec.String (Parser)

import Data.Ratio (Ratio(..))
import Data.Matrix (Matrix(..))
import Data.Matrix.AsXYZ (fromXYZ,prettyXYZ)
import qualified Data.Matrix.SeitzSymbol.Parser as P (seitzSymbol,toMatrix,toSeitzSymbol,toString)
import Data.Matrix.SymmetryOperationsSymbols.Common (properMatricesForPointGroup,hexagonalMatricesForPointGroup,MatrixForPointGroupCorrespondingSymmetryElement(..))

parser :: (Integral a, Read a) => 
          [MatrixForPointGroupCorrespondingSymmetryElement a]
        -> Parser (Matrix (Ratio a))
parser tbl = do
  s <- P.seitzSymbol
  case P.toMatrix tbl s of
    Just m -> return m
    Nothing -> parserFail "Matrix not found."

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
fromSeitzSymbol :: (Integral a, Read a) =>
                   SourceName
                -> Either ParseError (Matrix (Ratio a))
fromSeitzSymbol s = parse (parser properMatricesForPointGroup) s s

-- | for Hexagonal
fromSeitzSymbolH :: (Integral a, Read a) =>
                     String
                  -> Either ParseError (Matrix (Ratio a))
fromSeitzSymbolH s = parse (parser hexagonalMatricesForPointGroup) s s


-- |
--
-- >>> toSeitzSymbol . fromXYZ $ "x,y,z"
-- Just "{ 1 | 0 0 0 }"
-- >>> toSeitzSymbol . fromXYZ $ "-x+1/2,y+1/2,-z+1/2"
-- Just "{ 2 010 | 1/2 1/2 1/2 }"
--
toSeitzSymbol :: (Integral a, Show a) => Matrix (Ratio a) -> Maybe String
toSeitzSymbol m = P.toString <$> P.toSeitzSymbol m

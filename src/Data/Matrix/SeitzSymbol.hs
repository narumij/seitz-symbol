{- |
Module      : Data.Matrix.SeitzSymbol
Copyright   : (c) Jun Narumi 2020
License     : MIT
Maintainer  : narumij@gmail.com
Stability   : experimental

Read and Display Seitz Symbol

[References]

ネスポロ マッシモ:日本結晶学会誌 59，210-222(2017).
https://www.jstage.jst.go.jp/article/jcrsj/59/5/59_210/_pdf

-}
module Data.Matrix.SeitzSymbol (
  P.SeitzSymbol(..),
  fromSeitzSymbol,
  fromSeitzSymbolH,
  toSeitzSymbol,
  ) where

import Text.Parsec
import Text.Parsec.String (Parser)

import Data.Ratio (Ratio(..))
import Data.Matrix (Matrix(..))
import Data.Matrix.AsXYZ (fromXYZ,prettyXYZ)
import qualified Data.Matrix.SeitzSymbol.Parser
  as P (SeitzSymbol(..),seitzSymbol,toMatrix,toSeitzSymbol,toString)
import Data.Matrix.SymmetryOperationsSymbols
import Data.Matrix.SymmetryOperationsSymbols.Common (
  properMatricesForPointGroup,
  hexagonalMatricesForPointGroup,
  MatrixForPointGroupCorrespondingSymmetryElement(..)
  )

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
-- >>> prettyXYZ <$> fromSeitzSymbol "{ m 100 | 0 0 0 }"
-- Right "-x,y,z"
-- >>> (liftError . fromSeitzSymbol) "{ 2 010 | 1/2 1/2 1/2 }" >>= fromMatrix
-- Right " 2 (0,1/2,0) 1/4,y,1/4"
-- >>> (liftError . fromSeitzSymbol) "{ 3+ 111 | 1/2 1/2 1/2 }" >>= fromMatrix
-- Right " 3+(1/2,1/2,1/2) x,x,x"
-- >>> (liftError . fromSeitzSymbol) "{ -3+ 111 | 1/2 1/2 1/2 }" >>= fromMatrix
-- Right "-3+ x,x,x; 1/4,1/4,1/4"
-- >>> (liftError . fromSeitzSymbol) "{ m 100 | 0 0 0 }" >>= fromMatrix
-- Right " m  0,y,z"
--
fromSeitzSymbol :: (Integral a, Read a) =>
                   SourceName
                -> Either ParseError (Matrix (Ratio a))
fromSeitzSymbol s = parse (parser properMatricesForPointGroup) s s


-- | for Hexagonal
--
-- >>> prettyXYZ <$> fromSeitzSymbolH "{ m 100 | 0 0 0 }"
-- Right "y-x,y,z"
-- >>> prettyXYZ <$> fromSeitzSymbolH "{ m 120 | 0 0 0 }"
-- Right "x-y,-y,z"
-- >>> prettyXYZ <$> fromSeitzSymbolH "{ 2 100 | 0 0 0 }"
-- Right "x-y,-y,-z"
-- >>> (liftError . fromSeitzSymbolH) "{ m 100 | 0 0 0 }" >>= fromMatrix
-- Right " m  x,2x,z"
-- >>> (liftError . fromSeitzSymbolH) "{ m 120 | 0 0 0 }" >>= fromMatrix
-- Right " m  x,0,z"
-- >>> (liftError . fromSeitzSymbolH) "{ 2 100 | 0 0 0 }" >>= fromMatrix
-- Right " 2  x,0,0"
--
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

{- |
Module      : Data.Matrix.SeitzSymbol.Parser
Copyright   : (c) Jun Narumi 2020
License     : MIT
Maintainer  : narumij@gmail.com
Stability   : experimental

Seitz Symbol parser and etc.

[References]

A. Michael Glazer et al. Seitz symbols Acta Cryst. (2014). A70

ネスポロ マッシモ:日本結晶学会誌 59，210-222(2017).
https://www.jstage.jst.go.jp/article/jcrsj/59/5/59_210/_pdf

-}
module Data.Matrix.SeitzSymbol.Parser (
  SeitzSymbol(..),
  seitzSymbol,
  toMatrix,
  toSeitzSymbol,
  toString,
  ) where

import Data.Ratio (Ratio(..),(%))
import Text.Parsec
import Text.Parsec.String (Parser)

import Data.Ratio.Slash (Slash(..))
import Data.Matrix (Matrix(..),fromLists,toList,submatrix)
import Data.Matrix.AsXYZ (fromXYZ)
import qualified Data.Matrix as M ((<->),(<|>))

import Data.Matrix.SymmetryOperationsSymbols.Common (properMatricesForPointGroup,MatrixForPointGroupCorrespondingSymmetryElement(..))

type SeitzSymbol a = (String,String,(a,a,a),(Ratio a,Ratio a,Ratio a))

optionSpaces :: Parser ()
optionSpaces = skipMany space

identity :: Parser String
identity = do
  char '1'
  return "1"

symbol :: Parser (String,String)
symbol = try irot <|> try rot <|> miller <|> two
  where
    sign = oneOf "-+"
    rot = do
      a <- oneOf "346"
      b <- sign
      return (a:[],b:[])
    two = do
      char '2'
      return ("2","")
    miller = do
      char 'm'
      return ("m","")
    irot = do
      char '-'
      a <- oneOf "346"
      b <- sign
      return ('-':a:[],b:[])

zero :: Num a => Parser a
zero = do
  char '0'
  return 0

one :: Num a => Parser a
one = do
  char '1'
  return 1

two :: Num a => Parser a
two = do
  char '2'
  return 2

minus :: Num a => Parser a
minus = do
  string "-1"
  return (-1)

d :: Num a => Parser a
d = do
  a <- (zero <|> one <|> two <|> minus)
  return a

orientation :: Num a => Parser (a,a,a)
orientation = try a <|> b
  where
    a = do
      char '['
      a <- d
      b <- d
      c <- d
      char ']'
      return (a,b,c)
    b = do
      a <- d
      b <- d
      c <- d
      return (a,b,c)


num :: (Num a, Read a) => Parser a
num = do
  x <- oneOf "123456789"
  xs <- many digit
  return $ read (x : xs)

int :: (Integral a, Read a) => Parser a
int = zero <|> num

fract :: (Integral a, Read a) => Parser (Ratio a)
fract = do
  n <- int
  char '/'
  d <- int
  return $ n % d

integer :: (Integral a, Read a) => Parser (Ratio a)
integer = do
  i <- int
  return (i%1)

number :: (Integral a, Read a) => Parser (Ratio a)
number = do
  try fract <|> integer

matrixPart :: Num a => Parser (String,String,(a,a,a))
matrixPart = try a <|> b
  where
    a = do
      (sy,si) <- symbol
      spaces
      o <- orientation
      return (sy,si,o)
    b = do
      s <- identity
      return (s,"",(0,0,0))

seitzSymbol :: (Integral a, Read a) => Parser (SeitzSymbol a)
seitzSymbol = do
  char '{'
  optionSpaces
  (sy,si,o) <- matrixPart
  optionSpaces
  char '|'
  optionSpaces
  p <- number
  spaces
  q <- number
  spaces
  r <- number
  optionSpaces
  char '}'
  return (sy,si,o,(p,q,r))

toMatrix :: (Integral a,Read a) =>
            [MatrixForPointGroupCorrespondingSymmetryElement a]
          -> SeitzSymbol a
          -> Maybe (Matrix (Ratio a))
toMatrix tbl (sy,si,(o1,o2,o3),(p,q,r)) = build p q r <$> result
  where
    transformCoordinate (_,_,symbolLabel,sense,_,orientation,transformedCoordinate,_)
      = ( (symbolLabel,sense,if null orientation then [0,0,0] else orientation), transformedCoordinate )
    result = lookup (sy,si,[o1,o2,o3]) $ map transformCoordinate tbl
    build p q r xyz = _W M.<|> _w M.<-> fromLists [[0,0,0,1]]
      where
        _W = submatrix 1 3 1 3 . fromXYZ $ xyz
        _w = fromLists [[p],[q],[r]]

toString :: (Integral a, Show a) => SeitzSymbol a -> String
toString ("1",si,(o1,o2,o3),(p,q,r))
  = "{ " ++ "1"
  ++ " | "
  ++ show (Slash p) ++ " " ++ show (Slash q) ++ " " ++ show (Slash r)
  ++ " }"
toString (sy,si,(o1,o2,o3),(p,q,r))
  = "{ " ++ sy ++ si ++ " "
  ++ show o1 ++ show o2 ++ show o3
  ++ " | "
  ++ show (Slash p) ++ " " ++ show (Slash q) ++ " " ++ show (Slash r)
  ++ " }"

toSeitzSymbol :: Integral a => Matrix (Ratio a) -> Maybe (SeitzSymbol a)
toSeitzSymbol m = lookup w $ map tt properMatricesForPointGroup
  where
    getW = submatrix 1 3 1 3
    getw = submatrix 1 3 4 4
    w = getW m
    p:q:r:[] = toList . getw $ m
    tt (_,_,symbolLabel,sense,_,(o1:o2:o3:_),transformedCoordinate,_)
      = (getW . fromXYZ $ transformedCoordinate, (symbolLabel,sense,(o1,o2,o3),(p,q,r)))
    tt (_,_,symbolLabel,sense,_,[],transformedCoordinate,_)
      = (getW . fromXYZ $ transformedCoordinate, (symbolLabel,sense,(0,0,0),(p,q,r)))


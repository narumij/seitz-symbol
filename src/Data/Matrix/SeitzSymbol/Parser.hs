module Data.Matrix.SeitzSymbol.Parser where

import Data.Ratio
import Text.Parsec
import Text.Parsec.String (Parser)

import Data.Matrix.SeitzSymbol.Table
import Data.Matrix.AsXYZ
import qualified Data.Matrix as M

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

minus :: Num a => Parser a
minus = do
  string "-1"
  return (-1)

d :: Num a => Parser a
d = do
  a <- (zero <|> one <|> minus)
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

matrix :: Num a => Parser (String,String,(a,a,a))
matrix = try a <|> b
  where
    a = do
      (sy,si) <- symbol
      spaces
      o <- orientation
      return (sy,si,o)
    b = do
      s <- identity
      return (s,"",(0,0,0))

parser' :: (Integral a, Read a) => Parser (String,String,(a,a,a),(Ratio a,Ratio a,Ratio a))
parser' = do
  char '{'
  optionSpaces
  (sy,si,o) <- matrix
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

transformCoordinate' (_,_,symbolLabel,sense,_,orientation,transformedCoordinate,_)
  = ( (symbolLabel,sense,orientation), transformedCoordinate )

seitzSymbol = do
  (sy,si,(o1,o2,o3),(p,q,r)) <- parser'
  let result = lookup (sy,si,[o1,o2,o3]) $ map transformCoordinate' tbl
  case result of
    Just xyz -> return (build xyz p q r)
    Nothing -> parserFail "seitz matrix not found"
  where
    build xyz p q r = _W M.<|> _w M.<-> M.fromLists [[0,0,0,1]]
      where
        _W = M.submatrix 1 3 1 3 . fromXYZ $ xyz
        _w = M.fromLists [[p],[q],[r]]


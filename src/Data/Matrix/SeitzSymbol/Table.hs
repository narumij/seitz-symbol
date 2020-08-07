module Data.Matrix.SeitzSymbol.Table where

data Symbol
   = Id  --  '1'
   | T   --   t
   | Inv -- '-1'
   | M   --  'm'
   | A   --   a
   | B   --   b
   | C   --   c
   | D   --   d
   | G   --   g
   | N   --   n
   | R2  --  '2'
   | R3  --  '3'
   | R4  --  '4'
   | R6  --  '6'
   | RI3 -- '-3'
   | RI4 -- '-4'
   | RI6 -- '-6'
   deriving (Show,Eq)

data Lattice = Hexagonal | AnythingElse deriving (Eq)
type SymbolLabel = String
type Sense = String
type SymmetryElement = String
type Orientation = [Integer] -- orientation or location
type TransformedCoordinate = String
type AxisOrNormal = [Integer]

type Tbl = (Lattice,Symbol,SymbolLabel,Sense,SymmetryElement,Orientation,TransformedCoordinate,AxisOrNormal)

tbl :: [Tbl]
tbl = [
-- Table 11.2.2.1. Matrices for point-group symmetry operations and orientation
-- of corresponding symmetry elements, referred to a cubic, tetragonal, orthorhombic,
-- monoclinic, triclinic or rhombohedral coordinate system
  ( AnythingElse,  Id,  "1",  "",        "",         [],     "x,y,z", []),
  ( AnythingElse,  R2,  "2",  "",   "0,0,z", [ 0, 0, 1],   "-x,-y,z", []),
  ( AnythingElse,  R2,  "2",  "",   "0,y,0", [ 0, 1, 0],   "-x,y,-z", []),
  ( AnythingElse,  R2,  "2",  "",   "x,0,0", [ 1, 0, 0],   "x,-y,-z", []),
  ( AnythingElse,  R3,  "3", "+",   "x,x,x", [ 1, 1, 1],     "z,x,y", []),
  ( AnythingElse,  R3,  "3", "+", "x,-x,-x", [ 1,-1,-1],   "-z,-x,y", []),
  ( AnythingElse,  R3,  "3", "+", "-x,x,-x", [-1, 1,-1],   "z,-x,-y", []),
  ( AnythingElse,  R3,  "3", "+", "-x,-x,x", [-1,-1, 1],   "-z,x,-y", []),
  ( AnythingElse,  R3,  "3", "-",   "x,x,x", [ 1, 1, 1],     "y,z,x", []),
  ( AnythingElse,  R3,  "3", "-", "x,-x,-x", [ 1,-1,-1],   "-y,z,-x", []),
  ( AnythingElse,  R3,  "3", "-", "-x,x,-x", [-1, 1,-1],   "-y,-z,x", []),
  ( AnythingElse,  R3,  "3", "-", "-x,-x,x", [-1,-1, 1],   "y,-z,-x", []),
  ( AnythingElse,  R2,  "2",  "",   "x,x,0", [ 1, 1, 0],    "y,x,-z", []),
  ( AnythingElse,  R2,  "2",  "",   "x,0,x", [ 1, 0, 1],    "z,-y,x", []),
  ( AnythingElse,  R2,  "2",  "",   "0,y,y", [ 0, 1, 1],    "-x,z,y", []),
  ( AnythingElse,  R2,  "2",  "",  "x,-x,0", [ 1,-1, 0],  "-y,-x,-z", []),
  ( AnythingElse,  R2,  "2",  "",  "-x,0,x", [-1, 0, 1],  "-z,-y,-x", []),
  ( AnythingElse,  R2,  "2",  "",  "0,y,-y", [ 0, 1,-1],  "-x,-z,-y", []),
  ( AnythingElse,  R4,  "4", "+",   "0,0,z", [ 0, 0, 1],    "-y,x,z", []),
  ( AnythingElse,  R4,  "4", "+",   "0,y,0", [ 0, 1, 0],    "z,y,-x", []),
  ( AnythingElse,  R4,  "4", "+",   "x,0,0", [ 1, 0, 0],    "x,-z,y", []),
  ( AnythingElse,  R4,  "4", "-",   "0,0,z", [ 0, 0, 1],    "y,-x,z", []),
  ( AnythingElse,  R4,  "4", "-",   "0,y,0", [ 0, 1, 0],    "-z,y,x", []),
  ( AnythingElse,  R4,  "4", "-",   "x,0,0", [ 1, 0, 0],    "x,z,-y", []),
----
  ( AnythingElse, Inv, "-1",  "",   "0,0,0",         [],  "-x,-y,-z", []),
  ( AnythingElse,   M,  "m",  "",   "x,y,0", [ 0, 0, 1],    "x,y,-z", []),
  ( AnythingElse,   M,  "m",  "",   "x,0,z", [ 0, 1, 0],    "x,-y,z", []),
  ( AnythingElse,   M,  "m",  "",   "0,y,z", [ 1, 0, 0],    "-x,y,z", []),
  ( AnythingElse, RI3, "-3", "+",   "x,x,x", [ 1, 1, 1],  "-z,-x,-y", []),
  ( AnythingElse, RI3, "-3", "+", "x,-x,-x", [ 1,-1,-1],    "z,x,-y", []),
  ( AnythingElse, RI3, "-3", "+", "-x,x,-x", [-1, 1,-1],    "-z,x,y", []),
  ( AnythingElse, RI3, "-3", "+", "-x,-x,x", [-1,-1, 1],    "z,-x,y", []),
  ( AnythingElse, RI3, "-3", "-",   "x,x,x", [ 1, 1, 1],  "-y,-z,-x", []),
  ( AnythingElse, RI3, "-3", "-", "x,-x,-x", [ 1,-1,-1],    "y,-z,x", []),
  ( AnythingElse, RI3, "-3", "-", "-x,x,-x", [-1, 1,-1],    "y,z,-x", []),
  ( AnythingElse, RI3, "-3", "-", "-x,-x,x", [-1,-1, 1],    "-y,z,x", []),
  ( AnythingElse,   M,  "m",  "",  "x,-x,z", [ 1, 1, 0],   "-y,-x,z", []),
  ( AnythingElse,   M,  "m",  "",  "-x,y,x", [ 1, 0, 1],   "-z,y,-x", []),
  ( AnythingElse,   M,  "m",  "",  "x,y,-y", [ 0, 1, 1],   "x,-z,-y", []),
  ( AnythingElse,   M,  "m",  "",   "x,x,z", [ 1,-1, 0],     "y,x,z", []),
  ( AnythingElse,   M,  "m",  "",   "x,y,x", [-1, 0, 1],     "z,y,x", []),
  ( AnythingElse,   M,  "m",  "",   "x,y,y", [ 0, 1,-1],     "x,z,y", []),
  ( AnythingElse, RI4, "-4", "+",   "0,0,z", [ 0, 0, 1],   "y,-x,-z", []),
  ( AnythingElse, RI4, "-4", "+",   "0,y,0", [ 0, 1, 0],   "-z,-y,x", []),
  ( AnythingElse, RI4, "-4", "+",   "x,0,0", [ 1, 0, 0],   "-x,z,-y", []),
  ( AnythingElse, RI4, "-4", "-",   "0,0,z", [ 0, 0, 1],   "-y,x,-z", []),
  ( AnythingElse, RI4, "-4", "-",   "0,y,0", [ 0, 1, 0],   "z,-y,-x", []),
  ( AnythingElse, RI4, "-4", "-",   "x,0,0", [ 1, 0, 0],   "-x,-z,y", []),
-- Table 11.2.2.2. Matrices for point-group symmetry operations and orientation
-- of corresponding symmetry elements, referred to a hexagonal coordinate system
  (    Hexagonal,  Id,  "1",  "",        "",         [],     "x,y,z", []),
  (    Hexagonal,  R3,  "3", "+",   "0,0,z", [ 0, 0, 1],  "-y,x-y,z", []),
  (    Hexagonal,  R3,  "3", "-",   "0,0,z", [ 0, 0, 1],  "y-x,-x,z", []),
  (    Hexagonal,  R2,  "2",  "",   "0,0,z", [ 0, 0, 1],   "-x,-y,z", []),
  (    Hexagonal,  R6,  "6", "+",   "0,0,z", [ 0, 0, 1],   "x-y,x,z", []),
  (    Hexagonal,  R6,  "6", "-",   "0,0,z", [ 0, 0, 1],   "y,y-x,z", []),
  (    Hexagonal,  R2,  "2",  "",   "x,x,0", [ 1, 1, 0],    "y,x,-z", []),
  (    Hexagonal,  R2,  "2",  "",   "x,0,0", [ 1, 0, 0], "x-y,-y,-z", []),
  (    Hexagonal,  R2,  "2",  "",   "0,y,0", [ 0, 1, 0], "-x,y-x,-z", []),
  (    Hexagonal,  R2,  "2",  "",  "x,-x,0", [ 1,-1, 0],  "-y,-x,-z", []),
--
  (    Hexagonal,  R2,  "2",  "",  "x,2x,0", [ 1, 2, 0],  "y-x,y,-z", []),
  (    Hexagonal,  R2,  "2",  "",  "2x,x,0", [ 2, 1, 0],  "x,x-y,-z", []),
-- 
  (    Hexagonal, Inv, "-1",  "",   "0,0,0",         [],  "-x,-y,-z", []),
  (    Hexagonal, RI3, "-3", "+",   "0,0,z", [ 0, 0, 1],  "y,y-x,-z", []),
  (    Hexagonal, RI3, "-3", "-",   "0,0,z", [ 0, 0, 1],  "x-y,x,-z", []),
  (    Hexagonal,   M,  "m",  "",   "x,y,0", [ 0, 0, 1],    "x,y,-z", []),
  (    Hexagonal, RI6, "-6", "+",   "0,0,z", [ 0, 0, 1], "y-x,-x,-z", []),
  (    Hexagonal, RI6, "-6", "-",   "0,0,z", [ 0, 0, 1], "-y,x-y,-z", []),
  (    Hexagonal,   M,  "m",  "",  "x,-x,z", [ 1, 1, 0],   "-y,-x,z", []),
-- 以下二つが、Orientationを利用して解の補正をすることができなかったため、代替値を用意している
-- 行列を解いた場合の解平面とorientationが一致していない可能性（2017の試行錯誤)
-- そもそも勘違いの可能性もまだあるので、のちのち再確認する。
-- hackageに登録するに至らない理由の一つ
-- 正規化された値として正しいが、正規化の結果、復元に必要な情報が欠落してしまった可能性(2020リファクタリング時の見解)
-- どうしてこうなっているのか、やっぱりわからない。
  (    Hexagonal,   M,  "m",  "",  "x,2x,z", [ 1, 0, 0],   "y-x,y,z", [ 2,-1, 0]),
  (    Hexagonal,   M,  "m",  "",  "2x,x,z", [ 0, 1, 0],   "x,x-y,z", [-1, 2, 0]),

  (    Hexagonal,   M,  "m",  "",   "x,x,z", [ 1,-1, 0],     "y,x,z", []),
  (    Hexagonal,   M,  "m",  "",   "x,0,z", [ 1, 2, 0],  "x-y,-y,z", []),
  (    Hexagonal,   M,  "m",  "",   "0,y,z", [ 2, 1, 0],  "-x,y-x,z", [])
-- Notice
-- Hexagonal用のテーブルが、HexagonalのITで出現する対称操作全てをカバーしているわけではないことに注意
-- hexagonalでのlookup時には、hexagonal部分が優先となるよう、順番をいれかえている
-- それ以外のケースでは除外している
  ]

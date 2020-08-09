module SetizSymbolSpec where

import Test.Hspec

import Data.Matrix.SeitzSymbol
import Data.Matrix.AsXYZ (fromXYZ,prettyXYZ)
import Data.Matrix.SymmetryOperationsSymbols

spec :: Spec
spec = do

   describe "Data.Matrix.SeitzSymbol" $ do
      it "fromSeitzSymbol" $ do
        prettyXYZ <$> fromSeitzSymbolS "{ 1 | 0 0 0 }"
          `shouldBe` Right "x,y,z"
        prettyXYZ <$> fromSeitzSymbolS "{ 2 010 | 1/2 1/2 1/2 }"
          `shouldBe` Right "-x+1/2,y+1/2,-z+1/2"
        prettyXYZ <$> fromSeitzSymbolS "{ 3+ 111 | 1/2 1/2 1/2 }"
          `shouldBe` Right "z+1/2,x+1/2,y+1/2"
        prettyXYZ <$> fromSeitzSymbolS "{ -3+ 111 | 1/2 1/2 1/2 }"
          `shouldBe` Right "-z+1/2,-x+1/2,-y+1/2"
        prettyXYZ <$> fromSeitzSymbolS "{ m 100 | 0 0 0 }"
          `shouldBe`  Right "-x,y,z"
        ((liftError . fromSeitzSymbolS) "{ 2 010 | 1/2 1/2 1/2 }" >>= fromMatrix)
          `shouldBe` Right " 2 (0,1/2,0) 1/4,y,1/4"
        ((liftError . fromSeitzSymbolS) "{ 3+ 111 | 1/2 1/2 1/2 }" >>= fromMatrix)
          `shouldBe` Right " 3+(1/2,1/2,1/2) x,x,x"
        ((liftError . fromSeitzSymbolS) "{ -3+ 111 | 1/2 1/2 1/2 }" >>= fromMatrix)
          `shouldBe` Right "-3+ x,x,x; 1/4,1/4,1/4"
        ((liftError . fromSeitzSymbolS) "{ m 100 | 0 0 0 }" >>= fromMatrix)
          `shouldBe` Right " m  0,y,z"

      it "fromSeitzSymbolH" $ do
        prettyXYZ <$> fromSeitzSymbolHexS "{ m 100 | 0 0 0 }"
          `shouldBe` Right "y-x,y,z"
        prettyXYZ <$> fromSeitzSymbolHexS "{ m 120 | 0 0 0 }"
          `shouldBe` Right "x-y,-y,z"
        prettyXYZ <$> fromSeitzSymbolHexS "{ 2 100 | 0 0 0 }"
          `shouldBe` Right "x-y,-y,-z"
        ((liftError . fromSeitzSymbolHexS) "{ m 100 | 0 0 0 }" >>= fromMatrix)
          `shouldBe` Right " m  x,2x,z"
        ((liftError . fromSeitzSymbolHexS) "{ m 120 | 0 0 0 }" >>= fromMatrix)
          `shouldBe` Right " m  x,0,z"
        ((liftError . fromSeitzSymbolHexS) "{ 2 100 | 0 0 0 }" >>= fromMatrix)
          `shouldBe` Right " 2  x,0,0"

      it "toSeitzSymbol" $ do
        (toSeitzSymbolS . fromXYZ $ "x,y,z")
          `shouldBe` Just "{ 1 | 0 0 0 }"
        (toSeitzSymbolS . fromXYZ $ "-x+1/2,y+1/2,-z+1/2")
          `shouldBe` Just "{ 2 010 | 1/2 1/2 1/2 }"


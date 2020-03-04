module P3
  ( computeAffine 
  ) where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Static as S
import GHC.TypeNats as T

computeAffine :: Triangle -> Triangle -> S.Sq 3  
computeAffine (Triangle x1 x2 x3) (Triangle y1 y2 y3) = S.matrix [ ] 

data Triangle = Triangle (S.R 3) (S.R 3) (S.R 3)

toAffine :: T.KnownNat n => S.R n -> S.R (n + 1)
toAffine x = x # 1.0

toLin :: S.R 3 -> S.R 2
toLin x = let (y, _) = S.split x in y



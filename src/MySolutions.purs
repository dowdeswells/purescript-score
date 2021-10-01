module MySolutions where

import Prelude

import Math (abs, sqrt)
  

diagonal :: Number -> Number -> Number
diagonal l w = sqrt(l * l + w*w)

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }
  
newtype Complex =
    Complex
    {
        real :: Number,
        imaginary :: Number
    }

sign ::Number -> String
sign n = 
    case (n < 0.0)  of 
        true -> "-"
        _ -> "+"

instance showComplex :: Show Complex where
  show (Complex n) = (show n.real) <> (sign n.imaginary) <> (show (abs n.imaginary)) <> "i"
      

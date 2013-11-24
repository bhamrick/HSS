{-# LANGUAGE FlexibleInstances #-}
module HSS.Length where

import Prelude
import qualified Prelude as P
import HSS.Datatypes

import Data.ByteString.Char8
import Data.Char
import Text.Printf

-- TODO: Math with lengths
data LengthUnit = Em
                | Ex
                | Ch
                | Rem
                | Vw
                | Vh
                | Vmin
                | Vmax
                | Cm
                | Mm
                | In
                | Px
                | Pt
                | Pc
                deriving (Eq, Show, Ord, Enum)

data Length = Length Float LengthUnit
    deriving (Eq, Show, Ord)

instance PropertyValue Length where
    displayValue (Length x u) = pack $ printf "%f%s" x (P.map toLower $ show u)

liftUnitless f (Length x u) = Length (f x) u
lift2Unitless f (Length x1 u1) (Length x2 u2) = if u1 == u2
    then Length (f x1 x2) u2
    else error "Nonmatching units"

-- This only provides addition/subtraction, not a full instance
-- Will likely define custom operators rather than overloading
-- Num like this
instance Num Length where
    (+) = lift2Unitless (+)
    (*) = error "Multiplication of lengths not supported yet"
    negate = liftUnitless negate
    abs = error "No abs of lengths"
    signum = error "No signum of lengths"
    fromInteger = error "No fromInteger for lengths"

instance Num (LengthUnit -> Length) where
    x + y = \u -> lift2Unitless (+) (x u) (y u)
    x * y = \u -> lift2Unitless (*) (x u) (y u)
    negate = (liftUnitless negate .)
    abs = (liftUnitless abs .)
    signum = (liftUnitless signum .)
    fromInteger x = Length (fromInteger x)

instance Fractional (LengthUnit -> Length) where
    x / y = \u -> lift2Unitless (/) (x u) (y u)
    fromRational x = Length (fromRational x)


module Imaginary exposing
    ( Imaginary(..)
    , andMap
    , equal
    , equalImplementation
    , i
    , imaginary
    , map
    , negate
    , pure
    , zero
    )

import Float.Extra
import Typeclasses.Classes.Equality


{-| Imaginary portion
-}
type Imaginary i
    = Imaginary i


i : Imaginary number
i =
    Imaginary 1


{-| zero
-}
zero : Imaginary number
zero =
    Imaginary 0


negate : Imaginary number -> Imaginary number
negate imag =
    Imaginary -(imaginary imag)


{-| Extracts the imaginary part of a complex number
-}
imaginary : Imaginary a -> a
imaginary (Imaginary imag) =
    imag


{-| Map over a complex number
-}
map : (a -> b) -> Imaginary a -> Imaginary b
map f (Imaginary r) =
    Imaginary <| f r


{-| Place a value in the minimal Complex Number Cartesian context
-}
pure : a -> Imaginary a
pure a =
    Imaginary a


{-| Apply for Complex Number Cartesian representaiton applicative
-}
andMap :
    Imaginary a
    -> Imaginary (a -> b)
    -> Imaginary b
andMap (Imaginary imag) (Imaginary fImag) =
    Imaginary <| fImag imag


{-| Equality of Imaginary Numbers
-}
equalImplementation :
    Imaginary Float
    -> Imaginary Float
    -> Bool
equalImplementation (Imaginary realOne) (Imaginary realTwo) =
    Float.Extra.equalWithin 0.000000001 realOne realTwo


{-| `Equal` type for `Imaginary`.
-}
equal : Typeclasses.Classes.Equality.Equality (Imaginary Float)
equal =
    Typeclasses.Classes.Equality.eq equalImplementation

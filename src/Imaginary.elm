module Imaginary exposing
    ( Imaginary(..)
    , i
    , zero
    , imaginary
    , negate
    , map
    , pure
    , andMap
    , equal
    )

{-| A module for Imaginary numbers


# Types

@docs Imaginary


# Values

@docs i
@docs zero


# Arithmetic operations on complex numbers

@docs imaginary
@docs negate


# Semigroup, Monoid, Group, Ring, Field, Functor, Applicative Functor, and Monad

@docs map
@docs pure
@docs andMap

#Equality

@docs equal

-}

import Float.Extra
import Typeclasses.Classes.Equality


{-| Imaginary portion
-}
type Imaginary i
    = Imaginary i


{-| i
-}
i : Imaginary number
i =
    Imaginary 1


{-| zero
-}
zero : Imaginary number
zero =
    Imaginary 0


{-| Negate an Imaginary number
-}
negate : Imaginary number -> Imaginary number
negate imag =
    Imaginary -(imaginary imag)


{-| Extracts the value of an Imaginary number
-}
imaginary : Imaginary a -> a
imaginary (Imaginary imag) =
    imag


{-| Map over anImaginary Imaginary number
-}
map : (a -> b) -> Imaginary a -> Imaginary b
map f (Imaginary r) =
    Imaginary <| f r


{-| Place a value in the minimal Imaginary context
-}
pure : a -> Imaginary a
pure a =
    Imaginary a


{-| Apply for Imaginary representaiton applicative
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

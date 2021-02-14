module Imaginary exposing
    ( Imaginary(..)
    , andMap
    , i
    , imaginary
    , map
    , negate
    , pure
    , zero
    )

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

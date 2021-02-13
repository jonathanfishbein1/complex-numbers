module Imaginary exposing
    ( Imaginary(..)
    , i
    , imaginary
    , negate
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

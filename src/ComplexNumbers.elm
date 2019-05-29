module ComplexNumbers exposing
    ( Real(..)
    , Imaginary(..)
    , ComplexNumberCartesian(..)
    , i
    , zero
    , realPart
    , imaginaryPart
    , add
    , sum
    , multiply
    , product
    , subtract
    , divide
    , modulus
    , conjugate
    , convertFromCartesianToPolar
    , convertFromPolarToCartesian
    , mapCartesian
    , pureCartesian
    , applyCartesian
    , bindCartesian
    , equal
    )

{-| A module for complex numbers


# Types

@docs Real
@docs Imaginary
@docs Modulus
@docs Theta
@docs ComplexNumberCartesian
@docs ComplexNumberPolar


# Arithmetic operations on complex numbers

@docs i
@docs zero
@docs realPart
@docs imaginaryPart
@docs modulusPart
@docs thetaPart
@docs add
@docs sum
@docs multiply
@docs product
@docs subtract
@docs divide
@docs modulus
@docs conjugate
@docs convertFromCartesianToPolar
@docs convertFromPolarToCartesian
@docs multiplyPolar
@docs dividePolar
@docs power
@docs mapCartesian
@docs mapPolar
@docs pureCartesian
@docs purePolar
@docs applyCartesian
@docs applyPolar
@docs bindCartesian
@docs bindPolar

-}

import Float.Extra
import Internal.ComplexNumbers
import Monoid



-- Types


{-| Real portion
-}
type Real r
    = Real r


{-| Imaginary portion
-}
type Imaginary i
    = Imaginary i


{-| Cartesian representation of a complex number
-}
type ComplexNumberCartesian a
    = ComplexNumberCartesian (Real a) (Imaginary a)


{-| zero
-}
zero : ComplexNumberCartesian number
zero =
    ComplexNumberCartesian (Real 0) (Imaginary 0)


{-| The number i
-}
i : ComplexNumberCartesian number
i =
    ComplexNumberCartesian (Real 0) (Imaginary 1)


{-| Extracts the real part of a complex number
-}
realPart : ComplexNumberCartesian a -> a
realPart (ComplexNumberCartesian (Real real) _) =
    real


{-| Extracts the imaginary part of a complex number
-}
imaginaryPart : ComplexNumberCartesian a -> a
imaginaryPart (ComplexNumberCartesian _ (Imaginary imaginary)) =
    imaginary


{-| Add two complex numbers together
-}
add : ComplexNumberCartesian number -> ComplexNumberCartesian number -> ComplexNumberCartesian number
add complexOne complexTwo =
    liftCartesian (+) complexOne complexTwo


sumEmpty : ComplexNumberCartesian number
sumEmpty =
    ComplexNumberCartesian (Real 0) (Imaginary 0)


{-| Monoidally add two complex numbers together
-}
sum : Monoid.Monoid (ComplexNumberCartesian number)
sum =
    Monoid.monoid sumEmpty add


{-| Multiply two complex numbers together
-}
multiply : ComplexNumberCartesian Float -> ComplexNumberCartesian Float -> ComplexNumberCartesian Float
multiply complexNumberOne complexNumberTwo =
    Internal.ComplexNumbers.multiplyPolar (convertFromCartesianToPolar complexNumberOne) (convertFromCartesianToPolar complexNumberTwo)
        |> convertFromPolarToCartesian



--ComplexNumberCartesian (Real (realOne * realTwo - imaginaryOne * imaginaryTwo)) (Imaginary (realOne * imaginaryTwo + realTwo * imaginaryOne))


productEmpty : ComplexNumberCartesian number
productEmpty =
    ComplexNumberCartesian (Real 1) (Imaginary 0)


{-| Monoidally multiply two complex numbers together
-}
product : Monoid.Monoid (ComplexNumberCartesian Float)
product =
    Monoid.monoid productEmpty multiply


{-| Subtract two complex numbers together
-}
subtract : ComplexNumberCartesian number -> ComplexNumberCartesian number -> ComplexNumberCartesian number
subtract complexNumberOne complexNumberTwo =
    liftCartesian (-) complexNumberOne complexNumberTwo


calculateDivisor : ComplexNumberCartesian number -> number
calculateDivisor (ComplexNumberCartesian (Real realTwo) (Imaginary imaginaryTwo)) =
    realTwo ^ 2 + imaginaryTwo ^ 2


{-| Divide two complex numbers together
-}
divide : ComplexNumberCartesian Float -> ComplexNumberCartesian Float -> Result String (ComplexNumberCartesian Float)
divide (ComplexNumberCartesian (Real realOne) (Imaginary imaginaryOne)) complexNumberCartesianDivisor =
    let
        divisor =
            calculateDivisor complexNumberCartesianDivisor

        (ComplexNumberCartesian (Real realTwo) (Imaginary imaginaryTwo)) =
            complexNumberCartesianDivisor

        realResult =
            (realOne * realTwo + imaginaryOne * imaginaryTwo) / divisor

        imaginaryResult =
            realTwo * imaginaryOne - realOne * imaginaryTwo
    in
    case round divisor of
        0 ->
            Err "Divisor is zero"

        _ ->
            Ok <| ComplexNumberCartesian (Real realResult) (Imaginary imaginaryResult)


{-| Calculate the modulus of a complex number
-}
modulus : ComplexNumberCartesian Float -> Float
modulus =
    calculateDivisor >> sqrt


{-| Calculate the conjugate of a complex number
-}
conjugate : ComplexNumberCartesian number -> ComplexNumberCartesian number
conjugate (ComplexNumberCartesian real (Imaginary imaginaryOne)) =
    ComplexNumberCartesian real (Imaginary -imaginaryOne)


{-| Convert from the Cartesian representation of a complex number to the polar representation
-}
convertFromCartesianToPolar : ComplexNumberCartesian Float -> Internal.ComplexNumbers.ComplexNumberPolar Float
convertFromCartesianToPolar (ComplexNumberCartesian (Real real) (Imaginary imaginary)) =
    let
        polar =
            toPolar ( real, imaginary )
    in
    Internal.ComplexNumbers.ComplexNumberPolar (Internal.ComplexNumbers.Modulus <| Tuple.first polar) (Internal.ComplexNumbers.Theta <| Tuple.second polar)


{-| Convert from the polar representation of a complex number to the Cartesian representation
-}
convertFromPolarToCartesian : Internal.ComplexNumbers.ComplexNumberPolar Float -> ComplexNumberCartesian Float
convertFromPolarToCartesian (Internal.ComplexNumbers.ComplexNumberPolar (Internal.ComplexNumbers.Modulus ro) (Internal.ComplexNumbers.Theta theta)) =
    let
        cartesian =
            fromPolar ( ro, theta )
    in
    ComplexNumberCartesian (Real <| Tuple.first cartesian) (Imaginary <| Tuple.second cartesian)


{-| Map over a complex number
-}
mapCartesian : (a -> b) -> ComplexNumberCartesian a -> ComplexNumberCartesian b
mapCartesian f (ComplexNumberCartesian (Real realOne) (Imaginary imaginaryOne)) =
    ComplexNumberCartesian (Real <| f realOne) (Imaginary <| f imaginaryOne)


{-| Place a value in the minimal Complex Number Cartesian context
-}
pureCartesian : a -> ComplexNumberCartesian a
pureCartesian a =
    ComplexNumberCartesian (Real a) (Imaginary a)


{-| Apply for Complex Number Cartesian representaiton applicative
-}
applyCartesian : ComplexNumberCartesian (a -> b) -> ComplexNumberCartesian a -> ComplexNumberCartesian b
applyCartesian (ComplexNumberCartesian (Real fReal) (Imaginary fImaginary)) (ComplexNumberCartesian (Real real) (Imaginary imaginary)) =
    ComplexNumberCartesian (Real <| fReal real) (Imaginary <| fImaginary imaginary)


{-| Monadic bind for Complex Number Cartesian representaiton
-}
bindCartesian : ComplexNumberCartesian a -> (a -> ComplexNumberCartesian b) -> ComplexNumberCartesian b
bindCartesian (ComplexNumberCartesian (Real previousReal) (Imaginary previousImaginary)) f =
    ComplexNumberCartesian (Real <| realPart <| f previousReal) (Imaginary <| imaginaryPart <| f previousImaginary)


liftCartesian : (a -> b -> c) -> ComplexNumberCartesian a -> ComplexNumberCartesian b -> ComplexNumberCartesian c
liftCartesian f a b =
    applyCartesian (mapCartesian f a) b


{-| Equality of Complex Numbers
-}
equal : ComplexNumberCartesian Float -> ComplexNumberCartesian Float -> Bool
equal (ComplexNumberCartesian (Real realOne) (Imaginary imaginaryOne)) (ComplexNumberCartesian (Real realTwo) (Imaginary imaginaryTwo)) =
    Float.Extra.equalWithin 0.000000001 realOne realTwo && Float.Extra.equalWithin 0.000000001 imaginaryOne imaginaryTwo

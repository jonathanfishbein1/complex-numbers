module ComplexNumbers exposing
    ( ComplexNumber(..)
    , add
    , complexAdd
    , Imaginary(..), Real(..), divide, modulus, multiply
    )

{-| A module for complex numbers


# Types

@docs ComplexNumber


# Add two complex numbers

@docs add


# Monoidally add two complex numbers

@docs complexAdd

-}

import Monoid



-- Types
{- Real Portion -}


type Real r
    = Real r



{- Real Imaginary -}


type Imaginary i
    = Imaginary i


{-| Main type.
-}
type ComplexNumber a b
    = ComplexNumber (Real a) (Imaginary b)



-- Complex addition


{-| Add two complex numbers together
-}
add : ComplexNumber number number -> ComplexNumber number number -> ComplexNumber number number
add (ComplexNumber (Real realOne) (Imaginary imaginaryOne)) (ComplexNumber (Real realTwo) (Imaginary imaginaryTwo)) =
    ComplexNumber (Real (realOne + realTwo)) (Imaginary (imaginaryOne + imaginaryTwo))


{-| Monoidally add two complex numbers together
-}
complexAdd : Monoid.Monoid (ComplexNumber number number)
complexAdd =
    Monoid.monoid (ComplexNumber (Real 0) (Imaginary 0)) add


{-| Multiply two complex numbers together
-}
multiply : ComplexNumber number number -> ComplexNumber number number -> ComplexNumber number number
multiply (ComplexNumber (Real realOne) (Imaginary imaginaryOne)) (ComplexNumber (Real realTwo) (Imaginary imaginaryTwo)) =
    ComplexNumber (Real (realOne * realTwo - imaginaryOne * imaginaryTwo)) (Imaginary (realOne * imaginaryTwo + realTwo * imaginaryOne))


{-| Subtract two complex numbers together
-}
subtract : ComplexNumber number number -> ComplexNumber number number -> ComplexNumber number number
subtract (ComplexNumber (Real realOne) (Imaginary imaginaryOne)) (ComplexNumber (Real realTwo) (Imaginary imaginaryTwo)) =
    ComplexNumber (Real (realOne - realTwo)) (Imaginary (imaginaryOne - imaginaryTwo))


calculateDivisor : ComplexNumber number number -> number
calculateDivisor (ComplexNumber (Real realTwo) (Imaginary imaginaryTwo)) =
    realTwo ^ 2 + imaginaryTwo ^ 2


{-| Divide two complex numbers together
-}
divide : ComplexNumber Float Float -> ComplexNumber Float Float -> Result String (ComplexNumber Float Float)
divide (ComplexNumber (Real realOne) (Imaginary imaginaryOne)) complexNumberDivisor =
    let
        divisor =
            calculateDivisor complexNumberDivisor

        (ComplexNumber (Real realTwo) (Imaginary imaginaryTwo)) =
            complexNumberDivisor

        realResult =
            (realOne * realTwo + imaginaryOne * imaginaryTwo) / divisor

        imaginaryResult =
            realTwo * imaginaryOne - realOne * imaginaryTwo
    in
    case round divisor of
        0 ->
            Err "Divisor is zero"

        _ ->
            Ok <| ComplexNumber (Real realResult) (Imaginary imaginaryResult)


{-| Monoidally multiply two complex numbers together
-}
complexMultiply : Monoid.Monoid (ComplexNumber number number)
complexMultiply =
    Monoid.monoid (ComplexNumber (Real 1) (Imaginary 1)) multiply


{-| Calculate the modulus of a complex number
-}
modulus : ComplexNumber Float Float -> Float
modulus =
    calculateDivisor >> sqrt

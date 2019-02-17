module ComplexNumbers exposing
    ( ComplexNumber(..)
    , add
    , complexAdd
    , Imaginary(..), Real(..), divide, multiply
    )

{-| A module for complex numbers
For instance, we defined generic `concat` in this module using `Monoid` type as follows.

    concat : Monoid a -> List a -> a
    concat m =
        List.foldr (append m) (empty m)


# Types

@docs ComplexNumber


# Add two complex numbers

@docs add


# Monoidally add two complex numbers

@docs complexAdd

-}

import Monoid



-- Types


type Real r
    = Real r


type Imaginary i
    = Imaginary i


{-| Main type.
-}
type ComplexNumber a b
    = ComplexNumber (Real a) (Imaginary b)



-- Complex addition


add : ComplexNumber number number -> ComplexNumber number number -> ComplexNumber number number
add (ComplexNumber (Real realOne) (Imaginary imaginaryOne)) (ComplexNumber (Real realTwo) (Imaginary imaginaryTwo)) =
    ComplexNumber (Real (realOne + realTwo)) (Imaginary (imaginaryOne + imaginaryTwo))


complexAdd : Monoid.Monoid (ComplexNumber number number)
complexAdd =
    Monoid.monoid (ComplexNumber (Real 0) (Imaginary 0)) add


multiply : ComplexNumber number number -> ComplexNumber number number -> ComplexNumber number number
multiply (ComplexNumber (Real realOne) (Imaginary imaginaryOne)) (ComplexNumber (Real realTwo) (Imaginary imaginaryTwo)) =
    ComplexNumber (Real (realOne * realTwo - imaginaryOne * imaginaryTwo)) (Imaginary (realOne * imaginaryTwo + realTwo * imaginaryOne))


calculateDivisor : ComplexNumber number number -> number
calculateDivisor (ComplexNumber (Real realTwo) (Imaginary imaginaryTwo)) =
    realTwo ^ 2 + imaginaryTwo ^ 2


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


complexMultiply : Monoid.Monoid (ComplexNumber number number)
complexMultiply =
    Monoid.monoid (ComplexNumber (Real 1) (Imaginary 1)) multiply

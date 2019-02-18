module ComplexNumbers exposing
    ( Real(..)
    , Imaginary(..)
    , ComplexNumberCartesian(..)
    , add
    , divide
    , modulus
    , conjugate
    , convertFromCartesianToPolar
    , convertFromPolarToCartesian
    , multiply, product, sum
    )

{-| A module for complex numbers


# Types

@docs Real
@docs Imaginary
@docs Modulus
@docs Theta
@docs ComplexNumberCartesian
@docs ComplexNumbersPolar

{-| Arithmetic operations on complex numbers -}


# Add two complex numbers

@docs add


# Monoidally add two complex numbers

@docs complexAdd


# Multiply two complex numbers

@docs complexMultiply


# Monoidally multiply two complex numbers

@docs complexAdd


# Subtract two complex numbers

@docs subtract


# Divide two complex numbers

@docs divide


# Calculate modulus of a complex number

@docs modulus


# Calculate the conjugate of a complex number

@docs conjugate


# Convert from the Cartesian representation of a complex number to the polar representation

@docs convertFromCartesianToPolar


# Convert from the polar representation of a complex number to the Cartesian representation

@docs convertFromPolarToCartesian

-}

import Monoid



-- Types
{- Real Portion -}


type Real r
    = Real r



{- Real Imaginary -}


type Imaginary i
    = Imaginary i


type Modulus m
    = Modulus m


type Theta t
    = Theta t


{-| Main type.
-}
type ComplexNumberCartesian a b
    = ComplexNumberCartesian (Real a) (Imaginary b)


type ComplexNumberPolar a b
    = ComplexNumberPolar (Modulus a) (Theta b)



-- Complex addition


{-| Add two complex numbers together
-}
add : ComplexNumberCartesian number number -> ComplexNumberCartesian number number -> ComplexNumberCartesian number number
add (ComplexNumberCartesian (Real realOne) (Imaginary imaginaryOne)) (ComplexNumberCartesian (Real realTwo) (Imaginary imaginaryTwo)) =
    ComplexNumberCartesian (Real (realOne + realTwo)) (Imaginary (imaginaryOne + imaginaryTwo))


{-| Monoidally add two complex numbers together
-}
sum : Monoid.Monoid (ComplexNumberCartesian number number)
sum =
    Monoid.monoid (ComplexNumberCartesian (Real 0) (Imaginary 0)) add


{-| Multiply two complex numbers together
-}
multiply : ComplexNumberCartesian number number -> ComplexNumberCartesian number number -> ComplexNumberCartesian number number
multiply (ComplexNumberCartesian (Real realOne) (Imaginary imaginaryOne)) (ComplexNumberCartesian (Real realTwo) (Imaginary imaginaryTwo)) =
    ComplexNumberCartesian (Real (realOne * realTwo - imaginaryOne * imaginaryTwo)) (Imaginary (realOne * imaginaryTwo + realTwo * imaginaryOne))


{-| Monoidally multiply two complex numbers together
-}
product : Monoid.Monoid (ComplexNumberCartesian number number)
product =
    Monoid.monoid (ComplexNumberCartesian (Real 1) (Imaginary 0)) multiply


{-| Subtract two complex numbers together
-}
subtract : ComplexNumberCartesian number number -> ComplexNumberCartesian number number -> ComplexNumberCartesian number number
subtract (ComplexNumberCartesian (Real realOne) (Imaginary imaginaryOne)) (ComplexNumberCartesian (Real realTwo) (Imaginary imaginaryTwo)) =
    ComplexNumberCartesian (Real (realOne - realTwo)) (Imaginary (imaginaryOne - imaginaryTwo))


calculateDivisor : ComplexNumberCartesian number number -> number
calculateDivisor (ComplexNumberCartesian (Real realTwo) (Imaginary imaginaryTwo)) =
    realTwo ^ 2 + imaginaryTwo ^ 2


{-| Divide two complex numbers together
-}
divide : ComplexNumberCartesian Float Float -> ComplexNumberCartesian Float Float -> Result String (ComplexNumberCartesian Float Float)
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
modulus : ComplexNumberCartesian Float Float -> Float
modulus =
    calculateDivisor >> sqrt


{-| Calculate the conjugate of a complex number
-}
conjugate : ComplexNumberCartesian number number -> ComplexNumberCartesian number number
conjugate (ComplexNumberCartesian real (Imaginary imaginaryOne)) =
    ComplexNumberCartesian real (Imaginary -imaginaryOne)


{-| Convert from the Cartesian representation of a complex number to the polar representation
-}
convertFromCartesianToPolar : ComplexNumberCartesian Float Float -> ComplexNumberPolar Float Float
convertFromCartesianToPolar (ComplexNumberCartesian (Real real) (Imaginary imaginary)) =
    let
        polar =
            toPolar ( real, imaginary )
    in
    ComplexNumberPolar (Modulus <| Tuple.first polar) (Theta <| Tuple.second polar)


{-| Convert from the polar representation of a complex number to the Cartesian representation
-}
convertFromPolarToCartesian : ComplexNumberPolar Float Float -> ComplexNumberCartesian Float Float
convertFromPolarToCartesian (ComplexNumberPolar (Modulus ro) (Theta theta)) =
    let
        cartesian =
            fromPolar ( ro, theta )
    in
    ComplexNumberCartesian (Real <| Tuple.first cartesian) (Imaginary <| Tuple.second cartesian)

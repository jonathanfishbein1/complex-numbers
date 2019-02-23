module ComplexNumbers exposing
    ( Real(..)
    , Imaginary(..)
    , Modulus(..)
    , Theta(..)
    , ComplexNumberCartesian(..)
    , ComplexNumberPolar(..)
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
    , applyCartesian, applyPolar, bindCartesian, bindPolar, dividePolar, i, imaginaryPart, mapCartesian, mapPolar, modulusPart, multiplyPolar, power, pureCartesian, purePolar, realPart, thetaPart
    )

{-| A module for complex numbers


# Types

@docs Real
@docs Imaginary
@docs Modulus
@docs Theta
@docs ComplexNumberCartesian
@docs ComplexNumberPolar

{-| Arithmetic operations on complex numbers -}


# Add two complex numbers

@docs add


# Monoidally add two complex numbers

@docs sum


# Multiply two complex numbers

@docs multiply


# Monoidally multiply two complex numbers

@docs product


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


{-| Real portion
-}
type Real r
    = Real r


{-| Imaginary portion
-}
type Imaginary i
    = Imaginary i


{-| Modulus or magnitude portion
-}
type Modulus m
    = Modulus m


{-| Angle in real-complex plane of modulus
-}
type Theta t
    = Theta t


{-| Cartesian representation of a complex number
-}
type ComplexNumberCartesian a
    = ComplexNumberCartesian (Real a) (Imaginary a)


{-| Polar representation of a complex number
-}
type ComplexNumberPolar a
    = ComplexNumberPolar (Modulus a) (Theta a)


{-| The number i
-}
i : ComplexNumberCartesian number
i =
    ComplexNumberCartesian (Real 0) (Imaginary 1)



-- Unary Operation


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


{-| Extracts the modulus part of a complex number
-}
modulusPart : ComplexNumberPolar a -> a
modulusPart (ComplexNumberPolar (Modulus ro) _) =
    ro


{-| Extracts the imaginary part of a complex number
-}
thetaPart : ComplexNumberPolar a -> a
thetaPart (ComplexNumberPolar _ (Theta theta)) =
    theta



-- Binary Operations


{-| Add two complex numbers together
-}
add : ComplexNumberCartesian number -> ComplexNumberCartesian number -> ComplexNumberCartesian number
add (ComplexNumberCartesian (Real realOne) (Imaginary imaginaryOne)) (ComplexNumberCartesian (Real realTwo) (Imaginary imaginaryTwo)) =
    ComplexNumberCartesian (Real (realOne + realTwo)) (Imaginary (imaginaryOne + imaginaryTwo))


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
multiply : ComplexNumberCartesian number -> ComplexNumberCartesian number -> ComplexNumberCartesian number
multiply (ComplexNumberCartesian (Real realOne) (Imaginary imaginaryOne)) (ComplexNumberCartesian (Real realTwo) (Imaginary imaginaryTwo)) =
    ComplexNumberCartesian (Real (realOne * realTwo - imaginaryOne * imaginaryTwo)) (Imaginary (realOne * imaginaryTwo + realTwo * imaginaryOne))


productEmpty : ComplexNumberCartesian number
productEmpty =
    ComplexNumberCartesian (Real 1) (Imaginary 0)


{-| Monoidally multiply two complex numbers together
-}
product : Monoid.Monoid (ComplexNumberCartesian number)
product =
    Monoid.monoid productEmpty multiply


{-| Subtract two complex numbers together
-}
subtract : ComplexNumberCartesian number -> ComplexNumberCartesian number -> ComplexNumberCartesian number
subtract (ComplexNumberCartesian (Real realOne) (Imaginary imaginaryOne)) (ComplexNumberCartesian (Real realTwo) (Imaginary imaginaryTwo)) =
    ComplexNumberCartesian (Real (realOne - realTwo)) (Imaginary (imaginaryOne - imaginaryTwo))


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
convertFromCartesianToPolar : ComplexNumberCartesian Float -> ComplexNumberPolar Float
convertFromCartesianToPolar (ComplexNumberCartesian (Real real) (Imaginary imaginary)) =
    let
        polar =
            toPolar ( real, imaginary )
    in
    ComplexNumberPolar (Modulus <| Tuple.first polar) (Theta <| Tuple.second polar)


{-| Convert from the polar representation of a complex number to the Cartesian representation
-}
convertFromPolarToCartesian : ComplexNumberPolar Float -> ComplexNumberCartesian Float
convertFromPolarToCartesian (ComplexNumberPolar (Modulus ro) (Theta theta)) =
    let
        cartesian =
            fromPolar ( ro, theta )
    in
    ComplexNumberCartesian (Real <| Tuple.first cartesian) (Imaginary <| Tuple.second cartesian)


{-| Multiply two complex numbers in polar representations together
-}
multiplyPolar : ComplexNumberPolar number -> ComplexNumberPolar number -> ComplexNumberPolar number
multiplyPolar (ComplexNumberPolar (Modulus roOne) (Theta thetaOne)) (ComplexNumberPolar (Modulus roTwo) (Theta thetaTwo)) =
    ComplexNumberPolar (Modulus <| roOne * roTwo) (Theta <| thetaOne + thetaTwo)


{-| Divide two complex numbers in polar representations together
-}
dividePolar : ComplexNumberPolar Float -> ComplexNumberPolar Float -> Result String (ComplexNumberPolar Float)
dividePolar (ComplexNumberPolar (Modulus roOne) (Theta thetaOne)) (ComplexNumberPolar (Modulus roTwo) (Theta thetaTwo)) =
    case round roTwo of
        0 ->
            Err "Divisor is zero"

        _ ->
            Ok <| ComplexNumberPolar (Modulus <| roOne / roTwo) (Theta <| thetaOne - thetaTwo)


power : number -> ComplexNumberPolar number -> ComplexNumberPolar number
power n (ComplexNumberPolar (Modulus roOne) (Theta thetaOne)) =
    ComplexNumberPolar (Modulus <| roOne ^ n) (Theta <| n * thetaOne)


{-| Map over a complex number
-}
mapCartesian : (a -> b) -> ComplexNumberCartesian a -> ComplexNumberCartesian b
mapCartesian f (ComplexNumberCartesian (Real realOne) (Imaginary imaginaryOne)) =
    ComplexNumberCartesian (Real <| f realOne) (Imaginary <| f imaginaryOne)


{-| Map over a complex number in polar representation
-}
mapPolar : (a -> b) -> ComplexNumberPolar a -> ComplexNumberPolar b
mapPolar f (ComplexNumberPolar (Modulus ro) (Theta theta)) =
    ComplexNumberPolar (Modulus <| f ro) (Theta <| f theta)


pureCartesian : a -> ComplexNumberCartesian a
pureCartesian a =
    ComplexNumberCartesian (Real a) (Imaginary a)


purePolar : a -> ComplexNumberPolar a
purePolar a =
    ComplexNumberPolar (Modulus a) (Theta a)


applyCartesian : ComplexNumberCartesian (a -> b) -> ComplexNumberCartesian a -> ComplexNumberCartesian b
applyCartesian (ComplexNumberCartesian (Real fReal) (Imaginary fImaginary)) (ComplexNumberCartesian (Real real) (Imaginary imaginary)) =
    ComplexNumberCartesian (Real <| fReal real) (Imaginary <| fImaginary imaginary)


applyPolar : ComplexNumberPolar (a -> b) -> ComplexNumberPolar a -> ComplexNumberPolar b
applyPolar (ComplexNumberPolar (Modulus fRo) (Theta fTheta)) (ComplexNumberPolar (Modulus ro) (Theta theta)) =
    ComplexNumberPolar (Modulus <| fRo ro) (Theta <| fTheta theta)


bindCartesian : ComplexNumberCartesian a -> (a -> ComplexNumberCartesian b) -> ComplexNumberCartesian b
bindCartesian (ComplexNumberCartesian (Real previousReal) (Imaginary previousImaginary)) f =
    ComplexNumberCartesian (Real <| realPart <| f previousReal) (Imaginary <| imaginaryPart <| f previousImaginary)


bindPolar : ComplexNumberPolar a -> (a -> ComplexNumberPolar b) -> ComplexNumberPolar b
bindPolar (ComplexNumberPolar (Modulus previousModulus) (Theta previousTheta)) f =
    ComplexNumberPolar (Modulus <| modulusPart <| f previousModulus) (Theta <| thetaPart <| f previousTheta)

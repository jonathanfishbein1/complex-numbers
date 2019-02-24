module ComplexNumbers exposing
    ( Real(..)
    , Imaginary(..)
    , Modulus(..)
    , Theta(..)
    , ComplexNumber(..)
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
    , multiplyPolar
    , dividePolar
    , applyCartesian, applyPolar, bindCartesian, bindPolar, i, imaginaryPart, mapCartesian, mapPolar, modulusPart, power, pureCartesian, purePolar, realPart, thetaPart
    )

{-| A module for complex numbers


# Types

@docs Real
@docs Imaginary
@docs Modulus
@docs Theta
@docs ComplexNumber
@docs ComplexNumberPolar


# Arithmetic operations on complex numbers

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
type ComplexNumber a
    = ComplexNumber (Real a) (Imaginary a)


{-| Polar representation of a complex number
-}
type ComplexNumberPolar a
    = ComplexNumberPolar (Modulus a) (Theta a)


{-| The number i
-}
i : ComplexNumber number
i =
    ComplexNumber (Real 0) (Imaginary 1)



-- Unary Operation


{-| Extracts the real part of a complex number
-}
realPart : ComplexNumber a -> a
realPart (ComplexNumber (Real real) _) =
    real


{-| Extracts the imaginary part of a complex number
-}
imaginaryPart : ComplexNumber a -> a
imaginaryPart (ComplexNumber _ (Imaginary imaginary)) =
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
add : ComplexNumber number -> ComplexNumber number -> ComplexNumber number
add (ComplexNumber (Real realOne) (Imaginary imaginaryOne)) (ComplexNumber (Real realTwo) (Imaginary imaginaryTwo)) =
    ComplexNumber (Real (realOne + realTwo)) (Imaginary (imaginaryOne + imaginaryTwo))


sumEmpty : ComplexNumber number
sumEmpty =
    ComplexNumber (Real 0) (Imaginary 0)


{-| Monoidally add two complex numbers together
-}
sum : Monoid.Monoid (ComplexNumber number)
sum =
    Monoid.monoid sumEmpty add


{-| Multiply two complex numbers together
-}
multiply : ComplexNumber number -> ComplexNumber number -> ComplexNumber number
multiply (ComplexNumber (Real realOne) (Imaginary imaginaryOne)) (ComplexNumber (Real realTwo) (Imaginary imaginaryTwo)) =
    ComplexNumber (Real (realOne * realTwo - imaginaryOne * imaginaryTwo)) (Imaginary (realOne * imaginaryTwo + realTwo * imaginaryOne))


productEmpty : ComplexNumber number
productEmpty =
    ComplexNumber (Real 1) (Imaginary 0)


{-| Monoidally multiply two complex numbers together
-}
product : Monoid.Monoid (ComplexNumber number)
product =
    Monoid.monoid productEmpty multiply


{-| Subtract two complex numbers together
-}
subtract : ComplexNumber number -> ComplexNumber number -> ComplexNumber number
subtract (ComplexNumber (Real realOne) (Imaginary imaginaryOne)) (ComplexNumber (Real realTwo) (Imaginary imaginaryTwo)) =
    ComplexNumber (Real (realOne - realTwo)) (Imaginary (imaginaryOne - imaginaryTwo))


calculateDivisor : ComplexNumber number -> number
calculateDivisor (ComplexNumber (Real realTwo) (Imaginary imaginaryTwo)) =
    realTwo ^ 2 + imaginaryTwo ^ 2


{-| Divide two complex numbers together
-}
divide : ComplexNumber Float -> ComplexNumber Float -> Result String (ComplexNumber Float)
divide (ComplexNumber (Real realOne) (Imaginary imaginaryOne)) complexNumberCartesianDivisor =
    let
        divisor =
            calculateDivisor complexNumberCartesianDivisor

        (ComplexNumber (Real realTwo) (Imaginary imaginaryTwo)) =
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
            Ok <| ComplexNumber (Real realResult) (Imaginary imaginaryResult)


{-| Calculate the modulus of a complex number
-}
modulus : ComplexNumber Float -> Float
modulus =
    calculateDivisor >> sqrt


{-| Calculate the conjugate of a complex number
-}
conjugate : ComplexNumber number -> ComplexNumber number
conjugate (ComplexNumber real (Imaginary imaginaryOne)) =
    ComplexNumber real (Imaginary -imaginaryOne)


{-| Convert from the Cartesian representation of a complex number to the polar representation
-}
convertFromCartesianToPolar : ComplexNumber Float -> ComplexNumberPolar Float
convertFromCartesianToPolar (ComplexNumber (Real real) (Imaginary imaginary)) =
    let
        polar =
            toPolar ( real, imaginary )
    in
    ComplexNumberPolar (Modulus <| Tuple.first polar) (Theta <| Tuple.second polar)


{-| Convert from the polar representation of a complex number to the Cartesian representation
-}
convertFromPolarToCartesian : ComplexNumberPolar Float -> ComplexNumber Float
convertFromPolarToCartesian (ComplexNumberPolar (Modulus ro) (Theta theta)) =
    let
        cartesian =
            fromPolar ( ro, theta )
    in
    ComplexNumber (Real <| Tuple.first cartesian) (Imaginary <| Tuple.second cartesian)


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


{-| Calculate a complex number raised to a power
-}
power : number -> ComplexNumberPolar number -> ComplexNumberPolar number
power n (ComplexNumberPolar (Modulus roOne) (Theta thetaOne)) =
    ComplexNumberPolar (Modulus <| roOne ^ n) (Theta <| n * thetaOne)


{-| Map over a complex number
-}
mapCartesian : (a -> b) -> ComplexNumber a -> ComplexNumber b
mapCartesian f (ComplexNumber (Real realOne) (Imaginary imaginaryOne)) =
    ComplexNumber (Real <| f realOne) (Imaginary <| f imaginaryOne)


{-| Map over a complex number in polar representation
-}
mapPolar : (a -> b) -> ComplexNumberPolar a -> ComplexNumberPolar b
mapPolar f (ComplexNumberPolar (Modulus ro) (Theta theta)) =
    ComplexNumberPolar (Modulus <| f ro) (Theta <| f theta)


{-| Place a value in the minimal Complex Number Cartesian context
-}
pureCartesian : a -> ComplexNumber a
pureCartesian a =
    ComplexNumber (Real a) (Imaginary a)


{-| Place a value in the minimal Complex Number polar context
-}
purePolar : a -> ComplexNumberPolar a
purePolar a =
    ComplexNumberPolar (Modulus a) (Theta a)


{-| Apply for Complex Number Cartesian representaiton applicative
-}
applyCartesian : ComplexNumber (a -> b) -> ComplexNumber a -> ComplexNumber b
applyCartesian (ComplexNumber (Real fReal) (Imaginary fImaginary)) (ComplexNumber (Real real) (Imaginary imaginary)) =
    ComplexNumber (Real <| fReal real) (Imaginary <| fImaginary imaginary)


{-| Apply for Complex Number polar representaiton applicative
-}
applyPolar : ComplexNumberPolar (a -> b) -> ComplexNumberPolar a -> ComplexNumberPolar b
applyPolar (ComplexNumberPolar (Modulus fRo) (Theta fTheta)) (ComplexNumberPolar (Modulus ro) (Theta theta)) =
    ComplexNumberPolar (Modulus <| fRo ro) (Theta <| fTheta theta)


{-| Monadic bind for Complex Number Cartesian representaiton
-}
bindCartesian : ComplexNumber a -> (a -> ComplexNumber b) -> ComplexNumber b
bindCartesian (ComplexNumber (Real previousReal) (Imaginary previousImaginary)) f =
    ComplexNumber (Real <| realPart <| f previousReal) (Imaginary <| imaginaryPart <| f previousImaginary)


{-| Monadic bind for Complex Number polar representaiton
-}
bindPolar : ComplexNumberPolar a -> (a -> ComplexNumberPolar b) -> ComplexNumberPolar b
bindPolar (ComplexNumberPolar (Modulus previousModulus) (Theta previousTheta)) f =
    ComplexNumberPolar (Modulus <| modulusPart <| f previousModulus) (Theta <| thetaPart <| f previousTheta)

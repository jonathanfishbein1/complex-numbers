module ComplexNumbers exposing
    ( Real(..)
    , Imaginary(..)
    , Modulus(..)
    , Theta(..)
    , ComplexNumberCartesian(..)
    , ComplexNumberPolar(..)
    , i
    , zero
    , realPart
    , imaginaryPart
    , modulusPart
    , thetaPart
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
    , power
    , mapCartesian
    , mapPolar
    , pureCartesian
    , purePolar
    , applyCartesian
    , applyPolar
    , bindCartesian
    , bindPolar
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


{-| Calculate a complex number raised to a power
-}
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


{-| Place a value in the minimal Complex Number Cartesian context
-}
pureCartesian : a -> ComplexNumberCartesian a
pureCartesian a =
    ComplexNumberCartesian (Real a) (Imaginary a)


{-| Place a value in the minimal Complex Number polar context
-}
purePolar : a -> ComplexNumberPolar a
purePolar a =
    ComplexNumberPolar (Modulus a) (Theta a)


{-| Apply for Complex Number Cartesian representaiton applicative
-}
applyCartesian : ComplexNumberCartesian (a -> b) -> ComplexNumberCartesian a -> ComplexNumberCartesian b
applyCartesian (ComplexNumberCartesian (Real fReal) (Imaginary fImaginary)) (ComplexNumberCartesian (Real real) (Imaginary imaginary)) =
    ComplexNumberCartesian (Real <| fReal real) (Imaginary <| fImaginary imaginary)


{-| Apply for Complex Number polar representaiton applicative
-}
applyPolar : ComplexNumberPolar (a -> b) -> ComplexNumberPolar a -> ComplexNumberPolar b
applyPolar (ComplexNumberPolar (Modulus fRo) (Theta fTheta)) (ComplexNumberPolar (Modulus ro) (Theta theta)) =
    ComplexNumberPolar (Modulus <| fRo ro) (Theta <| fTheta theta)


{-| Monadic bind for Complex Number Cartesian representaiton
-}
bindCartesian : ComplexNumberCartesian a -> (a -> ComplexNumberCartesian b) -> ComplexNumberCartesian b
bindCartesian (ComplexNumberCartesian (Real previousReal) (Imaginary previousImaginary)) f =
    ComplexNumberCartesian (Real <| realPart <| f previousReal) (Imaginary <| imaginaryPart <| f previousImaginary)


{-| Monadic bind for Complex Number polar representaiton
-}
bindPolar : ComplexNumberPolar a -> (a -> ComplexNumberPolar b) -> ComplexNumberPolar b
bindPolar (ComplexNumberPolar (Modulus previousModulus) (Theta previousTheta)) f =
    ComplexNumberPolar (Modulus <| modulusPart <| f previousModulus) (Theta <| thetaPart <| f previousTheta)


liftCartesian : (a -> b -> c) -> ComplexNumberCartesian a -> ComplexNumberCartesian b -> ComplexNumberCartesian c
liftCartesian f a b =
    applyCartesian (mapCartesian f a) b

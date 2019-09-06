module ComplexNumbers exposing
    ( Real(..)
    , Imaginary(..)
    , ComplexNumberCartesian(..)
    , i
    , zero
    , one
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
    , power
    , complexField
    , read
    , print
    )

{-| A module for complex numbers


# Types

@docs Real
@docs Imaginary
@docs ComplexNumberCartesian


# Arithmetic operations on complex numbers

@docs i
@docs zero
@docs one
@docs realPart
@docs imaginaryPart
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
@docs mapCartesian
@docs pureCartesian
@docs applyCartesian
@docs bindCartesian
@docs equal
@docs power
@docs complexField

@docs read
@docs print

-}

import Field
import Float.Extra
import Internal.ComplexNumbers
import Parser exposing ((|.), (|=))
import Typeclasses.Classes.Equality
import Typeclasses.Classes.Monoid
import Typeclasses.Classes.Semigroup



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


{-| one
-}
one : ComplexNumberCartesian number
one =
    ComplexNumberCartesian (Real 1) (Imaginary 0)


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
sum : Typeclasses.Classes.Monoid.Monoid (ComplexNumberCartesian number)
sum =
    Typeclasses.Classes.Monoid.semigroupAndIdentity (Typeclasses.Classes.Semigroup.prepend add) sumEmpty


{-| Multiply two complex numbers together
-}
multiply : ComplexNumberCartesian Float -> ComplexNumberCartesian Float -> ComplexNumberCartesian Float
multiply complexNumberOne complexNumberTwo =
    Internal.ComplexNumbers.multiplyPolar (convertFromCartesianToPolar complexNumberOne) (convertFromCartesianToPolar complexNumberTwo)
        |> convertFromPolarToCartesian



--ComplexNumberCartesian (Real (realOne * realTwo - imaginaryOne * imaginaryTwo)) (Imaginary (realOne * imaginaryTwo + realTwo * imaginaryOne))


productEmpty : ComplexNumberCartesian number
productEmpty =
    one


{-| Monoidally multiply two complex numbers together
-}
product : Typeclasses.Classes.Monoid.Monoid (ComplexNumberCartesian Float)
product =
    Typeclasses.Classes.Monoid.semigroupAndIdentity (Typeclasses.Classes.Semigroup.prepend multiply) productEmpty


{-| Subtract two complex numbers together
-}
subtract : ComplexNumberCartesian number -> ComplexNumberCartesian number -> ComplexNumberCartesian number
subtract complexNumberOne complexNumberTwo =
    liftCartesian (-) complexNumberOne complexNumberTwo


{-| Divide two complex numbers together
-}
divide : ComplexNumberCartesian Float -> ComplexNumberCartesian Float -> ComplexNumberCartesian Float
divide complexNumberDividend complexNumberCartesianDivisor =
    Internal.ComplexNumbers.dividePolar (convertFromCartesianToPolar complexNumberDividend) (convertFromCartesianToPolar complexNumberCartesianDivisor)
        |> convertFromPolarToCartesian


{-| Calculate the modulus of a complex number
-}
modulus : ComplexNumberCartesian Float -> Float
modulus (ComplexNumberCartesian (Real realTwo) (Imaginary imaginaryTwo)) =
    (realTwo ^ 2 + imaginaryTwo ^ 2)
        |> sqrt


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
equalImplementation : ComplexNumberCartesian Float -> ComplexNumberCartesian Float -> Bool
equalImplementation (ComplexNumberCartesian (Real realOne) (Imaginary imaginaryOne)) (ComplexNumberCartesian (Real realTwo) (Imaginary imaginaryTwo)) =
    Float.Extra.equalWithin 0.000000001 realOne realTwo && Float.Extra.equalWithin 0.000000001 imaginaryOne imaginaryTwo


{-| Calculate a complex number raised to a power
-}
power : Int -> ComplexNumberCartesian Float -> ComplexNumberCartesian Float
power n complexNumber =
    Internal.ComplexNumbers.power (toFloat n) (convertFromCartesianToPolar complexNumber)
        |> convertFromPolarToCartesian


{-| `Equal` type for `ComplexNumber`.
-}
complexNumberEqual : Typeclasses.Classes.Equality.Equality (ComplexNumberCartesian Float)
complexNumberEqual =
    Typeclasses.Classes.Equality.eq equalImplementation


{-| Compare two ComplexNumbers for equality
-}
equal : ComplexNumberCartesian Float -> ComplexNumberCartesian Float -> Bool
equal =
    complexNumberEqual.eq


{-| Print ComplexNumber
-}
print : ComplexNumberCartesian Float -> String
print (ComplexNumberCartesian (Real real) (Imaginary imaginary)) =
    "ComplexNumberCartesian Real " ++ String.fromFloat real ++ " Imaginary " ++ String.fromFloat imaginary


{-| Read ComplexNumber
-}
read : String -> Result (List Parser.DeadEnd) (ComplexNumberCartesian Float)
read vectorString =
    Parser.run parseComplexNumber vectorString


parseComplexNumber : Parser.Parser (ComplexNumberCartesian Float)
parseComplexNumber =
    Parser.succeed ComplexNumberCartesian
        |. Parser.keyword "ComplexNumberCartesian"
        |. Parser.spaces
        |= parseReal
        |. Parser.spaces
        |= parseImaginary


parseReal : Parser.Parser (Real Float)
parseReal =
    Parser.succeed Real
        |. Parser.keyword "Real"
        |. Parser.spaces
        |= positiveOrNegativeFloat


parseImaginary : Parser.Parser (Imaginary Float)
parseImaginary =
    Parser.succeed Imaginary
        |. Parser.keyword "Imaginary"
        |. Parser.spaces
        |= positiveOrNegativeFloat


float : Parser.Parser Float
float =
    Parser.number
        { int = Just toFloat
        , hex = Nothing
        , octal = Nothing
        , binary = Nothing
        , float = Just identity
        }


positiveOrNegativeFloat : Parser.Parser Float
positiveOrNegativeFloat =
    Parser.oneOf
        [ Parser.succeed negate
            |. Parser.symbol "-"
            |= float
        , float
        ]


{-| Field for Complex numbers
-}
complexField : Field.Field (ComplexNumberCartesian Float)
complexField =
    { zero = zero
    , one = one
    , add = add
    , subtract = subtract
    , multiply = multiply
    , divide = divide
    , power = power
    }

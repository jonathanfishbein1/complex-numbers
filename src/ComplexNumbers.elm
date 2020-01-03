module ComplexNumbers exposing
    ( Real(..)
    , Imaginary(..)
    , ComplexNumber(..)
    , i
    , zero
    , one
    , real
    , imaginary
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
    , map
    , pure
    , andMap
    , andThen
    , equal
    , power
    , complexField
    , parseComplexNumber
    , read
    , print
    , euler
    )

{-| A module for complex numbers


# Types

@docs Real
@docs Imaginary
@docs ComplexNumber


# Arithmetic operations on complex numbers

@docs i
@docs zero
@docs one
@docs real
@docs imaginary
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
@docs map
@docs pure
@docs andMap
@docs andThen
@docs equal
@docs power
@docs complexField

@docs parseComplexNumber
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
type ComplexNumber a
    = ComplexNumber (Real a) (Imaginary a)


{-| zero
-}
zero : ComplexNumber number
zero =
    ComplexNumber (Real 0) (Imaginary 0)


{-| one
-}
one : ComplexNumber number
one =
    ComplexNumber (Real 1) (Imaginary 0)


{-| The number i
-}
i : ComplexNumber number
i =
    ComplexNumber (Real 0) (Imaginary 1)


{-| Extracts the real part of a complex number
-}
real : ComplexNumber a -> a
real (ComplexNumber (Real rl) _) =
    rl


{-| Extracts the imaginary part of a complex number
-}
imaginary : ComplexNumber a -> a
imaginary (ComplexNumber _ (Imaginary imag)) =
    imag


{-| Add two complex numbers together
-}
add :
    ComplexNumber number
    -> ComplexNumber number
    -> ComplexNumber number
add complexOne complexTwo =
    map2 (+) complexOne complexTwo


sumEmpty : ComplexNumber number
sumEmpty =
    ComplexNumber (Real 0) (Imaginary 0)


{-| Monoidally add two complex numbers together
-}
sum : Typeclasses.Classes.Monoid.Monoid (ComplexNumber number)
sum =
    Typeclasses.Classes.Monoid.semigroupAndIdentity
        (Typeclasses.Classes.Semigroup.prepend add)
        sumEmpty


{-| Multiply two complex numbers together
-}
multiply :
    ComplexNumber Float
    -> ComplexNumber Float
    -> ComplexNumber Float
multiply complexNumberOne complexNumberTwo =
    Internal.ComplexNumbers.multiply
        (convertFromCartesianToPolar complexNumberOne)
        (convertFromCartesianToPolar complexNumberTwo)
        |> convertFromPolarToCartesian


productEmpty : ComplexNumber number
productEmpty =
    one


{-| Monoidally multiply two complex numbers together
-}
product : Typeclasses.Classes.Monoid.Monoid (ComplexNumber Float)
product =
    Typeclasses.Classes.Monoid.semigroupAndIdentity
        (Typeclasses.Classes.Semigroup.prepend multiply)
        productEmpty


{-| Subtract two complex numbers together
-}
subtract :
    ComplexNumber number
    -> ComplexNumber number
    -> ComplexNumber number
subtract complexNumberOne complexNumberTwo =
    map2 (-) complexNumberOne complexNumberTwo


{-| Divide two complex numbers together
-}
divide :
    ComplexNumber Float
    -> ComplexNumber Float
    -> ComplexNumber Float
divide complexNumberDividend complexNumberCartesianDivisor =
    Internal.ComplexNumbers.divide
        (convertFromCartesianToPolar complexNumberDividend)
        (convertFromCartesianToPolar complexNumberCartesianDivisor)
        |> convertFromPolarToCartesian


{-| Calculate the modulus of a complex number
-}
modulus : ComplexNumber Float -> Float
modulus (ComplexNumber (Real rl) (Imaginary imag)) =
    (rl ^ 2 + imag ^ 2)
        |> sqrt


{-| Calculate the conjugate of a complex number
-}
conjugate : ComplexNumber number -> ComplexNumber number
conjugate (ComplexNumber rl (Imaginary imaginaryOne)) =
    ComplexNumber rl (Imaginary -imaginaryOne)


{-| Convert from the Cartesian representation of a complex number to the polar representation
-}
convertFromCartesianToPolar :
    ComplexNumber Float
    -> Internal.ComplexNumbers.ComplexNumber Float
convertFromCartesianToPolar (ComplexNumber (Real rl) (Imaginary imag)) =
    let
        polar =
            toPolar ( rl, imag )
    in
    Internal.ComplexNumbers.ComplexNumber
        (Internal.ComplexNumbers.Modulus <| Tuple.first polar)
        (Internal.ComplexNumbers.Theta <| Tuple.second polar)


{-| Convert from the polar representation of a complex number to the Cartesian representation
-}
convertFromPolarToCartesian :
    Internal.ComplexNumbers.ComplexNumber Float
    -> ComplexNumber Float
convertFromPolarToCartesian (Internal.ComplexNumbers.ComplexNumber (Internal.ComplexNumbers.Modulus ro) (Internal.ComplexNumbers.Theta theta)) =
    let
        cartesian =
            fromPolar ( ro, theta )
    in
    ComplexNumber (Real <| Tuple.first cartesian) (Imaginary <| Tuple.second cartesian)


{-| Map over a complex number
-}
map : (a -> b) -> ComplexNumber a -> ComplexNumber b
map f (ComplexNumber (Real realOne) (Imaginary imaginaryOne)) =
    ComplexNumber (Real <| f realOne) (Imaginary <| f imaginaryOne)


{-| Place a value in the minimal Complex Number Cartesian context
-}
pure : a -> ComplexNumber a
pure a =
    ComplexNumber (Real a) (Imaginary a)


{-| Apply for Complex Number Cartesian representaiton applicative
-}
andMap :
    ComplexNumber a
    -> ComplexNumber (a -> b)
    -> ComplexNumber b
andMap (ComplexNumber (Real rl) (Imaginary imag)) (ComplexNumber (Real fReal) (Imaginary fImaginary)) =
    ComplexNumber (Real <| fReal rl) (Imaginary <| fImaginary imag)


{-| Monadic bind for Complex Number Cartesian representaiton
-}
andThen :
    ComplexNumber a
    -> (a -> ComplexNumber b)
    -> ComplexNumber b
andThen (ComplexNumber (Real previousReal) (Imaginary previousImaginary)) f =
    ComplexNumber
        (Real <| real <| f previousReal)
        (Imaginary <| imaginary <| f previousImaginary)


{-| Lift a binary function to work with complex numbers
-}
map2 :
    (a -> b -> c)
    -> ComplexNumber a
    -> ComplexNumber b
    -> ComplexNumber c
map2 f a b =
    andMap b (map f a)


{-| Equality of Complex Numbers
-}
equalImplementation :
    ComplexNumber Float
    -> ComplexNumber Float
    -> Bool
equalImplementation (ComplexNumber (Real realOne) (Imaginary imaginaryOne)) (ComplexNumber (Real realTwo) (Imaginary imaginaryTwo)) =
    Float.Extra.equalWithin 0.000000001 realOne realTwo
        && Float.Extra.equalWithin 0.000000001 imaginaryOne imaginaryTwo


{-| Calculate a complex number raised to a power
-}
power : Float -> ComplexNumber Float -> ComplexNumber Float
power n complexNumber =
    Internal.ComplexNumbers.power n (convertFromCartesianToPolar complexNumber)
        |> convertFromPolarToCartesian


{-| `Equal` type for `ComplexNumber`.
-}
complexNumberEqual : Typeclasses.Classes.Equality.Equality (ComplexNumber Float)
complexNumberEqual =
    Typeclasses.Classes.Equality.eq equalImplementation


{-| Compare two ComplexNumbers for equality
-}
equal : ComplexNumber Float -> ComplexNumber Float -> Bool
equal =
    complexNumberEqual.eq


{-| Print ComplexNumber
-}
print : ComplexNumber Float -> String
print (ComplexNumber (Real rl) (Imaginary imag)) =
    "ComplexNumber Real "
        ++ String.fromFloat rl
        ++ " Imaginary "
        ++ String.fromFloat imag


{-| Read ComplexNumber
-}
read : String -> Result (List Parser.DeadEnd) (ComplexNumber Float)
read vectorString =
    Parser.run parseComplexNumber vectorString


{-| Parse ComplexNumber
-}
parseComplexNumber : Parser.Parser (ComplexNumber Float)
parseComplexNumber =
    Parser.succeed ComplexNumber
        |. Parser.keyword "ComplexNumber"
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
complexField : Field.Field (ComplexNumber Float)
complexField =
    { zero = zero
    , one = one
    , add = add
    , subtract = subtract
    , multiply = multiply
    , divide = divide
    , power = power
    , negate = multiply (ComplexNumber (Real -1) (Imaginary 0))
    }


{-| Euler's equation
-}
euler : Float -> ComplexNumber Float
euler theta =
    ComplexNumber (Real <| Basics.cos theta) (Imaginary <| Basics.sin theta)

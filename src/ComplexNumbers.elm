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
    , multiply
    , subtract
    , divide
    , modulus
    , conjugate
    , power
    , convertFromCartesianToPolar
    , convertFromPolarToCartesian
    , euler
    , complexSumSemigroup, complexProductSemigroup, complexSumCommutativeSemigroup, complexProductCommutativeSemigroup
    , complexSumMonoid, complexProductMonoid, complexSumCommutativeMonoid, complexProductCommutativeMonoid
    , complexSumGroup, complexProductGroup, complexAbelianGroup
    , complexRing, complexDivisionRing, complexCommutativeRing, complexCommutativeDivisionRing
    , complexField
    , map
    , pure
    , andMap
    , andThen
    , equal
    , parseComplexNumber
    , read
    , print
    , printiNotation
    , printiNotationWithRounding
    )

{-| A module for complex numbers


# Types

@docs Real
@docs Imaginary
@docs ComplexNumber


# Values

@docs i
@docs zero
@docs one


# Arithmetic operations on complex numbers

@docs real
@docs imaginary
@docs add
@docs multiply
@docs subtract
@docs divide
@docs modulus
@docs conjugate
@docs power
@docs convertFromCartesianToPolar
@docs convertFromPolarToCartesian
@docs euler


# Semigroup, Monoid, Group, Ring, Field, Functor, Applicative Functor, and Monad

@docs complexSumSemigroup, complexProductSemigroup, complexSumCommutativeSemigroup, complexProductCommutativeSemigroup
@docs complexSumMonoid, complexProductMonoid, complexSumCommutativeMonoid, complexProductCommutativeMonoid
@docs complexSumGroup, complexProductGroup, complexAbelianGroup
@docs complexRing, complexDivisionRing, complexCommutativeRing, complexCommutativeDivisionRing
@docs complexField
@docs map
@docs pure
@docs andMap
@docs andThen

#Equality

@docs equal


# Read and Print

@docs parseComplexNumber
@docs read
@docs print
@docs printiNotation
@docs printiNotationWithRounding

-}

import AbelianGroup
import CommutativeDivisionRing
import CommutativeMonoid
import CommutativeRing
import CommutativeSemigroup
import DivisionRing
import Field
import Float.Extra
import Group
import Internal.ComplexNumbers
import Monoid
import Parser exposing ((|.), (|=))
import Ring
import Round
import Semigroup
import Typeclasses.Classes.Equality



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
    zero


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
    (a -> ComplexNumber b)
    -> ComplexNumber a
    -> ComplexNumber b
andThen f (ComplexNumber (Real previousReal) (Imaginary previousImaginary)) =
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


{-| Print ComplexNumber i notation with rounding function
-}
printiNotationWithRounding : (Float -> String) -> ComplexNumber Float -> String
printiNotationWithRounding toString (ComplexNumber (Real rl) (Imaginary imag)) =
    (if rl < 0 then
        "-"

     else
        "+"
    )
        ++ toString (Basics.abs rl)
        ++ (if imag < 0 then
                "-"

            else
                "+"
           )
        ++ toString (Basics.abs imag)
        ++ "i"


{-| Print ComplexNumber i notation with two decimal places
-}
printiNotation : ComplexNumber Float -> String
printiNotation =
    printiNotationWithRounding (Round.round 2)


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


{-| Semigroup for Complex Numbers with addition as the operation
-}
complexSumSemigroup : Semigroup.Semigroup (ComplexNumber number)
complexSumSemigroup =
    add


{-| Semigroup for Complex Numbers with addition as the operation
-}
complexProductSemigroup : Semigroup.Semigroup (ComplexNumber Float)
complexProductSemigroup =
    multiply


{-| Semigroup for Complex Numbers with addition as the operation
-}
complexSumCommutativeSemigroup : CommutativeSemigroup.CommutativeSemigroup (ComplexNumber number)
complexSumCommutativeSemigroup =
    CommutativeSemigroup.CommutativeSemigroup complexSumSemigroup


{-| Semigroup for Complex Numbers with multiplicatoin as the operation
-}
complexProductCommutativeSemigroup : CommutativeSemigroup.CommutativeSemigroup (ComplexNumber Float)
complexProductCommutativeSemigroup =
    CommutativeSemigroup.CommutativeSemigroup complexProductSemigroup


{-| Monoid for Complex Numbers with addition as the operation
-}
complexSumMonoid : Monoid.Monoid (ComplexNumber number)
complexSumMonoid =
    Monoid.semigroupAndIdentity complexSumSemigroup sumEmpty


{-| Monoid for Complex Numbers with multiplication as the operation
-}
complexProductMonoid : Monoid.Monoid (ComplexNumber Float)
complexProductMonoid =
    Monoid.semigroupAndIdentity complexProductSemigroup productEmpty


{-| Monoid for Complex Numbers with addition as the operation
-}
complexSumCommutativeMonoid : CommutativeMonoid.CommutativeMonoid (ComplexNumber number)
complexSumCommutativeMonoid =
    CommutativeMonoid.CommutativeMonoid complexSumMonoid


{-| Monoid for Complex Numbers with multiplication as the operation
-}
complexProductCommutativeMonoid : CommutativeMonoid.CommutativeMonoid (ComplexNumber Float)
complexProductCommutativeMonoid =
    CommutativeMonoid.CommutativeMonoid complexProductMonoid


{-| Group for Complex Numbers with addition as the operation
-}
complexSumGroup : Group.Group (ComplexNumber number)
complexSumGroup =
    { monoid = complexSumMonoid, inverse = \(ComplexNumber (Real x) (Imaginary y)) -> ComplexNumber (Real -x) (Imaginary -y) }


{-| Group for Complex Numbers with multiplication as the operation
-}
complexProductGroup : Group.Group (ComplexNumber Float)
complexProductGroup =
    { monoid = complexProductMonoid, inverse = \(ComplexNumber (Real x) (Imaginary y)) -> divide one (ComplexNumber (Real x) (Imaginary y)) }


{-| Group for Complex Numbers with addition as the operation
-}
complexAbelianGroup : AbelianGroup.AbelianGroup (ComplexNumber number)
complexAbelianGroup =
    AbelianGroup.AbelianGroup complexSumGroup


{-| Ring for Complex Numbers
-}
complexRing : Ring.Ring (ComplexNumber Float)
complexRing =
    { addition = complexAbelianGroup, multiplication = complexProductMonoid }


{-| Division Ring for Complex Numbers
-}
complexDivisionRing : DivisionRing.DivisionRing (ComplexNumber Float)
complexDivisionRing =
    { addition = complexAbelianGroup, multiplication = complexProductGroup }


{-| Commutative Ring for Complex Numbers
-}
complexCommutativeRing : CommutativeRing.CommutativeRing (ComplexNumber Float)
complexCommutativeRing =
    CommutativeRing.CommutativeRing complexRing


{-| Commutative Division Ring for Complex Numbers
-}
complexCommutativeDivisionRing : CommutativeDivisionRing.CommutativeDivisionRing (ComplexNumber Float)
complexCommutativeDivisionRing =
    CommutativeDivisionRing.CommutativeDivisionRing complexDivisionRing


{-| Field for Complex Numbers
-}
complexField : Field.Field (ComplexNumber Float)
complexField =
    Field.Field complexCommutativeDivisionRing


{-| Euler's equation
-}
euler : Float -> ComplexNumber Float
euler theta =
    ComplexNumber (Real <| Basics.cos theta) (Imaginary <| Basics.sin theta)

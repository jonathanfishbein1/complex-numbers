module ComplexNumbers exposing
    ( ComplexNumber(..)
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
    , imaginaryAxisReflection
    )

{-| A module for complex numbers


# Types

@docs Real.Real
@docs Imaginary.Imaginary
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
import Imaginary
import Internal.ComplexNumbers
import Monoid
import Parser exposing ((|.), (|=))
import Real
import Ring
import Round
import Semigroup
import Typeclasses.Classes.Equality



-- Types


{-| Cartesian representation of a complex number
-}
type ComplexNumber a
    = ComplexNumber (Real.Real a) (Imaginary.Imaginary a)


{-| zero
-}
zero : ComplexNumber number
zero =
    ComplexNumber Real.zero Imaginary.zero


{-| one
-}
one : ComplexNumber number
one =
    ComplexNumber Real.one Imaginary.zero


{-| The number i
-}
i : ComplexNumber number
i =
    ComplexNumber Real.zero Imaginary.i


{-| Extracts the real part of a complex number
-}
real : ComplexNumber a -> a
real (ComplexNumber rl _) =
    Real.real rl


{-| Extracts the imaginary part of a complex number
-}
imaginary : ComplexNumber a -> a
imaginary (ComplexNumber _ imag) =
    Imaginary.imaginary imag


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
modulus (ComplexNumber (Real.Real rl) (Imaginary.Imaginary imag)) =
    (rl ^ 2 + imag ^ 2)
        |> sqrt


{-| Calculate the conjugate of a complex number
-}
conjugate : ComplexNumber number -> ComplexNumber number
conjugate (ComplexNumber rl img) =
    ComplexNumber rl (Imaginary.negate img)


{-| Calculate the conjugate of a complex number
-}
imaginaryAxisReflection : ComplexNumber number -> ComplexNumber number
imaginaryAxisReflection (ComplexNumber rl img) =
    ComplexNumber (Real.negate rl) img


{-| Convert from the Cartesian representation of a complex number to the polar representation
-}
convertFromCartesianToPolar :
    ComplexNumber Float
    -> Internal.ComplexNumbers.ComplexNumber Float
convertFromCartesianToPolar (ComplexNumber (Real.Real rl) (Imaginary.Imaginary imag)) =
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
    ComplexNumber (Real.Real <| Tuple.first cartesian) (Imaginary.Imaginary <| Tuple.second cartesian)


{-| Map over a complex number
-}
map : (a -> b) -> ComplexNumber a -> ComplexNumber b
map f (ComplexNumber rl img) =
    ComplexNumber (Real.map f rl) (Imaginary.map f img)


{-| Place a value in the minimal Complex Number Cartesian context
-}
pure : a -> ComplexNumber a
pure a =
    ComplexNumber (Real.pure a) (Imaginary.pure a)


{-| Apply for Complex Number Cartesian representaiton applicative
-}
andMap :
    ComplexNumber a
    -> ComplexNumber (a -> b)
    -> ComplexNumber b
andMap (ComplexNumber rl imag) (ComplexNumber fReal fImaginary) =
    ComplexNumber (Real.andMap rl fReal) (Imaginary.andMap imag fImaginary)


{-| Monadic bind for Complex Number Cartesian representaiton
-}
andThen :
    (a -> ComplexNumber b)
    -> ComplexNumber a
    -> ComplexNumber b
andThen f (ComplexNumber (Real.Real previousReal) (Imaginary.Imaginary previousImaginary)) =
    ComplexNumber
        (Real.Real <| real <| f previousReal)
        (Imaginary.Imaginary <| imaginary <| f previousImaginary)


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
equalImplementation (ComplexNumber (Real.Real realOne) (Imaginary.Imaginary imaginaryOne)) (ComplexNumber (Real.Real realTwo) (Imaginary.Imaginary imaginaryTwo)) =
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
print (ComplexNumber (Real.Real rl) (Imaginary.Imaginary imag)) =
    "ComplexNumber Real.Real "
        ++ String.fromFloat rl
        ++ " Imaginary.Imaginary "
        ++ String.fromFloat imag


{-| Print ComplexNumber i notation with rounding function
-}
printiNotationWithRounding : (Float -> String) -> ComplexNumber Float -> String
printiNotationWithRounding toString (ComplexNumber (Real.Real rl) (Imaginary.Imaginary imag)) =
    (if rl < 0 then
        "−"

     else
        "+"
    )
        ++ toString (Basics.abs rl)
        ++ (if imag < 0 then
                "−"

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


parseReal : Parser.Parser (Real.Real Float)
parseReal =
    Parser.succeed Real.Real
        |. Parser.keyword "Real.Real"
        |. Parser.spaces
        |= positiveOrNegativeFloat


parseImaginary : Parser.Parser (Imaginary.Imaginary Float)
parseImaginary =
    Parser.succeed Imaginary.Imaginary
        |. Parser.keyword "Imaginary.Imaginary"
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
    { monoid = complexSumMonoid, inverse = \(ComplexNumber (Real.Real x) (Imaginary.Imaginary y)) -> ComplexNumber (Real.Real -x) (Imaginary.Imaginary -y) }


{-| Group for Complex Numbers with multiplication as the operation
-}
complexProductGroup : Group.Group (ComplexNumber Float)
complexProductGroup =
    { monoid = complexProductMonoid, inverse = \(ComplexNumber (Real.Real x) (Imaginary.Imaginary y)) -> divide one (ComplexNumber (Real.Real x) (Imaginary.Imaginary y)) }


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
    ComplexNumber (Real.Real <| Basics.cos theta) (Imaginary.Imaginary <| Basics.sin theta)

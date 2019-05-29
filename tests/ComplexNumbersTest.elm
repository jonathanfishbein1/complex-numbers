module ComplexNumbersTest exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Internal.ComplexNumbers
import Monoid
import Test


suite : Test.Test
suite =
    Test.describe "The ComplexNumbers module"
        [ Test.fuzz2 Fuzz.float Fuzz.float "tests ComplexNumbers add" <|
            \real imaginary ->
                let
                    testValue =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real real) (ComplexNumbers.Imaginary imaginary)

                    expected =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real <| 2 * real) (ComplexNumbers.Imaginary <| 2 * imaginary)
                in
                ComplexNumbers.add testValue testValue
                    |> Expect.equal expected
        , Test.fuzz2 Fuzz.float Fuzz.float "tests ComplexNumbers zero is identity" <|
            \real imaginary ->
                let
                    testValue =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real real) (ComplexNumbers.Imaginary imaginary)
                in
                ComplexNumbers.add testValue ComplexNumbers.zero
                    |> Expect.equal testValue
        , Test.fuzz2 Fuzz.float Fuzz.float "tests ComplexNumberCartesian empty or identity value for sum" <|
            \real imaginary ->
                let
                    expected =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real real) (ComplexNumbers.Imaginary imaginary)
                in
                Monoid.append ComplexNumbers.sum expected (Monoid.empty ComplexNumbers.sum)
                    |> Expect.equal expected
        , Test.fuzz3 Fuzz.int Fuzz.int Fuzz.int "tests monoidally add" <|
            \one two three ->
                let
                    a =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary two)

                    b =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real two) (ComplexNumbers.Imaginary three)

                    c =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary three)

                    expected =
                        ComplexNumbers.add (ComplexNumbers.add a b) c

                    listOfMonoids =
                        [ a, b, c ]
                in
                Monoid.concat ComplexNumbers.sum listOfMonoids
                    |> Expect.equal expected
        , Test.fuzz3 Fuzz.float Fuzz.float Fuzz.float "tests ComplexNumbers addition is commutative" <|
            \one two three ->
                let
                    a =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary two)

                    b =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real two) (ComplexNumbers.Imaginary three)

                    testValueOne =
                        ComplexNumbers.add a b

                    testValueTwo =
                        ComplexNumbers.add b a
                in
                testValueOne
                    |> Expect.equal testValueTwo
        , Test.fuzz3 Fuzz.int Fuzz.int Fuzz.int "tests ComplexNumbers addition is associative" <|
            \one two three ->
                let
                    a =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary two)

                    b =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real two) (ComplexNumbers.Imaginary three)

                    c =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary three)

                    testValueOne =
                        ComplexNumbers.add (ComplexNumbers.add a b) c

                    testValueTwo =
                        ComplexNumbers.add a (ComplexNumbers.add b c)
                in
                testValueOne
                    |> Expect.equal testValueTwo
        , Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests ComplexNumberCartesian empty or identity value for product" <|
            \real imaginary ->
                let
                    expected =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real real) (ComplexNumbers.Imaginary imaginary)
                in
                Monoid.append ComplexNumbers.product expected (Monoid.empty ComplexNumbers.product)
                    |> Expect.equal expected
        , Test.fuzz3 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests monoidally product" <|
            \one two three ->
                let
                    a =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary two)

                    b =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real two) (ComplexNumbers.Imaginary three)

                    c =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary three)

                    expected =
                        ComplexNumbers.multiply (ComplexNumbers.multiply a b) c

                    listOfMonoids =
                        [ a, b, c ]
                in
                Monoid.concat ComplexNumbers.product listOfMonoids
                    |> Expect.equal expected
        , Test.fuzz3 Fuzz.float Fuzz.float Fuzz.float "tests ComplexNumbers multiplication is commutative" <|
            \one two three ->
                let
                    a =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary two)

                    b =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real two) (ComplexNumbers.Imaginary three)

                    testValueOne =
                        ComplexNumbers.multiply a b

                    testValueTwo =
                        ComplexNumbers.multiply b a
                in
                testValueOne
                    |> Expect.equal testValueTwo
        , Test.fuzz3 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests ComplexNumbers multiplication is associative" <|
            \one two three ->
                let
                    a =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary two)

                    b =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real two) (ComplexNumbers.Imaginary three)

                    c =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary three)

                    testValueOne =
                        ComplexNumbers.multiply (ComplexNumbers.multiply a b) c

                    testValueTwo =
                        ComplexNumbers.multiply a (ComplexNumbers.multiply b c)
                in
                testValueOne
                    |> Expect.equal testValueTwo
        , Test.fuzz3 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests ComplexNumbers multiplication distributes over addition" <|
            \one two three ->
                let
                    a =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary two)

                    b =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real two) (ComplexNumbers.Imaginary three)

                    c =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary three)

                    testValueOne =
                        ComplexNumbers.multiply a (ComplexNumbers.add b c)

                    testValueTwo =
                        ComplexNumbers.multiply a b
                            |> ComplexNumbers.add (ComplexNumbers.multiply a c)
                in
                testValueOne
                    |> Expect.equal testValueTwo
        , Test.fuzz2 Fuzz.int Fuzz.int "tests ComplexNumbers subtract" <|
            \real imaginary ->
                let
                    testValue =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real real) (ComplexNumbers.Imaginary imaginary)

                    expected =
                        testValue
                in
                ComplexNumbers.subtract testValue (Monoid.empty ComplexNumbers.sum)
                    |> Expect.equal expected
        , Test.fuzz3 Fuzz.float Fuzz.float Fuzz.float "tests ComplexNumbers division" <|
            \one two three ->
                let
                    dividend =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary two)

                    divisor =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real two) (ComplexNumbers.Imaginary three)

                    (ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real realDivisor) _) =
                        divisor

                    (ComplexNumbers.ComplexNumberCartesian _ (ComplexNumbers.Imaginary imaginaryDivisor)) =
                        divisor

                    squareOfModulus =
                        realDivisor ^ 2 + imaginaryDivisor ^ 2

                    quotient =
                        ComplexNumbers.divide dividend divisor
                in
                case round squareOfModulus of
                    0 ->
                        Expect.err quotient

                    _ ->
                        Expect.ok quotient
        , Test.fuzz2 Fuzz.float Fuzz.float "tests ComplexNumbers modulus" <|
            \one two ->
                let
                    number =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary two)

                    (ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real real) _) =
                        number

                    (ComplexNumbers.ComplexNumberCartesian _ (ComplexNumbers.Imaginary imaginary)) =
                        number

                    length =
                        real
                            ^ 2
                            + imaginary
                            ^ 2
                            |> sqrt
                in
                ComplexNumbers.modulus number
                    |> Expect.within (Expect.Absolute 0.000000001) length
        , Test.fuzz3 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests |c1||c2| = |c1c2|" <|
            \one two three ->
                let
                    numberOne =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary two)

                    numberTwo =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real two) (ComplexNumbers.Imaginary three)

                    lengthOne =
                        ComplexNumbers.modulus numberOne

                    lengthTwo =
                        ComplexNumbers.modulus numberTwo

                    productLengthOneLengthTwo =
                        lengthOne * lengthTwo

                    modulesOfProductOfNumberOneNumberTwo =
                        ComplexNumbers.multiply numberOne numberTwo
                            |> ComplexNumbers.modulus
                in
                Expect.within (Expect.Absolute 10) productLengthOneLengthTwo modulesOfProductOfNumberOneNumberTwo
        , Test.fuzz3 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests |c1 + c2| <= |c1| + |c2| (triangle inequality rule)" <|
            \one two three ->
                let
                    numberOne =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary two)

                    numberTwo =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real two) (ComplexNumbers.Imaginary three)

                    modulesOfSumOfNumberOneNumberTwo =
                        ComplexNumbers.add numberOne numberTwo
                            |> ComplexNumbers.modulus

                    modulusOne =
                        ComplexNumbers.modulus numberOne

                    modulusTwo =
                        ComplexNumbers.modulus numberTwo

                    sumLengthOneLengthTwo =
                        modulusOne + modulusTwo
                in
                modulesOfSumOfNumberOneNumberTwo |> Expect.atMost sumLengthOneLengthTwo
        , Test.fuzz2 Fuzz.float Fuzz.float "tests ComplexNumbers conjugate" <|
            \real imaginary ->
                let
                    testValue =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real real) (ComplexNumbers.Imaginary imaginary)

                    expected =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real <| real) (ComplexNumbers.Imaginary <| -imaginary)
                in
                ComplexNumbers.conjugate testValue
                    |> Expect.equal expected
        , Test.fuzz3 Fuzz.int Fuzz.int Fuzz.int "tests conjugation respects addition" <|
            \one two three ->
                let
                    numberOne =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary two)

                    numberTwo =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real two) (ComplexNumbers.Imaginary three)

                    conjugateOne =
                        ComplexNumbers.conjugate numberOne

                    conjugateTwo =
                        ComplexNumbers.conjugate numberTwo

                    sumOfconjugateOneconjugateTwo =
                        ComplexNumbers.add conjugateOne conjugateTwo

                    sumOfNumberOneNumberTwo =
                        ComplexNumbers.add numberOne numberTwo

                    conjugateOfsumOfNumberOneNumberTwo =
                        ComplexNumbers.conjugate sumOfNumberOneNumberTwo
                in
                Expect.equal sumOfconjugateOneconjugateTwo conjugateOfsumOfNumberOneNumberTwo
        , Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests complex conjugates multiplied equals modulus squared" <|
            \real imaginary ->
                let
                    testValue =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real real) (ComplexNumbers.Imaginary imaginary)

                    conjugate =
                        ComplexNumbers.conjugate testValue

                    producttestValueconjugate =
                        ComplexNumbers.multiply testValue conjugate
                            |> ComplexNumbers.modulus

                    expected =
                        ComplexNumbers.modulus testValue ^ 2
                in
                Expect.within (Expect.Absolute 0.000000001) producttestValueconjugate expected
        , Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests convertFromCartesianToPolar |> convertFromPolarToCartesian round trips" <|
            \real imaginary ->
                let
                    cartesianTestValue =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real real) (ComplexNumbers.Imaginary imaginary)

                    polarTestValue =
                        ComplexNumbers.convertFromCartesianToPolar cartesianTestValue

                    conversionResult =
                        ComplexNumbers.convertFromPolarToCartesian polarTestValue

                    result =
                        ComplexNumbers.equal cartesianTestValue conversionResult
                in
                Expect.true "Should be equal" result
        , Test.fuzz3 Fuzz.float Fuzz.float Fuzz.float "tests ComplexNumbers polar multiplication is commutative" <|
            \one two three ->
                let
                    a =
                        Internal.ComplexNumbers.ComplexNumberPolar (Internal.ComplexNumbers.Modulus one) (Internal.ComplexNumbers.Theta two)

                    b =
                        Internal.ComplexNumbers.ComplexNumberPolar (Internal.ComplexNumbers.Modulus two) (Internal.ComplexNumbers.Theta three)

                    testValueOne =
                        Internal.ComplexNumbers.multiplyPolar a b

                    testValueTwo =
                        Internal.ComplexNumbers.multiplyPolar b a
                in
                testValueOne
                    |> Expect.equal testValueTwo
        , Test.fuzz3 (Fuzz.intRange -10 10) (Fuzz.intRange -10 10) (Fuzz.intRange -10 10) "tests ComplexNumbers polar multiplication is associative" <|
            \one two three ->
                let
                    a =
                        Internal.ComplexNumbers.ComplexNumberPolar (Internal.ComplexNumbers.Modulus one) (Internal.ComplexNumbers.Theta two)

                    b =
                        Internal.ComplexNumbers.ComplexNumberPolar (Internal.ComplexNumbers.Modulus two) (Internal.ComplexNumbers.Theta three)

                    c =
                        Internal.ComplexNumbers.ComplexNumberPolar (Internal.ComplexNumbers.Modulus one) (Internal.ComplexNumbers.Theta three)

                    testValueOne =
                        Internal.ComplexNumbers.multiplyPolar (Internal.ComplexNumbers.multiplyPolar a b) c

                    testValueTwo =
                        Internal.ComplexNumbers.multiplyPolar a (Internal.ComplexNumbers.multiplyPolar b c)
                in
                testValueOne
                    |> Expect.equal testValueTwo
        , Test.fuzz3 Fuzz.float Fuzz.float Fuzz.float "tests ComplexNumbers polar division" <|
            \one two three ->
                let
                    dividend =
                        Internal.ComplexNumbers.ComplexNumberPolar (Internal.ComplexNumbers.Modulus one) (Internal.ComplexNumbers.Theta two)

                    divisor =
                        Internal.ComplexNumbers.ComplexNumberPolar (Internal.ComplexNumbers.Modulus two) (Internal.ComplexNumbers.Theta three)

                    (Internal.ComplexNumbers.ComplexNumberPolar (Internal.ComplexNumbers.Modulus modulusDivisor) _) =
                        divisor

                    quotient =
                        Internal.ComplexNumbers.dividePolar dividend divisor
                in
                case round modulusDivisor of
                    0 ->
                        Expect.err quotient

                    _ ->
                        Expect.ok quotient
        , Test.fuzz2 Fuzz.int Fuzz.int "tests pure f apply x equal map f x" <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary two)

                    f =
                        (*) 2

                    fMapX =
                        ComplexNumbers.mapCartesian f complexNumber

                    pureFApplyX =
                        ComplexNumbers.applyCartesian (ComplexNumbers.pureCartesian f) complexNumber
                in
                pureFApplyX
                    |> Expect.equal fMapX
        , Test.fuzz2 Fuzz.int Fuzz.int "tests apply + equal to add" <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary two)

                    doubleComplexNumber =
                        ComplexNumbers.add complexNumber complexNumber

                    f =
                        (+)

                    fMapX =
                        ComplexNumbers.mapCartesian f complexNumber

                    pureFApplyX =
                        ComplexNumbers.applyCartesian fMapX complexNumber
                in
                pureFApplyX
                    |> Expect.equal doubleComplexNumber
        , Test.fuzz2 Fuzz.int Fuzz.int "tests pure f apply x equal map f x polar" <|
            \one two ->
                let
                    complexNumber =
                        Internal.ComplexNumbers.ComplexNumberPolar (Internal.ComplexNumbers.Modulus one) (Internal.ComplexNumbers.Theta two)

                    f =
                        (*) 2

                    fMapX =
                        Internal.ComplexNumbers.mapPolar f complexNumber

                    pureFApplyX =
                        Internal.ComplexNumbers.applyPolar (Internal.ComplexNumbers.purePolar f) complexNumber
                in
                pureFApplyX
                    |> Expect.equal fMapX
        , Test.fuzz2 Fuzz.int Fuzz.int "tests apply - equal to add" <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary two)

                    f =
                        (-)

                    fMapX =
                        ComplexNumbers.mapCartesian f complexNumber

                    pureFApplyX =
                        ComplexNumbers.applyCartesian fMapX complexNumber
                in
                pureFApplyX
                    |> Expect.equal (ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 0) (ComplexNumbers.Imaginary 0))
        , Test.fuzz2 Fuzz.int Fuzz.int "tests pure f apply x equal map f x multiply polar" <|
            \one two ->
                let
                    complexNumber =
                        Internal.ComplexNumbers.ComplexNumberPolar (Internal.ComplexNumbers.Modulus one) (Internal.ComplexNumbers.Theta two)

                    f _ =
                        Internal.ComplexNumbers.ComplexNumberPolar (Internal.ComplexNumbers.Modulus (*)) (Internal.ComplexNumbers.Theta (+))

                    fMapX =
                        Internal.ComplexNumbers.mapPolar f complexNumber

                    pureFApplyX =
                        Internal.ComplexNumbers.applyPolar (Internal.ComplexNumbers.purePolar f) complexNumber
                in
                pureFApplyX
                    |> Expect.equal fMapX
        , Test.fuzz2 Fuzz.int Fuzz.int "tests pure f apply x equal map f x multiply divide" <|
            \one two ->
                let
                    complexNumber =
                        Internal.ComplexNumbers.ComplexNumberPolar (Internal.ComplexNumbers.Modulus one) (Internal.ComplexNumbers.Theta two)

                    f _ =
                        Internal.ComplexNumbers.ComplexNumberPolar (Internal.ComplexNumbers.Modulus (/)) (Internal.ComplexNumbers.Theta (-))

                    fMapX =
                        Internal.ComplexNumbers.mapPolar f complexNumber

                    pureFApplyX =
                        Internal.ComplexNumbers.applyPolar (Internal.ComplexNumbers.purePolar f) complexNumber
                in
                pureFApplyX
                    |> Expect.equal fMapX
        , Test.fuzz2 Fuzz.int Fuzz.int "tests monad left identity" <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.pureCartesian one

                    f x =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real x) (ComplexNumbers.Imaginary x)

                    bindResult =
                        ComplexNumbers.bindCartesian complexNumber f

                    fOfX =
                        f one
                in
                bindResult
                    |> Expect.equal fOfX
        , Test.fuzz2 Fuzz.int Fuzz.int "tests monad right identity" <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary two)

                    bindResult =
                        ComplexNumbers.bindCartesian complexNumber ComplexNumbers.pureCartesian
                in
                bindResult
                    |> Expect.equal complexNumber
        , Test.fuzz2 Fuzz.int Fuzz.int "tests monad left identity polar" <|
            \one two ->
                let
                    complexNumber =
                        Internal.ComplexNumbers.purePolar one

                    f x =
                        Internal.ComplexNumbers.ComplexNumberPolar (Internal.ComplexNumbers.Modulus x) (Internal.ComplexNumbers.Theta x)

                    bindResult =
                        Internal.ComplexNumbers.bindPolar complexNumber f

                    fOfX =
                        f one
                in
                bindResult
                    |> Expect.equal fOfX
        , Test.fuzz2 Fuzz.int Fuzz.int "tests monad right identity polar" <|
            \one two ->
                let
                    complexNumber =
                        Internal.ComplexNumbers.ComplexNumberPolar (Internal.ComplexNumbers.Modulus one) (Internal.ComplexNumbers.Theta two)

                    bindResult =
                        Internal.ComplexNumbers.bindPolar complexNumber Internal.ComplexNumbers.purePolar
                in
                bindResult
                    |> Expect.equal complexNumber
        , Test.fuzz2 Fuzz.int Fuzz.int "tests power" <|
            \one two ->
                let
                    complexNumber =
                        Internal.ComplexNumbers.ComplexNumberPolar (Internal.ComplexNumbers.Modulus one) (Internal.ComplexNumbers.Theta two)

                    powerResult =
                        Internal.ComplexNumbers.power 2 complexNumber

                    productResult =
                        Internal.ComplexNumbers.multiplyPolar complexNumber complexNumber
                in
                powerResult
                    |> Expect.equal productResult
        ]

module ComplexNumbersTest exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
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
        , Test.fuzz2 Fuzz.float Fuzz.float "tests ComplexNumberCartesian empty or identity value" <|
            \real imaginary ->
                let
                    expected =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real real) (ComplexNumbers.Imaginary imaginary)

                    empty =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 0) (ComplexNumbers.Imaginary 0)
                in
                Monoid.append ComplexNumbers.sum expected empty
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
        , Test.fuzz3 (Fuzz.intRange -10 10) (Fuzz.intRange -10 10) (Fuzz.intRange -10 10) "tests monoidally product" <|
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
        , Test.fuzz3 (Fuzz.intRange -10 10) (Fuzz.intRange -10 10) (Fuzz.intRange -10 10) "tests ComplexNumbers multiplication is associative" <|
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
        , Test.fuzz3 (Fuzz.intRange -10 10) (Fuzz.intRange -10 10) (Fuzz.intRange -10 10) "tests ComplexNumbers multiplication distributes over addition" <|
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

                    (ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real realAfterConversion) (ComplexNumbers.Imaginary imaginaryAfterConv)) =
                        ComplexNumbers.convertFromPolarToCartesian polarTestValue
                in
                Expect.within (Expect.Absolute 0.000000001) real realAfterConversion
        ]

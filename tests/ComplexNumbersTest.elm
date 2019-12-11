module ComplexNumbersTest exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Internal.ComplexNumbers
import Test


suite : Test.Test
suite =
    Test.describe "The ComplexNumbers module"
        [ Test.fuzz2 Fuzz.float Fuzz.float "tests ComplexNumbers modulus" <|
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
        , Test.fuzz3 Fuzz.int Fuzz.int Fuzz.int "tests conjugation respects multiplication" <|
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

                    productOfconjugateOneconjugateTwo =
                        ComplexNumbers.add conjugateOne conjugateTwo

                    productOfNumberOneNumberTwo =
                        ComplexNumbers.add numberOne numberTwo

                    conjugateOfproductOfNumberOneNumberTwo =
                        ComplexNumbers.conjugate productOfNumberOneNumberTwo
                in
                Expect.equal productOfconjugateOneconjugateTwo conjugateOfproductOfNumberOneNumberTwo
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
        , Test.test "tests ComplexNumbers polar division" <|
            \_ ->
                let
                    complexNumberDividend =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 2) (ComplexNumbers.Imaginary 0)
                            |> ComplexNumbers.convertFromCartesianToPolar

                    complexNumberDivisor =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 1) (ComplexNumbers.Imaginary 1)
                            |> ComplexNumbers.convertFromCartesianToPolar

                    quotient =
                        Debug.log "quotient " <| Internal.ComplexNumbers.dividePolar complexNumberDividend complexNumberDivisor

                    quotientMod =
                        Internal.ComplexNumbers.modulusPart quotient

                    quotientPhase =
                        Internal.ComplexNumbers.thetaPart quotient

                    quotientCartesian =
                        ComplexNumbers.convertFromPolarToCartesian quotient

                    expected =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real (quotientMod * Basics.cos quotientPhase)) (ComplexNumbers.Imaginary (quotientMod * Basics.sin quotientPhase))
                in
                Expect.equal quotientCartesian (Debug.log "Expected " expected)
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
        , Test.fuzz2 Fuzz.float Fuzz.float "print ComplexNumberCartesian" <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary two)

                    printedComplexNumber =
                        ComplexNumbers.print complexNumber

                    readComplexNumber =
                        ComplexNumbers.read printedComplexNumber
                in
                Expect.equal readComplexNumber (Ok complexNumber)
        ]

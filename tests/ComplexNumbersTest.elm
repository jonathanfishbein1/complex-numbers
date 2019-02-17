module ComplexNumbersTest exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Monoid
import Test


suite : Test.Test
suite =
    Test.describe "The ComplexNumbers module"
        [ Test.fuzz2 Fuzz.float Fuzz.float "tests that ++ equivalent to Monoid.concat for string" <|
            \real imaginary ->
                let
                    testValue =
                        ComplexNumbers.ComplexNumber (ComplexNumbers.Real real) (ComplexNumbers.Imaginary imaginary)

                    expected =
                        ComplexNumbers.ComplexNumber (ComplexNumbers.Real <| 2 * real) (ComplexNumbers.Imaginary <| 2 * imaginary)
                in
                ComplexNumbers.add testValue testValue
                    |> Expect.equal expected
        , Test.fuzz2 Fuzz.float Fuzz.float "tests ComplexNumber empty " <|
            \real imaginary ->
                let
                    expected =
                        ComplexNumbers.ComplexNumber (ComplexNumbers.Real real) (ComplexNumbers.Imaginary imaginary)

                    empty =
                        ComplexNumbers.ComplexNumber (ComplexNumbers.Real 0) (ComplexNumbers.Imaginary 0)
                in
                Monoid.append ComplexNumbers.complexAdd expected empty
                    |> Expect.equal expected
        , Test.fuzz3 Fuzz.float Fuzz.float Fuzz.float "tests ComplexNumbers multiplication is communanative" <|
            \one two three ->
                let
                    a =
                        ComplexNumbers.ComplexNumber (ComplexNumbers.Real one) (ComplexNumbers.Imaginary two)

                    b =
                        ComplexNumbers.ComplexNumber (ComplexNumbers.Real two) (ComplexNumbers.Imaginary three)

                    testValueOne =
                        ComplexNumbers.multiply a b

                    testValueTwo =
                        ComplexNumbers.multiply b a
                in
                testValueOne
                    |> Expect.equal testValueTwo
        , Test.fuzz3 Fuzz.float Fuzz.float Fuzz.float "tests ComplexNumbers division" <|
            \one two three ->
                let
                    dividend =
                        ComplexNumbers.ComplexNumber (ComplexNumbers.Real one) (ComplexNumbers.Imaginary two)

                    divisor =
                        ComplexNumbers.ComplexNumber (ComplexNumbers.Real two) (ComplexNumbers.Imaginary three)

                    (ComplexNumbers.ComplexNumber (ComplexNumbers.Real realDivisor) _) =
                        divisor

                    (ComplexNumbers.ComplexNumber _ (ComplexNumbers.Imaginary imaginaryDivisor)) =
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
        ]

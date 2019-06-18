module ComplexNumbersMonoidTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Internal.ComplexNumbers
import Monoid
import Test


suite : Test.Test
suite =
    Test.describe "The ComplexNumbers module"
        [ Test.fuzz2 Fuzz.float Fuzz.float "tests ComplexNumberCartesian empty or identity value for sum" <|
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
        , Test.fuzz2 (Fuzz.map toFloat (Fuzz.intRange -10 10)) (Fuzz.map toFloat (Fuzz.intRange -10 10)) "tests ComplexNumberCartesian empty or identity value for product" <|
            \real imaginary ->
                let
                    expected =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real real) (ComplexNumbers.Imaginary imaginary)

                    result =
                        ComplexNumbers.equal (Monoid.append ComplexNumbers.product expected (Monoid.empty ComplexNumbers.product)) expected
                in
                Expect.true "equal" result
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

                    result =
                        ComplexNumbers.equal (Monoid.concat ComplexNumbers.product listOfMonoids) expected
                in
                Expect.true "equal" result
        ]

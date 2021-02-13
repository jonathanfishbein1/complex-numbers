module ComplexNumbersMonoidTests exposing (suite)

import CommutativeMonoid
import ComplexNumbers
import Expect
import Fuzz
import Imaginary
import Real
import Test


suite : Test.Test
suite =
    Test.describe "The ComplexNumbers module"
        [ Test.fuzz2
            Fuzz.float
            Fuzz.float
            "tests ComplexNumber empty or identity value for sum"
          <|
            \real imaginary ->
                let
                    expected =
                        ComplexNumbers.ComplexNumber (Real.Real real) (Imaginary.Imaginary imaginary)

                    (CommutativeMonoid.CommutativeMonoid monoid) =
                        ComplexNumbers.complexSumCommutativeMonoid
                in
                monoid.semigroup expected monoid.identity
                    |> Expect.equal expected
        , Test.fuzz3
            Fuzz.int
            Fuzz.int
            Fuzz.int
            "tests monoidally add"
          <|
            \one two three ->
                let
                    a =
                        ComplexNumbers.ComplexNumber
                            (Real.Real one)
                            (Imaginary.Imaginary two)

                    b =
                        ComplexNumbers.ComplexNumber
                            (Real.Real two)
                            (Imaginary.Imaginary three)

                    c =
                        ComplexNumbers.ComplexNumber
                            (Real.Real one)
                            (Imaginary.Imaginary three)

                    expected =
                        ComplexNumbers.add (ComplexNumbers.add a b) c

                    listOfMonoids =
                        [ a, b, c ]

                    (CommutativeMonoid.CommutativeMonoid monoid) =
                        ComplexNumbers.complexSumCommutativeMonoid
                in
                monoid.concat listOfMonoids
                    |> Expect.equal expected
        , Test.fuzz2
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests ComplexNumber empty or identity value for product"
          <|
            \real imaginary ->
                let
                    expected =
                        ComplexNumbers.ComplexNumber (Real.Real real) (Imaginary.Imaginary imaginary)

                    (CommutativeMonoid.CommutativeMonoid monoid) =
                        ComplexNumbers.complexProductCommutativeMonoid

                    result =
                        ComplexNumbers.equal (monoid.semigroup expected monoid.identity) expected
                in
                Expect.true "equal" result
        , Test.fuzz3
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests monoidally product"
          <|
            \one two three ->
                let
                    a =
                        ComplexNumbers.ComplexNumber
                            (Real.Real one)
                            (Imaginary.Imaginary two)

                    b =
                        ComplexNumbers.ComplexNumber
                            (Real.Real two)
                            (Imaginary.Imaginary three)

                    c =
                        ComplexNumbers.ComplexNumber
                            (Real.Real one)
                            (Imaginary.Imaginary three)

                    expected =
                        ComplexNumbers.multiply (ComplexNumbers.multiply a b) c

                    listOfMonoids =
                        [ a, b, c ]

                    (CommutativeMonoid.CommutativeMonoid monoid) =
                        ComplexNumbers.complexProductCommutativeMonoid

                    result =
                        ComplexNumbers.equal (monoid.concat listOfMonoids) expected
                in
                Expect.true "equal" result
        ]

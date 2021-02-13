module ComplexNumbersFieldTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Imaginary
import Real
import Test


suite : Test.Test
suite =
    Test.describe "The ComplexNumbers Field"
        [ Test.fuzz2
            Fuzz.float
            Fuzz.float
            "tests ComplexNumbers add"
          <|
            \real imaginary ->
                let
                    testValue =
                        ComplexNumbers.ComplexNumber
                            (Real.Real real)
                            (Imaginary.Imaginary imaginary)

                    expected =
                        ComplexNumbers.ComplexNumber
                            (Real.Real <| 2 * real)
                            (Imaginary.Imaginary <| 2 * imaginary)
                in
                ComplexNumbers.add testValue testValue
                    |> Expect.equal expected
        , Test.fuzz3
            Fuzz.float
            Fuzz.float
            Fuzz.float
            "tests ComplexNumbers addition is commutative"
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

                    testValueOne =
                        ComplexNumbers.add a b

                    testValueTwo =
                        ComplexNumbers.add b a
                in
                testValueOne
                    |> Expect.equal testValueTwo
        , Test.fuzz3
            Fuzz.int
            Fuzz.int
            Fuzz.int
            "tests ComplexNumbers addition is associative"
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

                    testValueOne =
                        ComplexNumbers.add (ComplexNumbers.add a b) c

                    testValueTwo =
                        ComplexNumbers.add a (ComplexNumbers.add b c)
                in
                testValueOne
                    |> Expect.equal testValueTwo
        , Test.fuzz2
            Fuzz.float
            Fuzz.float
            "tests ComplexNumbers zero is identity"
          <|
            \real imaginary ->
                let
                    testValue =
                        ComplexNumbers.ComplexNumber
                            (Real.Real real)
                            (Imaginary.Imaginary imaginary)
                in
                ComplexNumbers.add testValue ComplexNumbers.zero
                    |> Expect.equal testValue
        , Test.fuzz3
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests ComplexNumbers multiplication distributes over addition"
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

                    testValueOne =
                        ComplexNumbers.multiply a (ComplexNumbers.add b c)

                    testValueTwo =
                        ComplexNumbers.multiply a b
                            |> ComplexNumbers.add (ComplexNumbers.multiply a c)

                    result =
                        ComplexNumbers.equal testValueOne testValueTwo
                in
                Expect.true "equal" result
        , Test.fuzz2
            Fuzz.int
            Fuzz.int
            "tests ComplexNumbers subtract"
          <|
            \real imaginary ->
                let
                    testValue =
                        ComplexNumbers.ComplexNumber (Real.Real real) (Imaginary.Imaginary imaginary)

                    zero =
                        ComplexNumbers.zero

                    expected =
                        testValue
                in
                ComplexNumbers.subtract testValue zero
                    |> Expect.equal expected
        ]

module ComplexNumbersSemigroupTests exposing (..)

import CommutativeSemigroup
import ComplexNumbers
import Expect
import Fuzz
import Test


suite : Test.Test
suite =
    Test.describe "The Semigroup abstraction"
        [ Test.fuzz3
            Fuzz.int
            Fuzz.int
            Fuzz.int
            "tests complexSumSemigroup is associative"
          <|
            \one two three ->
                let
                    a =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real one)
                            (ComplexNumbers.Imaginary two)

                    b =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real two)
                            (ComplexNumbers.Imaginary three)

                    c =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real one)
                            (ComplexNumbers.Imaginary three)

                    semigroup =
                        ComplexNumbers.complexSumSemigroup

                    aTimesBThenTimesC =
                        semigroup (semigroup a b) c

                    bTimesCThenTimesA =
                        semigroup a (semigroup b c)
                in
                aTimesBThenTimesC
                    |> Expect.equal bTimesCThenTimesA
        , Test.fuzz2
            Fuzz.int
            Fuzz.int
            "tests complexSumSemigroup is commutative"
          <|
            \one two ->
                let
                    a =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real one)
                            (ComplexNumbers.Imaginary two)

                    b =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real two)
                            (ComplexNumbers.Imaginary one)

                    (CommutativeSemigroup.CommutativeSemigroup semigroup) =
                        ComplexNumbers.complexSumCommutativeSemigroup

                    aTimesB =
                        semigroup a b

                    bTimesA =
                        semigroup b a
                in
                aTimesB
                    |> Expect.equal bTimesA
        , Test.fuzz3
            Fuzz.float
            Fuzz.float
            Fuzz.float
            "tests ComplexNumbers multiplication is commutative"
          <|
            \one two three ->
                let
                    a =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real one)
                            (ComplexNumbers.Imaginary two)

                    b =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real two)
                            (ComplexNumbers.Imaginary three)

                    testValueOne =
                        ComplexNumbers.multiply a b

                    testValueTwo =
                        ComplexNumbers.multiply b a
                in
                testValueOne
                    |> Expect.equal testValueTwo
        , Test.fuzz3
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            (Fuzz.floatRange -10 10)
            "tests ComplexNumbers multiplication is associative"
          <|
            \one two three ->
                let
                    a =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real one)
                            (ComplexNumbers.Imaginary two)

                    b =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real two)
                            (ComplexNumbers.Imaginary three)

                    c =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real one)
                            (ComplexNumbers.Imaginary three)

                    testValueOne =
                        ComplexNumbers.multiply (ComplexNumbers.multiply a b) c

                    testValueTwo =
                        ComplexNumbers.multiply a (ComplexNumbers.multiply b c)

                    result =
                        ComplexNumbers.equal testValueOne testValueTwo
                in
                Expect.true "equal" result
        ]

module ComplexNumbersSemigroupTests exposing (..)

import CommutativeSemigroup
import ComplexNumbers
import Expect
import Fuzz
import Imaginary
import Real
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
                            (Real.Real one)
                            (Imaginary.Imaginary two)

                    b =
                        ComplexNumbers.ComplexNumber
                            (Real.Real two)
                            (Imaginary.Imaginary one)

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
                            (Real.Real one)
                            (Imaginary.Imaginary two)

                    b =
                        ComplexNumbers.ComplexNumber
                            (Real.Real two)
                            (Imaginary.Imaginary three)

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
                        ComplexNumbers.multiply (ComplexNumbers.multiply a b) c

                    testValueTwo =
                        ComplexNumbers.multiply a (ComplexNumbers.multiply b c)

                    result =
                        ComplexNumbers.equal.eq testValueOne testValueTwo
                in
                Expect.true "equal" result
        ]

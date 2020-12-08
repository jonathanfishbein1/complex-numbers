module ComplexNumbersSemigroupTests exposing (..)

import CommutativeMonoid
import CommutativeSemigroup
import ComplexNumbers exposing (ComplexNumber)
import Expect
import Fuzz
import Internal.ComplexNumbers
import Semigroup
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
        ]

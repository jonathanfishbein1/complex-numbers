module ComplexNumbersGroupTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Imaginary
import Real
import Test


suite : Test.Test
suite =
    Test.describe "The Group abstraction"
        [ Test.fuzz2
            Fuzz.int
            Fuzz.int
            "tests Complex Number sum group has an inverse"
          <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumber
                            (Real.Real one)
                            (Imaginary.Imaginary two)

                    inversePlusA =
                        ComplexNumbers.complexSumGroup.monoid.semigroup
                            (ComplexNumbers.complexSumGroup.inverse complexNumber)
                            complexNumber

                    aPlusInverse =
                        ComplexNumbers.complexSumGroup.monoid.semigroup
                            complexNumber
                            (ComplexNumbers.complexSumGroup.inverse complexNumber)
                in
                Expect.true "All equal identity"
                    (inversePlusA
                        == ComplexNumbers.complexSumGroup.monoid.identity
                        && aPlusInverse
                        == ComplexNumbers.complexSumGroup.monoid.identity
                    )
        , Test.fuzz2
            (Fuzz.floatRange 1 10)
            (Fuzz.floatRange 1 10)
            "tests Complex Number has an multiplicative inverse"
          <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumber
                            (Real.Real one)
                            (Imaginary.Imaginary two)

                    inversePlusA =
                        ComplexNumbers.complexProductGroup.monoid.semigroup
                            (ComplexNumbers.complexProductGroup.inverse complexNumber)
                            complexNumber

                    aPlusInverse =
                        ComplexNumbers.complexProductGroup.monoid.semigroup
                            complexNumber
                            (ComplexNumbers.complexProductGroup.inverse complexNumber)
                in
                Expect.true "All equal identity"
                    (ComplexNumbers.equal inversePlusA ComplexNumbers.complexProductGroup.monoid.identity
                        && ComplexNumbers.equal aPlusInverse ComplexNumbers.complexProductGroup.monoid.identity
                    )
        ]

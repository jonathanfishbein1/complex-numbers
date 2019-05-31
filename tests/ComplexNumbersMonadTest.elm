module ComplexNumbersMonadTest exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Internal.ComplexNumbers
import Monoid
import Test


suite : Test.Test
suite =
    Test.describe "The ComplexNumbers Monad abstraction"
        [ Test.fuzz Fuzz.int "tests ComplexNumbers Monad left identity" <|
            \one ->
                let
                    f a =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real <| a * 2) (ComplexNumbers.Imaginary <| a * 2)

                    leftSide =
                        ComplexNumbers.bindCartesian (ComplexNumbers.pureCartesian one) f

                    rightSide =
                        f one
                in
                Expect.equal leftSide rightSide
        ]

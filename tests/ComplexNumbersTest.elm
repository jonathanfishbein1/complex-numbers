module ComplexNumbersTest exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
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
        ]

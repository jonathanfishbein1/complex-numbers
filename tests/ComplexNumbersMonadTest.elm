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
        [ Test.describe
            "ComplexNumbers Cartesian Monad tests"
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
            , Test.fuzz Fuzz.int "tests ComplexNumbers Cartesian Monad right identity" <|
                \one ->
                    let
                        m =
                            ComplexNumbers.pureCartesian one

                        leftSide =
                            ComplexNumbers.bindCartesian m ComplexNumbers.pureCartesian
                    in
                    Expect.equal leftSide m
            , Test.fuzz Fuzz.int "tests ComplexNumbers Cartesian Monad associativity" <|
                \one ->
                    let
                        m =
                            ComplexNumbers.pureCartesian one

                        f a =
                            ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real <| a * 2) (ComplexNumbers.Imaginary <| a * 2)

                        g a =
                            ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real <| a * 3) (ComplexNumbers.Imaginary <| a * 3)

                        leftSide =
                            ComplexNumbers.bindCartesian (ComplexNumbers.bindCartesian m f) g

                        rightSide =
                            ComplexNumbers.bindCartesian m (\x -> ComplexNumbers.bindCartesian (f x) g)
                    in
                    Expect.equal leftSide rightSide
            ]
        ]

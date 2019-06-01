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
        , Test.describe
            "ComplexNumbers Polar Monad tests"
            [ Test.fuzz Fuzz.int "tests ComplexNumbers Monad left identity" <|
                \one ->
                    let
                        f a =
                            Internal.ComplexNumbers.ComplexNumberPolar (Internal.ComplexNumbers.Modulus <| a * 2) (Internal.ComplexNumbers.Theta <| a * 2)

                        leftSide =
                            Internal.ComplexNumbers.bindPolar (Internal.ComplexNumbers.purePolar one) f

                        rightSide =
                            f one
                    in
                    Expect.equal leftSide rightSide
            , Test.fuzz Fuzz.int "tests ComplexNumbers Polar Monad right identity" <|
                \one ->
                    let
                        m =
                            Internal.ComplexNumbers.purePolar one

                        leftSide =
                            Internal.ComplexNumbers.bindPolar m Internal.ComplexNumbers.purePolar
                    in
                    Expect.equal leftSide m
            , Test.fuzz Fuzz.int "tests ComplexNumbers Polar Monad associativity" <|
                \one ->
                    let
                        m =
                            Internal.ComplexNumbers.purePolar one

                        f a =
                            Internal.ComplexNumbers.ComplexNumberPolar (Internal.ComplexNumbers.Modulus <| a * 2) (Internal.ComplexNumbers.Theta <| a * 2)

                        g a =
                            Internal.ComplexNumbers.ComplexNumberPolar (Internal.ComplexNumbers.Modulus <| a * 3) (Internal.ComplexNumbers.Theta <| a * 3)

                        leftSide =
                            Internal.ComplexNumbers.bindPolar (Internal.ComplexNumbers.bindPolar m f) g

                        rightSide =
                            Internal.ComplexNumbers.bindPolar m (\x -> Internal.ComplexNumbers.bindPolar (f x) g)
                    in
                    Expect.equal leftSide rightSide
            ]
        ]

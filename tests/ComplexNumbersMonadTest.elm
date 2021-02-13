module ComplexNumbersMonadTest exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Imaginary
import Internal.ComplexNumbers
import Real
import Test


suite : Test.Test
suite =
    Test.describe "The ComplexNumbers Monad abstraction"
        [ Test.describe
            "ComplexNumbers Cartesian Monad tests"
            [ Test.fuzz
                Fuzz.int
                "tests ComplexNumbers Monad left identity"
              <|
                \one ->
                    let
                        f a =
                            ComplexNumbers.ComplexNumber
                                (Real.Real <| a * 2)
                                (Imaginary.Imaginary <| a * 2)

                        leftSide =
                            ComplexNumbers.andThen
                                f
                                (ComplexNumbers.pure one)

                        rightSide =
                            f one
                    in
                    Expect.equal leftSide rightSide
            , Test.fuzz
                Fuzz.int
                "tests ComplexNumbers Cartesian Monad right identity"
              <|
                \one ->
                    let
                        m =
                            ComplexNumbers.pure one

                        leftSide =
                            ComplexNumbers.andThen ComplexNumbers.pure m
                    in
                    Expect.equal leftSide m
            , Test.fuzz
                Fuzz.int
                "tests ComplexNumbers Cartesian Monad associativity"
              <|
                \one ->
                    let
                        m =
                            ComplexNumbers.pure one

                        f a =
                            ComplexNumbers.ComplexNumber
                                (Real.Real <| a * 2)
                                (Imaginary.Imaginary <| a * 2)

                        g a =
                            ComplexNumbers.ComplexNumber
                                (Real.Real <| a * 3)
                                (Imaginary.Imaginary <| a * 3)

                        leftSide =
                            ComplexNumbers.andThen
                                g
                                (ComplexNumbers.andThen f m)

                        rightSide =
                            ComplexNumbers.andThen (\x -> ComplexNumbers.andThen g (f x)) m
                    in
                    Expect.equal leftSide rightSide
            ]
        , Test.describe
            "ComplexNumbers Polar Monad tests"
            [ Test.fuzz
                Fuzz.int
                "tests ComplexNumbers Monad left identity"
              <|
                \one ->
                    let
                        f a =
                            Internal.ComplexNumbers.ComplexNumber
                                (Internal.ComplexNumbers.Modulus <| a * 2)
                                (Internal.ComplexNumbers.Theta <| a * 2)

                        leftSide =
                            Internal.ComplexNumbers.andThen
                                f
                                (Internal.ComplexNumbers.pure one)

                        rightSide =
                            f one
                    in
                    Expect.equal leftSide rightSide
            , Test.fuzz
                Fuzz.int
                "tests ComplexNumbers Polar Monad right identity"
              <|
                \one ->
                    let
                        m =
                            Internal.ComplexNumbers.pure one

                        leftSide =
                            Internal.ComplexNumbers.andThen Internal.ComplexNumbers.pure m
                    in
                    Expect.equal leftSide m
            , Test.fuzz
                Fuzz.int
                "tests ComplexNumbers Polar Monad associativity"
              <|
                \one ->
                    let
                        m =
                            Internal.ComplexNumbers.pure one

                        f a =
                            Internal.ComplexNumbers.ComplexNumber
                                (Internal.ComplexNumbers.Modulus <| a * 2)
                                (Internal.ComplexNumbers.Theta <| a * 2)

                        g a =
                            Internal.ComplexNumbers.ComplexNumber
                                (Internal.ComplexNumbers.Modulus <| a * 3)
                                (Internal.ComplexNumbers.Theta <| a * 3)

                        leftSide =
                            Internal.ComplexNumbers.andThen
                                g
                                (Internal.ComplexNumbers.andThen f m)

                        rightSide =
                            Internal.ComplexNumbers.andThen (\x -> Internal.ComplexNumbers.andThen g (f x)) m
                    in
                    Expect.equal leftSide rightSide
            ]
        ]

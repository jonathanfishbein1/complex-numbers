module ComplexNumbersFunctorTest exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Monoid
import Test


suite : Test.Test
suite =
    Test.describe "The ComplexNumbers Functor abstraction"
        [ Test.fuzz2 Fuzz.int Fuzz.int "tests ComplexNumbers map identity" <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary two)

                    mapResult =
                        ComplexNumbers.mapCartesian identity complexNumber
                in
                mapResult
                    |> Expect.equal complexNumber
        , Test.fuzz2 Fuzz.int Fuzz.int "tests ComplexNumbers map identity polar representation" <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumberPolar (ComplexNumbers.Modulus one) (ComplexNumbers.Theta two)

                    mapResult =
                        ComplexNumbers.mapPolar identity complexNumber
                in
                mapResult
                    |> Expect.equal complexNumber
        , Test.fuzz2 Fuzz.int Fuzz.int "tests ComplexNumbers Functor composition" <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real one) (ComplexNumbers.Imaginary two)

                    f =
                        (*) 2

                    g =
                        (-) 1

                    fdotG =
                        f << g

                    mapResult =
                        ComplexNumbers.mapCartesian fdotG complexNumber
                in
                mapResult
                    |> Expect.equal (ComplexNumbers.mapCartesian f (ComplexNumbers.mapCartesian g complexNumber))
        , Test.fuzz2 Fuzz.int Fuzz.int "tests ComplexNumbers Functor composition polar representation" <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumberPolar (ComplexNumbers.Modulus one) (ComplexNumbers.Theta two)

                    f =
                        (*) 2

                    g =
                        (-) 1

                    fdotG =
                        f << g

                    mapResult =
                        ComplexNumbers.mapPolar fdotG complexNumber
                in
                mapResult
                    |> Expect.equal (ComplexNumbers.mapPolar f (ComplexNumbers.mapPolar g complexNumber))
        ]

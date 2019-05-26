module ComplexNumbersApplicativeFunctorTest exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Monoid
import Test


suite : Test.Test
suite =
    Test.describe "Tests Applicative Functor abstraction for Complex Numbers"
        [ Test.fuzz2 Fuzz.int Fuzz.int "tests first applicative law for Complex Numbers" <|
            \one two ->
                let
                    cIdentity =
                        ComplexNumbers.pureCartesian identity

                    c =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                one
                            )
                            (ComplexNumbers.Imaginary
                                two
                            )

                    cApplied =
                        ComplexNumbers.applyCartesian cIdentity c
                in
                Expect.equal cApplied c
        , Test.fuzz Fuzz.int "tests third applicative law for Matrix" <|
            \one ->
                let
                    f =
                        (*) 2

                    pureF =
                        ComplexNumbers.pureCartesian f

                    pureOne =
                        ComplexNumbers.pureCartesian one

                    expected =
                        ComplexNumbers.pureCartesian <| f one

                    cApplied =
                        ComplexNumbers.applyCartesian pureF pureOne
                in
                Expect.equal cApplied expected
        , Test.fuzz2 Fuzz.int Fuzz.int "tests first applicative law for Complex Numbers Polar" <|
            \one two ->
                let
                    cIdentity =
                        ComplexNumbers.purePolar identity

                    c =
                        ComplexNumbers.ComplexNumberPolar
                            (ComplexNumbers.Modulus
                                one
                            )
                            (ComplexNumbers.Theta
                                two
                            )

                    cApplied =
                        ComplexNumbers.applyPolar cIdentity c
                in
                Expect.equal cApplied c
        , Test.fuzz Fuzz.int "tests third applicative law for Complex Numbers Polar" <|
            \one ->
                let
                    f =
                        (*) 2

                    pureF =
                        ComplexNumbers.purePolar f

                    pureOne =
                        ComplexNumbers.purePolar one

                    expected =
                        ComplexNumbers.purePolar <| f one

                    cApplied =
                        ComplexNumbers.applyPolar pureF pureOne
                in
                Expect.equal cApplied expected
        ]

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
        , Test.fuzz Fuzz.int "tests second applicative law for Complex Numbers" <|
            \one ->
                let
                    f =
                        (<<)

                    fPure =
                        ComplexNumbers.pureCartesian f

                    u =
                        ComplexNumbers.pureCartesian identity

                    v =
                        ComplexNumbers.pureCartesian identity

                    w =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real
                                one
                            )
                            (ComplexNumbers.Imaginary
                                one
                            )

                    leftSide =
                        ComplexNumbers.applyCartesian (ComplexNumbers.applyCartesian (ComplexNumbers.applyCartesian fPure u) v) w

                    rightSide =
                        ComplexNumbers.applyCartesian u (ComplexNumbers.applyCartesian v w)
                in
                Expect.equal leftSide rightSide
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
        , Test.fuzz Fuzz.int "tests fourth applicative law for Complex Numbers" <|
            \one ->
                let
                    pureOne =
                        ComplexNumbers.pureCartesian identity

                    pureTwo =
                        ComplexNumbers.pureCartesian one

                    leftSide =
                        ComplexNumbers.applyCartesian pureOne pureTwo

                    rightSide =
                        ComplexNumbers.applyCartesian (ComplexNumbers.pureCartesian (\_ -> one)) pureOne
                in
                Expect.equal leftSide rightSide
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
        , Test.fuzz Fuzz.int "tests second applicative law for Complex Numbers Polar" <|
            \one ->
                let
                    f =
                        (<<)

                    fPure =
                        ComplexNumbers.purePolar f

                    u =
                        ComplexNumbers.purePolar identity

                    v =
                        ComplexNumbers.purePolar identity

                    w =
                        ComplexNumbers.ComplexNumberPolar
                            (ComplexNumbers.Modulus
                                one
                            )
                            (ComplexNumbers.Theta
                                one
                            )

                    leftSide =
                        ComplexNumbers.applyPolar (ComplexNumbers.applyPolar (ComplexNumbers.applyPolar fPure u) v) w

                    rightSide =
                        ComplexNumbers.applyPolar u (ComplexNumbers.applyPolar v w)
                in
                Expect.equal leftSide rightSide
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
        , Test.fuzz Fuzz.int "tests fourth applicative law for Complex Numbers Polar" <|
            \one ->
                let
                    pureOne =
                        ComplexNumbers.purePolar identity

                    pureTwo =
                        ComplexNumbers.purePolar one

                    leftSide =
                        ComplexNumbers.applyPolar pureOne pureTwo

                    rightSide =
                        ComplexNumbers.applyPolar (ComplexNumbers.purePolar (\_ -> one)) pureOne
                in
                Expect.equal leftSide rightSide
        ]

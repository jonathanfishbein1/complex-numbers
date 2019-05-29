module ComplexNumbersApplicativeFunctorTest exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Internal.ComplexNumbers
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
                        Internal.ComplexNumbers.purePolar identity

                    c =
                        Internal.ComplexNumbers.ComplexNumberPolar
                            (Internal.ComplexNumbers.Modulus
                                one
                            )
                            (Internal.ComplexNumbers.Theta
                                two
                            )

                    cApplied =
                        Internal.ComplexNumbers.applyPolar cIdentity c
                in
                Expect.equal cApplied c
        , Test.fuzz Fuzz.int "tests second applicative law for Complex Numbers Polar" <|
            \one ->
                let
                    f =
                        (<<)

                    fPure =
                        Internal.ComplexNumbers.purePolar f

                    u =
                        Internal.ComplexNumbers.purePolar identity

                    v =
                        Internal.ComplexNumbers.purePolar identity

                    w =
                        Internal.ComplexNumbers.ComplexNumberPolar
                            (Internal.ComplexNumbers.Modulus
                                one
                            )
                            (Internal.ComplexNumbers.Theta
                                one
                            )

                    leftSide =
                        Internal.ComplexNumbers.applyPolar (Internal.ComplexNumbers.applyPolar (Internal.ComplexNumbers.applyPolar fPure u) v) w

                    rightSide =
                        Internal.ComplexNumbers.applyPolar u (Internal.ComplexNumbers.applyPolar v w)
                in
                Expect.equal leftSide rightSide
        , Test.fuzz Fuzz.int "tests third applicative law for Complex Numbers Polar" <|
            \one ->
                let
                    f =
                        (*) 2

                    pureF =
                        Internal.ComplexNumbers.purePolar f

                    pureOne =
                        Internal.ComplexNumbers.purePolar one

                    expected =
                        Internal.ComplexNumbers.purePolar <| f one

                    cApplied =
                        Internal.ComplexNumbers.applyPolar pureF pureOne
                in
                Expect.equal cApplied expected
        , Test.fuzz Fuzz.int "tests fourth applicative law for Complex Numbers Polar" <|
            \one ->
                let
                    pureOne =
                        Internal.ComplexNumbers.purePolar identity

                    pureTwo =
                        Internal.ComplexNumbers.purePolar one

                    leftSide =
                        Internal.ComplexNumbers.applyPolar pureOne pureTwo

                    rightSide =
                        Internal.ComplexNumbers.applyPolar (Internal.ComplexNumbers.purePolar (\_ -> one)) pureOne
                in
                Expect.equal leftSide rightSide
        ]

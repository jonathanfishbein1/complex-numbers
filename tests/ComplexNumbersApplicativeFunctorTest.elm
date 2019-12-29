module ComplexNumbersApplicativeFunctorTest exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Internal.ComplexNumbers
import Test


suite : Test.Test
suite =
    Test.describe "Tests Applicative Functor abstraction for Complex Numbers"
        [ Test.describe "Tests Applicative Four Laws For Both Cartesian and Polar"
            [ Test.fuzz2
                Fuzz.int
                Fuzz.int
                "tests first applicative law for Complex Numbers"
              <|
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
            , Test.fuzz
                Fuzz.int
                "tests second applicative law for Complex Numbers"
              <|
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
                            ComplexNumbers.applyCartesian
                                (ComplexNumbers.applyCartesian (ComplexNumbers.applyCartesian fPure u) v)
                                w

                        rightSide =
                            ComplexNumbers.applyCartesian
                                u
                                (ComplexNumbers.applyCartesian v w)
                    in
                    Expect.equal leftSide rightSide
            , Test.fuzz
                Fuzz.int
                "tests third applicative law for ComplexNumbers"
              <|
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
            , Test.fuzz
                Fuzz.int
                "tests fourth applicative law for Complex Numbers"
              <|
                \one ->
                    let
                        pureOne =
                            ComplexNumbers.pureCartesian identity

                        pureTwo =
                            ComplexNumbers.pureCartesian one

                        leftSide =
                            ComplexNumbers.applyCartesian pureOne pureTwo

                        rightSide =
                            ComplexNumbers.applyCartesian
                                (ComplexNumbers.pureCartesian (Basics.always one))
                                pureOne
                    in
                    Expect.equal leftSide rightSide
            , Test.fuzz2
                Fuzz.int
                Fuzz.int
                "tests first applicative law for Complex Numbers Polar"
              <|
                \one two ->
                    let
                        cIdentity =
                            Internal.ComplexNumbers.pure identity

                        c =
                            Internal.ComplexNumbers.ComplexNumber
                                (Internal.ComplexNumbers.Modulus
                                    one
                                )
                                (Internal.ComplexNumbers.Theta
                                    two
                                )

                        cApplied =
                            Internal.ComplexNumbers.apply cIdentity c
                    in
                    Expect.equal cApplied c
            , Test.fuzz
                Fuzz.int
                "tests second applicative law for Complex Numbers Polar"
              <|
                \one ->
                    let
                        f =
                            (<<)

                        fPure =
                            Internal.ComplexNumbers.pure f

                        u =
                            Internal.ComplexNumbers.pure identity

                        v =
                            Internal.ComplexNumbers.pure identity

                        w =
                            Internal.ComplexNumbers.ComplexNumber
                                (Internal.ComplexNumbers.Modulus
                                    one
                                )
                                (Internal.ComplexNumbers.Theta
                                    one
                                )

                        leftSide =
                            Internal.ComplexNumbers.apply
                                (Internal.ComplexNumbers.apply (Internal.ComplexNumbers.apply fPure u) v)
                                w

                        rightSide =
                            Internal.ComplexNumbers.apply
                                u
                                (Internal.ComplexNumbers.apply v w)
                    in
                    Expect.equal leftSide rightSide
            , Test.fuzz
                Fuzz.int
                "tests third applicative law for Complex Numbers Polar"
              <|
                \one ->
                    let
                        f =
                            (*) 2

                        pureF =
                            Internal.ComplexNumbers.pure f

                        pureOne =
                            Internal.ComplexNumbers.pure one

                        expected =
                            Internal.ComplexNumbers.pure <| f one

                        cApplied =
                            Internal.ComplexNumbers.apply pureF pureOne
                    in
                    Expect.equal cApplied expected
            , Test.fuzz
                Fuzz.int
                "tests fourth applicative law for Complex Numbers Polar"
              <|
                \one ->
                    let
                        pureOne =
                            Internal.ComplexNumbers.pure identity

                        pureTwo =
                            Internal.ComplexNumbers.pure one

                        leftSide =
                            Internal.ComplexNumbers.apply pureOne pureTwo

                        rightSide =
                            Internal.ComplexNumbers.apply
                                (Internal.ComplexNumbers.pure (Basics.always one))
                                pureOne
                    in
                    Expect.equal leftSide rightSide
            ]
        , Test.fuzz2
            Fuzz.int
            Fuzz.int
            "tests pure f apply x equal map f x"
          <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real one)
                            (ComplexNumbers.Imaginary two)

                    f =
                        (*) 2

                    fMapX =
                        ComplexNumbers.mapCartesian f complexNumber

                    pureFApplyX =
                        ComplexNumbers.applyCartesian (ComplexNumbers.pureCartesian f) complexNumber
                in
                pureFApplyX
                    |> Expect.equal fMapX
        , Test.fuzz2
            Fuzz.int
            Fuzz.int
            "tests pure f apply x equal map f x polar"
          <|
            \one two ->
                let
                    complexNumber =
                        Internal.ComplexNumbers.ComplexNumber
                            (Internal.ComplexNumbers.Modulus one)
                            (Internal.ComplexNumbers.Theta two)

                    f =
                        (*) 2

                    fMapX =
                        Internal.ComplexNumbers.map f complexNumber

                    pureFApplyX =
                        Internal.ComplexNumbers.apply
                            (Internal.ComplexNumbers.pure f)
                            complexNumber
                in
                pureFApplyX
                    |> Expect.equal fMapX
        , Test.fuzz2
            Fuzz.int
            Fuzz.int
            "tests apply + equal to add"
          <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real one)
                            (ComplexNumbers.Imaginary two)

                    doubleComplexNumber =
                        ComplexNumbers.add complexNumber complexNumber

                    f =
                        (+)

                    fMapX =
                        ComplexNumbers.mapCartesian f complexNumber

                    pureFApplyX =
                        ComplexNumbers.applyCartesian fMapX complexNumber
                in
                pureFApplyX
                    |> Expect.equal doubleComplexNumber
        , Test.fuzz2
            Fuzz.int
            Fuzz.int
            "tests apply - equal to add"
          <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumberCartesian
                            (ComplexNumbers.Real one)
                            (ComplexNumbers.Imaginary two)

                    f =
                        (-)

                    fMapX =
                        ComplexNumbers.mapCartesian f complexNumber

                    pureFApplyX =
                        ComplexNumbers.applyCartesian fMapX complexNumber
                in
                pureFApplyX
                    |> Expect.equal (ComplexNumbers.ComplexNumberCartesian (ComplexNumbers.Real 0) (ComplexNumbers.Imaginary 0))
        , Test.fuzz2
            Fuzz.int
            Fuzz.int
            "tests pure f apply x equal map f x multiply polar"
          <|
            \one two ->
                let
                    complexNumber =
                        Internal.ComplexNumbers.ComplexNumber (Internal.ComplexNumbers.Modulus one) (Internal.ComplexNumbers.Theta two)

                    f _ =
                        Internal.ComplexNumbers.ComplexNumber (Internal.ComplexNumbers.Modulus (*)) (Internal.ComplexNumbers.Theta (+))

                    fMapX =
                        Internal.ComplexNumbers.map f complexNumber

                    pureFApplyX =
                        Internal.ComplexNumbers.apply (Internal.ComplexNumbers.pure f) complexNumber
                in
                pureFApplyX
                    |> Expect.equal fMapX
        , Test.fuzz2
            Fuzz.int
            Fuzz.int
            "tests pure f apply x equal map f x multiply divide"
          <|
            \one two ->
                let
                    complexNumber =
                        Internal.ComplexNumbers.ComplexNumber
                            (Internal.ComplexNumbers.Modulus one)
                            (Internal.ComplexNumbers.Theta two)

                    f _ =
                        Internal.ComplexNumbers.ComplexNumber
                            (Internal.ComplexNumbers.Modulus (/))
                            (Internal.ComplexNumbers.Theta (-))

                    fMapX =
                        Internal.ComplexNumbers.map f complexNumber

                    pureFApplyX =
                        Internal.ComplexNumbers.apply
                            (Internal.ComplexNumbers.pure f)
                            complexNumber
                in
                pureFApplyX
                    |> Expect.equal fMapX
        ]

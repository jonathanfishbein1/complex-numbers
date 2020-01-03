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
                            ComplexNumbers.pure identity

                        c =
                            ComplexNumbers.ComplexNumber
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    two
                                )

                        cApplied =
                            ComplexNumbers.andMap cIdentity c
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
                            ComplexNumbers.pure f

                        u =
                            ComplexNumbers.pure identity

                        v =
                            ComplexNumbers.pure identity

                        w =
                            ComplexNumbers.ComplexNumber
                                (ComplexNumbers.Real
                                    one
                                )
                                (ComplexNumbers.Imaginary
                                    one
                                )

                        leftSide =
                            ComplexNumbers.andMap
                                (ComplexNumbers.andMap (ComplexNumbers.andMap fPure u) v)
                                w

                        rightSide =
                            ComplexNumbers.andMap
                                u
                                (ComplexNumbers.andMap v w)
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
                            ComplexNumbers.pure f

                        pureOne =
                            ComplexNumbers.pure one

                        expected =
                            ComplexNumbers.pure <| f one

                        cApplied =
                            ComplexNumbers.andMap pureF pureOne
                    in
                    Expect.equal cApplied expected
            , Test.fuzz
                Fuzz.int
                "tests fourth applicative law for Complex Numbers"
              <|
                \one ->
                    let
                        pureOne =
                            ComplexNumbers.pure identity

                        pureTwo =
                            ComplexNumbers.pure one

                        leftSide =
                            ComplexNumbers.andMap pureOne pureTwo

                        rightSide =
                            ComplexNumbers.andMap
                                (ComplexNumbers.pure (Basics.always one))
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
                            Internal.ComplexNumbers.andMap c cIdentity
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
                            Internal.ComplexNumbers.andMap
                                w
                                (Internal.ComplexNumbers.andMap v (Internal.ComplexNumbers.andMap u fPure))

                        rightSide =
                            Internal.ComplexNumbers.andMap
                                (Internal.ComplexNumbers.andMap w v)
                                u
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
                            Internal.ComplexNumbers.andMap pureOne pureF
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
                            Internal.ComplexNumbers.andMap pureTwo pureOne

                        rightSide =
                            Internal.ComplexNumbers.andMap
                                pureOne
                                (Internal.ComplexNumbers.pure (Basics.always one))
                    in
                    Expect.equal leftSide rightSide
            ]
        , Test.fuzz2
            Fuzz.int
            Fuzz.int
            "tests pure f andMap x equal map f x"
          <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real one)
                            (ComplexNumbers.Imaginary two)

                    f =
                        (*) 2

                    fMapX =
                        ComplexNumbers.map f complexNumber

                    pureFApplyX =
                        ComplexNumbers.andMap (ComplexNumbers.pure f) complexNumber
                in
                pureFApplyX
                    |> Expect.equal fMapX
        , Test.fuzz2
            Fuzz.int
            Fuzz.int
            "tests pure f andMap x equal map f x polar"
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
                        Internal.ComplexNumbers.andMap
                            complexNumber
                            (Internal.ComplexNumbers.pure f)
                in
                pureFApplyX
                    |> Expect.equal fMapX
        , Test.fuzz2
            Fuzz.int
            Fuzz.int
            "tests andMap + equal to add"
          <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real one)
                            (ComplexNumbers.Imaginary two)

                    doubleComplexNumber =
                        ComplexNumbers.add complexNumber complexNumber

                    f =
                        (+)

                    fMapX =
                        ComplexNumbers.map f complexNumber

                    pureFApplyX =
                        ComplexNumbers.andMap fMapX complexNumber
                in
                pureFApplyX
                    |> Expect.equal doubleComplexNumber
        , Test.fuzz2
            Fuzz.int
            Fuzz.int
            "tests andMap - equal to add"
          <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumber
                            (ComplexNumbers.Real one)
                            (ComplexNumbers.Imaginary two)

                    f =
                        (-)

                    fMapX =
                        ComplexNumbers.map f complexNumber

                    pureFApplyX =
                        ComplexNumbers.andMap fMapX complexNumber
                in
                pureFApplyX
                    |> Expect.equal (ComplexNumbers.ComplexNumber (ComplexNumbers.Real 0) (ComplexNumbers.Imaginary 0))
        , Test.fuzz2
            Fuzz.int
            Fuzz.int
            "tests pure f andMap x equal map f x multiply polar"
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
                        Internal.ComplexNumbers.andMap complexNumber (Internal.ComplexNumbers.pure f)
                in
                pureFApplyX
                    |> Expect.equal fMapX
        , Test.fuzz2
            Fuzz.int
            Fuzz.int
            "tests pure f andMap x equal map f x multiply divide"
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
                        Internal.ComplexNumbers.andMap
                            complexNumber
                            (Internal.ComplexNumbers.pure f)
                in
                pureFApplyX
                    |> Expect.equal fMapX
        ]

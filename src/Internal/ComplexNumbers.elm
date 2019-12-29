module Internal.ComplexNumbers exposing
    ( ComplexNumberPolar(..)
    , Modulus(..)
    , Theta(..)
    , applyPolar
    , bindPolar
    , dividePolar
    , liftPolar
    , mapPolar
    , modulus
    , multiplyPolar
    , power
    , purePolar
    , theta
    )

{-| Modulus or magnitude portion
-}


type Modulus m
    = Modulus m


{-| Angle in real-complex plane of modulus
-}
type Theta t
    = Theta t


{-| Polar representation of a complex number
-}
type ComplexNumberPolar a
    = ComplexNumberPolar (Modulus a) (Theta a)


{-| Extracts the modulus part of a complex number
-}
modulus : ComplexNumberPolar a -> a
modulus (ComplexNumberPolar (Modulus ro) _) =
    ro


{-| Extracts the imaginary part of a complex number
-}
theta : ComplexNumberPolar a -> a
theta (ComplexNumberPolar _ (Theta thta)) =
    thta


{-| Multiply two complex numbers in polar representations together
-}
multiplyPolar :
    ComplexNumberPolar number
    -> ComplexNumberPolar number
    -> ComplexNumberPolar number
multiplyPolar (ComplexNumberPolar (Modulus roOne) (Theta thetaOne)) (ComplexNumberPolar (Modulus roTwo) (Theta thetaTwo)) =
    ComplexNumberPolar (Modulus <| roOne * roTwo) (Theta <| thetaOne + thetaTwo)


{-| Divide two complex numbers in polar representations together
-}
dividePolar :
    ComplexNumberPolar Float
    -> ComplexNumberPolar Float
    -> ComplexNumberPolar Float
dividePolar (ComplexNumberPolar (Modulus roOne) (Theta thetaOne)) (ComplexNumberPolar (Modulus roTwo) (Theta thetaTwo)) =
    ComplexNumberPolar (Modulus <| roOne / roTwo) (Theta <| thetaOne - thetaTwo)


{-| Calculate a complex number raised to a power
-}
power : number -> ComplexNumberPolar number -> ComplexNumberPolar number
power n (ComplexNumberPolar (Modulus roOne) (Theta thetaOne)) =
    ComplexNumberPolar (Modulus <| roOne ^ n) (Theta <| n * thetaOne)


{-| Map over a complex number in polar representation
-}
mapPolar : (a -> b) -> ComplexNumberPolar a -> ComplexNumberPolar b
mapPolar f (ComplexNumberPolar (Modulus ro) (Theta thta)) =
    ComplexNumberPolar (Modulus <| f ro) (Theta <| f thta)


{-| Place a value in the minimal Complex Number polar context
-}
purePolar : a -> ComplexNumberPolar a
purePolar a =
    ComplexNumberPolar (Modulus a) (Theta a)


{-| Apply for Complex Number polar representaiton applicative
-}
applyPolar :
    ComplexNumberPolar (a -> b)
    -> ComplexNumberPolar a
    -> ComplexNumberPolar b
applyPolar (ComplexNumberPolar (Modulus fRo) (Theta fTheta)) (ComplexNumberPolar (Modulus ro) (Theta thta)) =
    ComplexNumberPolar (Modulus <| fRo ro) (Theta <| fTheta thta)


{-| Monadic bind for Complex Number polar representaiton
-}
bindPolar :
    ComplexNumberPolar a
    -> (a -> ComplexNumberPolar b)
    -> ComplexNumberPolar b
bindPolar (ComplexNumberPolar (Modulus previousModulus) (Theta previousTheta)) f =
    ComplexNumberPolar (Modulus <| modulus <| f previousModulus) (Theta <| theta <| f previousTheta)


liftPolar :
    (a -> b -> c)
    -> ComplexNumberPolar a
    -> ComplexNumberPolar b
    -> ComplexNumberPolar c
liftPolar f a b =
    applyPolar (mapPolar f a) b

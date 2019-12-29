module Internal.ComplexNumbers exposing
    ( ComplexNumber(..)
    , Modulus(..)
    , Theta(..)
    , apply
    , bind
    , dividePolar
    , liftA2
    , mapPolar
    , modulus
    , multiplyPolar
    , power
    , pure
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
type ComplexNumber a
    = ComplexNumber (Modulus a) (Theta a)


{-| Extracts the modulus part of a complex number
-}
modulus : ComplexNumber a -> a
modulus (ComplexNumber (Modulus ro) _) =
    ro


{-| Extracts the imaginary part of a complex number
-}
theta : ComplexNumber a -> a
theta (ComplexNumber _ (Theta thta)) =
    thta


{-| Multiply two complex numbers in polar representations together
-}
multiplyPolar :
    ComplexNumber number
    -> ComplexNumber number
    -> ComplexNumber number
multiplyPolar (ComplexNumber (Modulus roOne) (Theta thetaOne)) (ComplexNumber (Modulus roTwo) (Theta thetaTwo)) =
    ComplexNumber (Modulus <| roOne * roTwo) (Theta <| thetaOne + thetaTwo)


{-| Divide two complex numbers in polar representations together
-}
dividePolar :
    ComplexNumber Float
    -> ComplexNumber Float
    -> ComplexNumber Float
dividePolar (ComplexNumber (Modulus roOne) (Theta thetaOne)) (ComplexNumber (Modulus roTwo) (Theta thetaTwo)) =
    ComplexNumber (Modulus <| roOne / roTwo) (Theta <| thetaOne - thetaTwo)


{-| Calculate a complex number raised to a power
-}
power : number -> ComplexNumber number -> ComplexNumber number
power n (ComplexNumber (Modulus roOne) (Theta thetaOne)) =
    ComplexNumber (Modulus <| roOne ^ n) (Theta <| n * thetaOne)


{-| Map over a complex number in polar representation
-}
mapPolar : (a -> b) -> ComplexNumber a -> ComplexNumber b
mapPolar f (ComplexNumber (Modulus ro) (Theta thta)) =
    ComplexNumber (Modulus <| f ro) (Theta <| f thta)


{-| Place a value in the minimal Complex Number polar context
-}
pure : a -> ComplexNumber a
pure a =
    ComplexNumber (Modulus a) (Theta a)


{-| Apply for Complex Number polar representaiton applicative
-}
apply :
    ComplexNumber (a -> b)
    -> ComplexNumber a
    -> ComplexNumber b
apply (ComplexNumber (Modulus fRo) (Theta fTheta)) (ComplexNumber (Modulus ro) (Theta thta)) =
    ComplexNumber (Modulus <| fRo ro) (Theta <| fTheta thta)


{-| Monadic bind for Complex Number polar representaiton
-}
bind :
    ComplexNumber a
    -> (a -> ComplexNumber b)
    -> ComplexNumber b
bind (ComplexNumber (Modulus previousModulus) (Theta previousTheta)) f =
    ComplexNumber (Modulus <| modulus <| f previousModulus) (Theta <| theta <| f previousTheta)


liftA2 :
    (a -> b -> c)
    -> ComplexNumber a
    -> ComplexNumber b
    -> ComplexNumber c
liftA2 f a b =
    apply (mapPolar f a) b

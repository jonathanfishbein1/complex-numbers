module Internal.ComplexNumbers exposing
    ( ComplexNumber(..)
    , Modulus(..)
    , Theta(..)
    , apply
    , bind
    , divide
    , liftA2
    , map
    , modulus
    , multiply
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
multiply :
    ComplexNumber number
    -> ComplexNumber number
    -> ComplexNumber number
multiply (ComplexNumber (Modulus roOne) (Theta thetaOne)) (ComplexNumber (Modulus roTwo) (Theta thetaTwo)) =
    ComplexNumber (Modulus <| roOne * roTwo) (Theta <| thetaOne + thetaTwo)


{-| Divide two complex numbers in polar representations together
-}
divide :
    ComplexNumber Float
    -> ComplexNumber Float
    -> ComplexNumber Float
divide (ComplexNumber (Modulus roOne) (Theta thetaOne)) (ComplexNumber (Modulus roTwo) (Theta thetaTwo)) =
    ComplexNumber (Modulus <| roOne / roTwo) (Theta <| thetaOne - thetaTwo)


{-| Calculate a complex number raised to a power
-}
power : number -> ComplexNumber number -> ComplexNumber number
power n (ComplexNumber (Modulus roOne) (Theta thetaOne)) =
    ComplexNumber (Modulus <| roOne ^ n) (Theta <| n * thetaOne)


{-| Map over a complex number in polar representation
-}
map : (a -> b) -> ComplexNumber a -> ComplexNumber b
map f (ComplexNumber (Modulus ro) (Theta thta)) =
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
    apply (map f a) b

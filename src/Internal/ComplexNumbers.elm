module Internal.ComplexNumbers exposing
    ( ComplexNumber(..)
    , Modulus(..)
    , Theta(..)
    , andThen
    , apply
    , divide
    , map
    , map2
    , modulus
    , multiply
    , power
    , product
    , productEmpty
    , pure
    , theta
    )

{-| Modulus or magnitude portion
-}

import Typeclasses.Classes.Monoid
import Typeclasses.Classes.Semigroup


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


{-| Monadic andThen for Complex Number polar representaiton
-}
andThen :
    ComplexNumber a
    -> (a -> ComplexNumber b)
    -> ComplexNumber b
andThen (ComplexNumber (Modulus previousModulus) (Theta previousTheta)) f =
    ComplexNumber
        (Modulus <| modulus <| f previousModulus)
        (Theta <| theta <| f previousTheta)


map2 :
    (a -> b -> c)
    -> ComplexNumber a
    -> ComplexNumber b
    -> ComplexNumber c
map2 f a b =
    apply (map f a) b


{-| one
-}
one : ComplexNumber number
one =
    ComplexNumber (Modulus 1) (Theta 0)


productEmpty : ComplexNumber number
productEmpty =
    one


{-| Monoidally multiply two complex numbers together
-}
product : Typeclasses.Classes.Monoid.Monoid (ComplexNumber Float)
product =
    Typeclasses.Classes.Monoid.semigroupAndIdentity
        (Typeclasses.Classes.Semigroup.prepend multiply)
        productEmpty

module ComplexNumbers exposing (ComplexNumber(..))

{-| A module for complex numbers
For instance, we defined generic `concat` in this module using `Monoid` type as follows.

    concat : Monoid a -> List a -> a
    concat m =
        List.foldr (append m) (empty m)


# Types

@docs ComplexNumber


# Constructors

@docs monoid


# Functions for unwraping Monoid

@docs empty


# Convenient functions for monoid

@docs concat


# Monoid types for popular types

@docs string

-}

import Monoid



-- Types


type Real r
    = Real r


type Imaginary i
    = Imaginary i


{-| Main type.
-}
type ComplexNumber a b
    = ComplexNumber (Real a) (Imaginary b)



-- Complex addition


add : ComplexNumber number number -> ComplexNumber number number -> ComplexNumber number number
add (ComplexNumber (Real realOne) (Imaginary imaginaryOne)) (ComplexNumber (Real realTwo) (Imaginary imaginaryTwo)) =
    ComplexNumber (Real (realOne + realTwo)) (Imaginary (imaginaryOne + imaginaryTwo))


complexAdd : Monoid.Monoid (ComplexNumber number number)
complexAdd =
    Monoid.monoid (ComplexNumber (Real 0) (Imaginary 0)) add

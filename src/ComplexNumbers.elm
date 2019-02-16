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

-- Types


type alias Real =
    Float


type alias Imaginary =
    Float


{-| Main type.
-}
type ComplexNumber
    = ComplexNumber Real Imaginary



-- Complex addition


add : ComplexNumber -> ComplexNumber -> ComplexNumber
add (ComplexNumber realOne imaginaryOne) (ComplexNumber realTwo imaginaryTwo) =
    ComplexNumber (realOne + realTwo) (imaginaryOne + imaginaryTwo)

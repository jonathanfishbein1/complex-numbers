module Real exposing
    ( Real(..)
    , zero
    , one
    , real
    , negate
    , map
    , pure
    , andMap
    , andThen
    , equal
    )

{-| A module for Real numbers


# Types

@docs Real


# Values

@docs zero
@docs one


# Arithmetic operations on complex numbers

@docs real
@docs negate


# Semigroup, Monoid, Group, Ring, Field, Functor, Applicative Functor, and Monad

@docs map
@docs pure
@docs andMap
@docs andThen

#Equality

@docs equal

-}

import Float.Extra
import Typeclasses.Classes.Equality


{-| Real portion
-}
type Real r
    = Real r


{-| zero
-}
zero : Real number
zero =
    Real 0


{-| one
-}
one : Real number
one =
    Real 1


{-| Negate a Real number
-}
negate : Real number -> Real number
negate rl =
    Real -(real rl)


{-| Extracts the value of a Real number
-}
real : Real a -> a
real (Real rl) =
    rl


{-| Map over a Real number
-}
map : (a -> b) -> Real a -> Real b
map f (Real r) =
    Real <| f r


{-| Place a value in the minimal Real Number context
-}
pure : a -> Real a
pure a =
    Real a


{-| Apply for Real Number representaiton applicative
-}
andMap :
    Real a
    -> Real (a -> b)
    -> Real b
andMap (Real rl) (Real fReal) =
    Real <| fReal rl


{-| Monadic bind for Real Number representaiton
-}
andThen :
    (a -> Real b)
    -> Real a
    -> Real b
andThen f (Real previousReal) =
    Real <| real <| f previousReal


{-| Equality of Real Numbers
-}
equalImplementation :
    Real Float
    -> Real Float
    -> Bool
equalImplementation (Real realOne) (Real realTwo) =
    Float.Extra.equalWithin 0.000000001 realOne realTwo


{-| `Equal` type for `Real`.
-}
equal : Typeclasses.Classes.Equality.Equality (Real Float)
equal =
    Typeclasses.Classes.Equality.eq equalImplementation

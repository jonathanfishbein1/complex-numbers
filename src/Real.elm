module Real exposing
    ( Real(..)
    , andMap
    , andThen
    , equal
    , equalImplementation
    , map
    , negate
    , one
    , pure
    , real
    , zero
    )

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


negate : Real number -> Real number
negate rl =
    Real -(real rl)


{-| Extracts the real part of a complex number
-}
real : Real a -> a
real (Real rl) =
    rl


{-| Map over a complex number
-}
map : (a -> b) -> Real a -> Real b
map f (Real r) =
    Real <| f r


{-| Place a value in the minimal Complex Number Cartesian context
-}
pure : a -> Real a
pure a =
    Real a


{-| Apply for Complex Number Cartesian representaiton applicative
-}
andMap :
    Real a
    -> Real (a -> b)
    -> Real b
andMap (Real rl) (Real fReal) =
    Real <| fReal rl


{-| Monadic bind for Complex Number Cartesian representaiton
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

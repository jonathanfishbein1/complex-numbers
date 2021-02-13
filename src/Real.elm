module Real exposing
    ( Real(..)
    , negate
    , one
    , real
    , zero
    )

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

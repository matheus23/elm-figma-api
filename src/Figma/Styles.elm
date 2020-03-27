module Figma.Styles exposing (..)

{-|


# Figma Styles

@docs Style, StyleType

-}


{-| A reference to a style in a figma file.
-}
type alias StyleReference =
    { key : String
    , name : String
    , styleType : StyleType
    , description : String
    }


{-| The type of style reference
-}
type StyleType
    = FillStyleType
    | TextStyleType
    | EffectStyleType
    | GridStyleType

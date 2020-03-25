module Figma.Appearance exposing
    ( StrokeAlign(..), Paint(..), SolidColor, Image, ScaleMode(..), Gradient, ColorStop
    , TextStyle, TextStyleOverride, TextHorizontalAlign(..), TextVerticalAlign(..)
    , BlendMode(..), Effect(..), Blur, Shadow
    )

{-|


# Fills and strokes

@docs StrokeAlign, Paint, SolidColor, Image, ScaleMode, Gradient, ColorStop


# Text styling

@docs TextStyle, TextStyleOverride, TextHorizontalAlign, TextVerticalAlign


# Blending modes and effetcs

@docs BlendMode, Effect, Blur, Shadow

-}

import Color exposing (Color)
import Math.Vector2 exposing (Vec2)



-- BLEND MODE


{-| How a layer blends with layers below.
-}
type BlendMode
    = NormalMode
      -- Only applicable to objects with children
    | PassThroughMode
      -- Darken modes
    | DarkenMode
    | MultiplyMode
    | LinearBurnMode
    | ColorBurnMode
      -- Lighten modes
    | LightenMode
    | ScreenMode
    | LinearDodgeMode
    | ColorDodgeMode
      -- Contrast modes
    | OverlayMode
    | SoftLightMode
    | HardLightMode
      -- Inversion modes
    | DifferenceMode
    | ExclusionMode
      -- Component modes
    | HueMode
    | SaturationMode
    | ColorMode
    | LuminosityMode



-- EFFECT


{-| A visual effect such as a shadow or blur.
-}
type Effect
    = InnerShadowEffect Shadow
    | DropShadowEffect Shadow
    | LayerBlurEffect Blur
    | BackgroundBlurEffect Blur


{-| Shadow visual effect.
-}
type alias Shadow =
    { isVisible : Bool
    , radius : Float
    , color : Color
    , blendMode : BlendMode
    , offset : Vec2
    }


{-| Blur visual effect.
-}
type alias Blur =
    { isVisible : Bool
    , radius : Float
    }



-- TYPE


{-| -}
type TextVerticalAlign
    = TopAlign
    | CenterVerticalAlign
    | BottomAlign


{-| -}
type TextHorizontalAlign
    = LeftAlign
    | RightAlign
    | CenterHorizontalAlign
    | JustifiedAlign


{-| Character formatting.
-}
type alias TextStyle =
    { fontFamily : String
    , fontPostScriptName : String
    , isItalic : Bool
    , fontWeight : Int -- 100 .. 900
    , fontSize : Float
    , horizontalAlign : TextHorizontalAlign
    , verticalAlign : TextVerticalAlign
    , letterSpacing : Float
    , fills : List Paint
    , lineHeightPx : Float
    , lineHeightPercent : Float
    }


{-| Character formatting overrides.

Only relevant fields are set while specifying an override. All other fields are supposed
to be found in the master `TextStyle` record.

-}
type alias TextStyleOverride =
    { fontFamily : Maybe String
    , fontPostScriptName : Maybe String
    , isItalic : Maybe Bool
    , fontWeight : Maybe Int -- 100 .. 900
    , fontSize : Maybe Float
    , horizontalAlign : Maybe TextHorizontalAlign
    , verticalAlign : Maybe TextVerticalAlign
    , letterSpacing : Maybe Float
    , fills : Maybe (List Paint)
    , lineHeightPx : Maybe Float
    , lineHeightPercent : Maybe Float
    }



-- STROKE ALIGN


{-| Where stroke is drawn relative to the vector outline.
-}
type StrokeAlign
    = InsideStroke
    | OutsideStroke
    | CenterStroke



-- PAINT


{-| A solid color, gradient, or image texture that can be applied as fills or strokes.
-}
type Paint
    = ColorPaint SolidColor
    | ImagePaint Image
    | LinearGradientPaint Gradient
    | RadialGradientPaint Gradient
    | AngularGradientPaint Gradient
    | EmojiPaint
    | DiamondGradientPaint Gradient


{-| Solid color paint.
-}
type alias SolidColor =
    { isVisible : Bool
    , opacity : Float
    , color : Color
    , blendMode : BlendMode
    }


{-| A color gradient paint. In particular:

  - `start`: position of the start handle for the gradient (value 0, for the purposes of calculating gradient stops)
  - `stop`: position of the end handle for the gradient (value 1),
  - `width`: width of the gradient (only relevant for non-linear gradients)

These three values are stored in normalized object space. Normalized object space is if the top left corner of the bounding box of the object is `0, 0` and the bottom right is `1,1`.

  - `colorStops`: positions of key points along the gradient axis with the colors anchored there. Colors along the gradient are interpolated smoothly between neighboring gradient stops.

-}
type alias Gradient =
    { isVisible : Bool
    , opacity : Float
    , start : Vec2
    , stop : Vec2
    , width : Vec2
    , colorStops : List ColorStop
    , blendMode : BlendMode
    }


{-| A image-textured paint.
-}
type alias Image =
    { isVisible : Bool
    , opacity : Float
    , scaleMode : ScaleMode
    , blendMode : BlendMode
    }


{-| Image scaling mode.
-}
type ScaleMode
    = FillMode
    | FitMode
    | TileMode
    | StretchMode


{-| A position color pair representing a gradient stop.
-}
type alias ColorStop =
    { position : Float
    , color : Color
    }

module Figma.Internal.Geometry exposing
    ( boundingBoxDecoder
    , encodePoint
    , pointDecoder
    , positionDecoder
    )

import Figma.Geometry exposing (..)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


boundingBoxDecoder : Decoder BoundingBox
boundingBoxDecoder =
    D.succeed BoundingBox
        |> D.required "x" D.float
        |> D.required "y" D.float
        |> D.required "width" D.float
        |> D.required "height" D.float


pointDecoder : Decoder Point
pointDecoder =
    D.succeed Point
        |> D.required "x" D.float
        |> D.required "y" D.float


encodePoint : Point -> E.Value
encodePoint point =
    E.object
        [ ( "x", E.float point.x )
        , ( "y", E.float point.y )
        ]


positionDecoder : Decoder Position
positionDecoder =
    D.oneOf
        [ D.map AbsolutePosition pointDecoder
        , D.succeed RelativePositionTo
            |> D.required "node_id" D.string
            |> D.required "node_offset" pointDecoder
        ]

module Figma.Internal.Styles exposing (..)

import Dict exposing (Dict)
import Figma.Styles exposing (..)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


styleReferencesDecoder : Decoder (Dict String StyleReference)
styleReferencesDecoder =
    D.dict
        (D.succeed StyleReference
            |> D.required "key" D.string
            |> D.required "name" D.string
            |> D.required "styleType" styleTypeDecoder
            |> D.required "description" D.string
        )


styleTypeDecoder : Decoder StyleType
styleTypeDecoder =
    D.string
        |> D.andThen
            (\value ->
                case value of
                    "FILL" ->
                        D.succeed FillStyleType

                    "TEXT" ->
                        D.succeed TextStyleType

                    "EFFECT" ->
                        D.succeed EffectStyleType

                    "GRID" ->
                        D.succeed GridStyleType

                    unrecognized ->
                        D.fail <| "Unrecognized style type: " ++ unrecognized
            )

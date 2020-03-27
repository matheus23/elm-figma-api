module Figma.Internal.Layout exposing
    ( autoLayoutDecoder
    , gridDecoder
    , horizontalConstraintDecoder
    , layoutAlignDecoder
    , verticalConstraintDecoder
    )

import Figma.Internal.Appearance exposing (..)
import Figma.Layout exposing (..)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D



-- GRIDS


gridDecoder : Decoder LayoutGrid
gridDecoder =
    D.field "pattern" D.string
        |> D.andThen
            (\value ->
                case value of
                    "COLUMNS" ->
                        D.map ColumnsGrid columnsGridDecoder

                    "ROWS" ->
                        D.map RowsGrid rowsGridDecoder

                    "GRID" ->
                        D.map SquareGrid squareGridDecoder

                    _ ->
                        D.fail <| "Unrecognized grid pattern value: " ++ value
            )


columnsGridDecoder : Decoder Columns
columnsGridDecoder =
    D.succeed Columns
        |> D.required "sectionSize" D.float
        |> D.required "visible" D.bool
        |> D.required "color" colorDecoder
        |> D.required "gutterSize" D.float
        |> D.required "offset" D.float
        |> D.required "count" D.int
        |> D.required "alignment" alignDecoder


rowsGridDecoder : Decoder Rows
rowsGridDecoder =
    D.succeed Rows
        |> D.required "sectionSize" D.float
        |> D.required "visible" D.bool
        |> D.required "color" colorDecoder
        |> D.required "gutterSize" D.float
        |> D.required "offset" D.float
        |> D.required "count" D.int
        |> D.required "alignment" alignDecoder


squareGridDecoder : Decoder Grid
squareGridDecoder =
    D.succeed Grid
        |> D.required "sectionSize" D.float
        |> D.required "visible" D.bool
        |> D.required "color" colorDecoder


alignDecoder : Decoder GridAlign
alignDecoder =
    D.string
        |> D.andThen
            (\value ->
                case value of
                    "MIN" ->
                        D.succeed MinAlign

                    "STRETCH" ->
                        D.succeed MaxAlign

                    "CENTER" ->
                        D.succeed CenterAlign

                    unrecognized ->
                        D.fail <| "Unrecognized grid align value: " ++ unrecognized
            )



-- CONSTRAINTS


verticalConstraintDecoder : Decoder LayoutVerticalConstraint
verticalConstraintDecoder =
    D.string
        |> D.andThen
            (\value ->
                case value of
                    "TOP" ->
                        D.succeed TopConstraint

                    "BOTTOM" ->
                        D.succeed BottomConstraint

                    "TOP_BOTTOM" ->
                        D.succeed TopBottomConstraint

                    "CENTER" ->
                        D.succeed CenterVerticalConstraint

                    "SCALE" ->
                        D.succeed ScaleVerticalConstraint

                    unrecognized ->
                        D.fail <| "Unrecognized layout constraint value: " ++ unrecognized
            )


horizontalConstraintDecoder : Decoder LayoutHorizontalConstraint
horizontalConstraintDecoder =
    D.string
        |> D.andThen
            (\value ->
                case value of
                    "LEFT" ->
                        D.succeed LeftConstraint

                    "RIGHT" ->
                        D.succeed RightConstraint

                    "LEFT_RIGHT" ->
                        D.succeed LeftRightConstraint

                    "CENTER" ->
                        D.succeed CenterHorizontalConstraint

                    "SCALE" ->
                        D.succeed ScaleHorizontalConstraint

                    unrecognized ->
                        D.fail <| "Unrecognized layout constraint value: " ++ unrecognized
            )



-- AUTO LAYOUT


autoLayoutDecoder : Decoder (Maybe LayoutAutoLayout)
autoLayoutDecoder =
    D.succeed identity
        |> D.optional "layoutMode" (D.maybe layoutModeDecoder) Nothing
        |> D.andThen
            (\args ->
                case args of
                    Nothing ->
                        D.succeed Nothing

                    Just layoutMode ->
                        D.succeed (LayoutAutoLayout layoutMode)
                            |> D.optional "counterAxisSizingMode" layoutCounterAxisSizingModeDecoder AutoCounterAxisSizingMode
                            |> D.optional "itemSpacing" D.float 0
                            |> D.optional "horizontalPadding" D.float 0
                            |> D.optional "verticalPadding" D.float 0
                            |> D.map Just
            )


layoutAlignDecoder : Decoder LayoutAlign
layoutAlignDecoder =
    D.string
        |> D.andThen
            (\value ->
                case value of
                    "MIN" ->
                        D.succeed MinLayoutAlign

                    "CENTER" ->
                        D.succeed CenterLayoutAlign

                    "MAX" ->
                        D.succeed MaxLayoutAlign

                    "STRETCH" ->
                        D.succeed StretchLayoutAlign

                    unrecognized ->
                        D.fail <| "Unrecognized layout align: " ++ unrecognized
            )


layoutModeDecoder : Decoder LayoutMode
layoutModeDecoder =
    D.string
        |> D.andThen
            (\value ->
                case value of
                    "HORIZONTAL" ->
                        D.succeed HorizontalLayoutMode

                    "VERTICAL" ->
                        D.succeed VerticalLayoutMode

                    unrecognized ->
                        D.fail <| "Unrecognized layout mode: " ++ unrecognized
            )


layoutCounterAxisSizingModeDecoder : Decoder LayoutCounterAxisSizingMode
layoutCounterAxisSizingModeDecoder =
    D.string
        |> D.andThen
            (\value ->
                case value of
                    "FIXED" ->
                        D.succeed FixedCounterAxisSizingMode

                    "AUTO" ->
                        D.succeed AutoCounterAxisSizingMode

                    unrecognized ->
                        D.fail <| "Unrecognized counter axis sizing mode: " ++ unrecognized
            )

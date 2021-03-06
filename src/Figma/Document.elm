module Figma.Document exposing
    ( Node(..), Tree, NodeId, Document, Canvas, Frame, Group, BooleanGroup, BooleanOperation(..), Vector, Rectangle, Slice, Text, Component, Instance
    , ExportConstraint(..), ExportSetting, ExportFormat(..)
    , singleton, tree, node, children, foldl, toRosetree
    )

{-| This module provides several data structures to describe a Figma document
and functions which operate on it.


# Document node types

@docs Node, Tree, NodeId, Document, Canvas, Frame, Group, BooleanGroup, BooleanOperation, Vector, Rectangle, Slice, Text, Component, Instance


# Export constraints and settings

@docs ExportConstraint, ExportSetting, ExportFormat


# Tree creation and manipulation

@docs singleton, tree, node, children, foldl, toRosetree

-}

import Color exposing (Color)
import Dict exposing (Dict)
import Figma.Appearance exposing (..)
import Figma.Geometry exposing (..)
import Figma.Layout exposing (..)
import Tree as T


{-| Represents a multiway tree made of `Node`'s.
-}
type Tree
    = Tree (T.Tree Node)



-- CREATION


{-| Creates a singleton tree. This corresponds to `tree v []`.
-}
singleton : Node -> Tree
singleton =
    T.singleton >> Tree


{-| Construct a tree from a node and a list of children.
-}
tree : Node -> List Tree -> Tree
tree node_ children_ =
    Tree (T.tree node_ (List.map toRosetree children_))


{-| Return a Rosetree data structure, which can be used with
the [Elm Rosetree package](http://package.elm-lang.org/packages/zwilias/elm-rosetree/latest).
-}
toRosetree : Tree -> T.Tree Node
toRosetree (Tree rosetree) =
    rosetree



-- READING


{-| Return the node of a tree.
-}
node : Tree -> Node
node (Tree rosetree) =
    T.label rosetree


{-| Return the children of a tree as a list.
-}
children : Tree -> List Tree
children (Tree rosetree) =
    T.children rosetree
        |> List.map Tree



-- FOLDING


{-| Fold over all the nodes in a tree, left to right, depth first.
-}
foldl : (Node -> b -> b) -> b -> Tree -> b
foldl fn accumulator (Tree rosetree) =
    T.foldl fn accumulator rosetree


{-| A value which uniquely identifies a node in the document.
-}
type alias NodeId =
    String


{-| A Figma document consists as tree of nodes. It starts with a
`DocumentNode`, which has one or more `CanvasNode`'s children — called _Pages_ in the UI —
which in turn contain nodes for frames, images, vector shapes, etc.
-}
type Node
    = DocumentNode Document
    | CanvasNode Canvas
    | FrameNode Frame
    | GroupNode Group
    | BooleanGroupNode BooleanGroup
    | VectorNode Vector
    | StarNode Vector
    | LineNode Vector
    | EllipseNode Vector
    | RegularPolygonNode Vector
    | RectangleNode Rectangle
    | SliceNode Slice
    | TextNode Text
    | ComponentNode Component
    | InstanceNode Instance


{-| A reusable component node. It has the same fields of `Frame`.
-}
type alias Component =
    Frame



-- DOCUMENT


{-| The root node of a document.
-}
type alias Document =
    { id : NodeId
    , name : String
    , isVisible : Bool
    }



-- CANVAS


{-| A single page in a document.
-}
type alias Canvas =
    { id : NodeId
    , name : String
    , isVisible : Bool
    , backgroundColor : Color
    , exportSettings : List ExportSetting
    }



-- FRAME


{-| A node of fixed size containing other nodes.
-}
type alias Frame =
    { id : NodeId
    , name : String
    , isVisible : Bool
    , backgroundColor : Color
    , exportSettings : List ExportSetting
    , blendMode : BlendMode
    , preserveRatio : Bool
    , horizontalConstraint : LayoutHorizontalConstraint
    , verticalConstraint : LayoutVerticalConstraint
    , transitionTo : Maybe NodeId
    , opacity : Float
    , boundingBox : BoundingBox
    , clipContent : Bool
    , layoutGrids : List LayoutGrid
    , effects : List Effect
    , isMask : Bool
    , layoutAlign : Maybe LayoutAlign
    , autoLayout : Maybe LayoutAutoLayout
    }



-- GROUP


{-| A logical grouping of nodes.
-}
type alias Group =
    { id : NodeId
    , name : String
    , isVisible : Bool
    , backgroundColor : Color
    , exportSettings : List ExportSetting
    , blendMode : BlendMode
    , preserveRatio : Bool
    , horizontalConstraint : LayoutHorizontalConstraint
    , verticalConstraint : LayoutVerticalConstraint
    , transitionTo : Maybe NodeId
    , opacity : Float
    , boundingBox : BoundingBox
    , clipContent : Bool
    , layoutGrids : List LayoutGrid
    , effects : List Effect
    , isMask : Bool
    , layoutAlign : Maybe LayoutAlign
    }



-- BOOLEAN


{-| A group that has a boolean operation (union, intersection, subtraction and exlusion) applied to it.
-}
type alias BooleanGroup =
    { id : NodeId
    , name : String
    , isVisible : Bool
    , exportSettings : List ExportSetting
    , blendMode : BlendMode
    , preserveRatio : Bool
    , horizontalConstraint : LayoutHorizontalConstraint
    , verticalConstraint : LayoutVerticalConstraint
    , transitionTo : Maybe NodeId
    , opacity : Float
    , boundingBox : BoundingBox
    , effects : List Effect
    , isMask : Bool
    , fills : List Paint
    , strokes : List Paint
    , strokeWeight : Float
    , strokeAlign : StrokeAlign
    , layoutAlign : Maybe LayoutAlign
    , operation : BooleanOperation
    }


{-| Type of boolean operation applied to the boolean group children.
-}
type BooleanOperation
    = UnionOperation
    | IntersectOperation
    | SubtractOperation
    | ExcludeOperation



-- SHAPE


{-| A vector network, consisting of vertices and edges. This data structure
is reused multiple times to represent lines, regular polygons, stars and other
document elements.
-}
type alias Vector =
    { id : NodeId
    , name : String
    , isVisible : Bool
    , exportSettings : List ExportSetting
    , blendMode : BlendMode
    , preserveRatio : Bool
    , horizontalConstraint : LayoutHorizontalConstraint
    , verticalConstraint : LayoutVerticalConstraint
    , transitionTo : Maybe NodeId
    , opacity : Float
    , boundingBox : BoundingBox
    , effects : List Effect
    , isMask : Bool
    , fills : List Paint
    , strokes : List Paint
    , strokeWeight : Float
    , strokeAlign : StrokeAlign
    , layoutAlign : Maybe LayoutAlign
    }



-- RECTANGLE


{-| A rectangular vector shape.
-}
type alias Rectangle =
    { id : NodeId
    , name : String
    , isVisible : Bool
    , exportSettings : List ExportSetting
    , blendMode : BlendMode
    , preserveRatio : Bool
    , horizontalConstraint : LayoutHorizontalConstraint
    , verticalConstraint : LayoutVerticalConstraint
    , transitionTo : Maybe NodeId
    , opacity : Float
    , boundingBox : BoundingBox
    , effects : List Effect
    , isMask : Bool
    , fills : List Paint
    , strokes : List Paint
    , strokeWeight : Float
    , strokeAlign : StrokeAlign
    , layoutAlign : Maybe LayoutAlign
    , cornerRadius : Float
    }



-- SLICE


{-| A rectangular region of the canvas that can be exported.
-}
type alias Slice =
    { id : NodeId
    , name : String
    , isVisible : Bool
    , exportSettings : List ExportSetting
    , boundingBox : BoundingBox
    }



-- TEXT


{-| A text box.
-}
type alias Text =
    { id : NodeId
    , name : String
    , isVisible : Bool
    , exportSettings : List ExportSetting
    , blendMode : BlendMode
    , preserveRatio : Bool
    , horizontalConstraint : LayoutHorizontalConstraint
    , verticalConstraint : LayoutVerticalConstraint
    , transitionTo : Maybe NodeId
    , opacity : Float
    , boundingBox : BoundingBox
    , effects : List Effect
    , isMask : Bool
    , fills : List Paint
    , strokes : List Paint
    , strokeWeight : Float
    , strokeAlign : StrokeAlign
    , layoutAlign : Maybe LayoutAlign
    , characters : String
    , style : TextStyle
    , characterStyleOverrides : List Int
    , styleOverrides : Dict Int TextStyleOverride
    }



-- COMPONENT


{-| An instance of a component. Changes to the original component result
in the same changes applied to the instance.
-}
type alias Instance =
    { id : NodeId
    , name : String
    , isVisible : Bool
    , backgroundColor : Color
    , exportSettings : List ExportSetting
    , blendMode : BlendMode
    , preserveRatio : Bool
    , horizontalConstraint : LayoutHorizontalConstraint
    , verticalConstraint : LayoutVerticalConstraint
    , transitionTo : Maybe NodeId
    , opacity : Float
    , boundingBox : BoundingBox
    , clipContent : Bool
    , layoutGrids : List LayoutGrid
    , effects : List Effect
    , isMask : Bool
    , layoutAlign : Maybe LayoutAlign
    , autoLayout : Maybe LayoutAutoLayout
    , componentId : NodeId
    }



-- EXPORT SETTING


{-| Format and size to export an asset at.
-}
type alias ExportSetting =
    { suffix : String
    , format : ExportFormat
    , constraint : ExportConstraint
    }


{-| Format to export an asset to.
-}
type ExportFormat
    = PngFormat
    | JpegFormat
    | SvgFormat
    | PdfFormat


{-| Sizing constraint for exports.
-}
type ExportConstraint
    = ScaleConstraint Float
    | WidthConstraint Float
    | HeightConstraint Float

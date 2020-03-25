module Figma exposing
    ( AuthenticationToken, personalToken, oauth2Token
    , FileKey, File, getFile, ComponentMeta, VersionId, Version, getVersions, getFileWithVersion
    , Comment(..), CommentData, ReplyData, getComments, postComment
    , exportNodesAsPng, exportNodesAsJpeg, exportNodesAsSvg, exportNodesWithOptions, ExportedImage
    , TeamId, ProjectId, Project, getProjects, getFiles, FileMeta
    , User
    , fileDecoder
    )

{-| This module provides endpoints for the Figma web API.


# Authentication

@docs AuthenticationToken, personalToken, oauth2Token


# Document file and versions

@docs FileKey, File, getFile, ComponentMeta, VersionId, Version, getVersions, getFileWithVersion


# Comments

@docs Comment, CommentData, ReplyData, getComments, postComment


# Export document nodes

@docs exportNodesAsPng, exportNodesAsJpeg, exportNodesAsSvg, exportNodesWithOptions, ExportedImage


# Team projects and files

@docs TeamId, ProjectId, Project, getProjects, getFiles, FileMeta


# User

@docs User


# Decoders

@docs fileDecoder

-}

import Dict exposing (Dict)
import Figma.Document exposing (..)
import Figma.Geometry exposing (..)
import Figma.Internal.Document exposing (..)
import Figma.Internal.Geometry exposing (..)
import Http
import Iso8601
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Time



-- AUTHENTICATION


{-| -}
type AuthenticationToken
    = Personal String
    | OAuth2 String


{-| Create a token to be used with the Personal Access Token authentication method.
[Read more](https://www.figma.com/developers/docs#auth-dev-token).
-}
personalToken : String -> AuthenticationToken
personalToken token =
    Personal token


{-| Create a token to be used with the OAuth 2 authentication method.
[Read more](https://www.figma.com/developers/docs#auth-oauth).
-}
oauth2Token : String -> AuthenticationToken
oauth2Token token =
    OAuth2 token


authHeader : AuthenticationToken -> Http.Header
authHeader authToken =
    case authToken of
        Personal token ->
            Http.header "X-Figma-Token" token

        OAuth2 token ->
            Http.header "Authorization" ("Bearer " ++ token)



-- DOCUMENT


baseUrl =
    "https://api.figma.com"


{-| A file key which univocally identifies Figma document on the server.

**Note**: The _file key_ can be extracted from any Figma file URL: `https://www.figma.com/file/:key/:title`, or via the [`getFiles`][#getFiles] function.

-}
type alias FileKey =
    String


{-| Construct a web request to return the latest version of the file referred by _key_ and store it into a `File` record.

    import Http
    import Figma as F

    F.getFile ( F.personalToken "your-token" ) "your-file-key"
        |> Http.send FileReceived

Alternatively `getFile` can be chained together with another request (or any other task) resulting in a single command.

    F.getFile ( F.personalToken "your-token" ) "your-file-key"
        |> Http.toTask
        |> Task.andThen
            (\file ->
                let
                    options =
                        { format = Document.JpegFormat
                        , scale = 1.0
                        }

                    nodeIds =
                        -- Extract your node ID's from file.document here
                in
                    F.exportNodesWithOptions authToken fileKey options nodeIds
                        |> Http.toTask
            )
        |> Task.attempt NodesExported

-}
getFile :
    AuthenticationToken
    -> FileKey
    -> (Result Http.Error File -> msg)
    -> Cmd msg
getFile token fileKey handle =
    let
        url =
            baseUrl ++ "/v1/files/" ++ fileKey
    in
    Http.request
        { method = "GET"
        , headers = [ authHeader token ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson handle fileDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


{-| The file data returned by the server. In particular:

  - `document` is the root node of the document.
  - `components` is a mapping from node ID's to component metadata. This helps you determine
    which components each instance comes from.

-}
type alias File =
    { schemaVersion : Int

    --, name : String
    , thumbnailUrl : String

    --, lastModified : Time.Posix
    , document : Tree
    , components : Dict NodeId ComponentMeta
    }


{-| -}
fileDecoder : Decoder File
fileDecoder =
    D.succeed File
        |> D.required "schemaVersion" D.int
        --|> D.required "name" D.string
        |> D.required "thumbnailUrl" D.string
        --|> D.required "lastModified" dateDecoder
        |> D.required "document" treeDecoder
        |> D.required "components" (D.dict componentMetaDecoder)


{-| Metadata for a master component. Component data is stored in a `Document.Component` record.
-}
type alias ComponentMeta =
    { name : String
    , description : String
    }


componentMetaDecoder : Decoder ComponentMeta
componentMetaDecoder =
    D.succeed ComponentMeta
        |> D.required "name" D.string
        |> D.required "description" D.string



-- VERSION


{-| Unique identifier for file version.
-}
type alias VersionId =
    String


{-| A version of a file.
-}
type alias Version =
    { id : String
    , createdAt : Time.Posix
    , label : String
    , description : String
    , user : User
    }


{-| Construct a web request to return specific file version.
-}
getFileWithVersion :
    AuthenticationToken
    -> FileKey
    -> VersionId
    -> (Result Http.Error File -> msg)
    -> Cmd msg
getFileWithVersion token fileKey versionId handle =
    let
        url =
            baseUrl ++ "/v1/files/" ++ fileKey ++ "?version=" ++ versionId
    in
    Http.request
        { method = "GET"
        , headers = [ authHeader token ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson handle fileDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Construct a web request to list the version history of a file. The version history
consists of versions, manually-saved additions to the version history of a file. If the
account is not on a paid team, version history is limited to the past 30 days.

Note that version history will not include autosaved versions.

-}
getVersions :
    AuthenticationToken
    -> FileKey
    -> (Result Http.Error (List Version) -> msg)
    -> Cmd msg
getVersions token fileKey handle =
    let
        url =
            baseUrl ++ "/v1/files/" ++ fileKey ++ "/versions"
    in
    Http.request
        { method = "GET"
        , headers = [ authHeader token ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson handle versionsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


versionsDecoder : Decoder (List Version)
versionsDecoder =
    D.field "versions" (D.list versionDecoder)


versionDecoder : Decoder Version
versionDecoder =
    D.succeed Version
        |> D.required "id" D.string
        |> D.required "created_at" dateDecoder
        |> D.required "label" D.string
        |> D.required "description" D.string
        |> D.required "user" userDecoder


{-| Construct a web request to return a list of comments left on the document.
-}
getComments :
    AuthenticationToken
    -> FileKey
    -> (Result Http.Error (List Comment) -> msg)
    -> Cmd msg
getComments token fileKey handle =
    let
        url =
            baseUrl ++ "/v1/files/" ++ fileKey ++ "/comments"
    in
    Http.request
        { method = "GET"
        , headers = [ authHeader token ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson handle commentsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Construct a web request to add a new comment to the document.
Return the `Comment` that was successfully posted.
-}
postComment :
    AuthenticationToken
    -> FileKey
    -> { message : String, position : Position }
    -> (Result Http.Error Comment -> msg)
    -> Cmd msg
postComment token fileKey comment handle =
    let
        url =
            baseUrl
                ++ "/v1/files/"
                ++ fileKey
                ++ "/comments"
    in
    Http.request
        { method = "POST"
        , headers = [ authHeader token ]
        , url = url
        , body = Http.jsonBody <| encodeComment comment
        , expect = Http.expectJson handle commentDecoder
        , timeout = Nothing
        , tracker = Nothing
        }



-- PROJECT


{-| A value which uniquely identifies a project.
-}
type alias ProjectId =
    Int


{-| A value which uniquely identifies a team.
-}
type alias TeamId =
    String


{-| Metadata for a project file.
-}
type alias FileMeta =
    { key : FileKey
    , name : String
    , thumbnailUrl : String
    , lastModified : Time.Posix
    }


{-| A single team project.
-}
type alias Project =
    { id : ProjectId
    , name : String
    }


{-| Construct a web request to return the list of files of the given project.
-}
getFiles :
    AuthenticationToken
    -> ProjectId
    -> (Result Http.Error (List FileMeta) -> msg)
    -> Cmd msg
getFiles token projectId handle =
    let
        url =
            baseUrl ++ "/v1/projects/" ++ String.fromInt projectId ++ "/files"
    in
    Http.request
        { method = "GET"
        , headers = [ authHeader token ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson handle filesDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


filesDecoder : Decoder (List FileMeta)
filesDecoder =
    D.field "files" (D.list fileMetaDecoder)


fileMetaDecoder : Decoder FileMeta
fileMetaDecoder =
    D.succeed FileMeta
        |> D.required "key" D.string
        |> D.required "name" D.string
        |> D.required "thumbnail_url" D.string
        |> D.required "last_modified" dateDecoder


{-| Construct a web request and return the list of projects of the given team.

Note that this will only return projects visible to the authenticated user
or owner of the developer token.

-}
getProjects : AuthenticationToken -> TeamId -> (Result Http.Error (List Project) -> msg) -> Cmd msg
getProjects token teamId handle =
    let
        url =
            baseUrl ++ "/v1/teams/" ++ teamId ++ "/projects"
    in
    Http.request
        { method = "GET"
        , headers = [ authHeader token ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson handle projectsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


projectsDecoder : Decoder (List Project)
projectsDecoder =
    D.field "files" (D.list projectDecoder)


projectDecoder : Decoder Project
projectDecoder =
    D.succeed Project
        |> D.required "key" D.int
        |> D.required "name" D.string



-- EXPORT


{-| Construct a web request to export a list of document nodes into PNG files at 1x resolution.

If you need to specify a different scale value use `exportNodesWithOptions`.

-}
exportNodesAsPng :
    AuthenticationToken
    -> FileKey
    -> List NodeId
    -> (Result Http.Error (List ExportedImage) -> msg)
    -> Cmd msg
exportNodesAsPng token fileKey ids handle =
    let
        options =
            { format = PngFormat, scale = 1.0 }
    in
    exportNodesWithOptions token fileKey options ids handle


{-| Construct a web request to export a list of document nodes into JPEG files at 1x resolution.

If you need to specify a different scale value use `exportNodesWithOptions`.

-}
exportNodesAsJpeg :
    AuthenticationToken
    -> FileKey
    -> List NodeId
    -> (Result Http.Error (List ExportedImage) -> msg)
    -> Cmd msg
exportNodesAsJpeg token fileKey ids handle =
    let
        options =
            { format = JpegFormat, scale = 1.0 }
    in
    exportNodesWithOptions token fileKey options ids handle


{-| Construct a web request to export a list of document nodes into SVG files at 1x resolution.

If you need to specify a different scale value use `exportNodesWithOptions`.

-}
exportNodesAsSvg :
    AuthenticationToken
    -> FileKey
    -> List NodeId
    -> (Result Http.Error (List ExportedImage) -> msg)
    -> Cmd msg
exportNodesAsSvg token fileKey ids handle =
    let
        options =
            { format = SvgFormat, scale = 1.0 }
    in
    exportNodesWithOptions token fileKey options ids handle


{-| Construct a web request to export a list of document nodes into the given `format` files using
the given `scale` factor automatically clamped within the 0.01–4 range.
-}
exportNodesWithOptions :
    AuthenticationToken
    -> FileKey
    -> { format : ExportFormat, scale : Float }
    -> List NodeId
    -> (Result Http.Error (List ExportedImage) -> msg)
    -> Cmd msg
exportNodesWithOptions token fileKey options ids handle =
    let
        format =
            formatToString options.format

        scale =
            clamp 0.01 4 options.scale |> String.fromFloat

        ids_ =
            String.join "," ids

        url =
            baseUrl
                ++ "/v1/images/"
                ++ fileKey
                ++ "?ids="
                ++ ids_
                ++ "&scale="
                ++ scale
                ++ "&format="
                ++ format
    in
    Http.request
        { method = "GET"
        , headers = [ authHeader token ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson handle exportDataDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


formatToString format =
    case format of
        JpegFormat ->
            "jpg"

        PngFormat ->
            "png"

        SvgFormat ->
            "svg"

        PdfFormat ->
            "pdf"


{-| A tuple made of the node ID and the image URL of its rendered representation.

A `Nothing` value indicates that rendering of the specific node has failed. This may be due to
the node id not existing, or other reasons such has the node having no renderable components.

-}
type alias ExportedImage =
    ( NodeId, Maybe String )


exportDataDecoder : Decoder (List ExportedImage)
exportDataDecoder =
    D.field "images" (D.keyValuePairs (D.nullable D.string))



-- COMMENTS


{-| A comment is either a "top comment" or a reply to it.
-}
type Comment
    = Comment CommentData
    | Reply ReplyData


{-| A comment left by a user.
-}
type alias CommentData =
    { id : String
    , message : String
    , fileKey : FileKey
    , position : Position
    , user : User
    , createdAt : Time.Posix
    , resolvedAt : Maybe Time.Posix
    , orderId : String
    }


{-| A reply to a comment.
-}
type alias ReplyData =
    { id : String
    , message : String
    , fileKey : FileKey
    , parentId : String -- TODO commentId
    , user : User
    , createdAt : Time.Posix
    , resolvedAt : Maybe Time.Posix
    }


commentsDecoder : Decoder (List Comment)
commentsDecoder =
    D.field "comments"
        (D.list
            (D.oneOf
                [ commentDecoder
                , replyDecoder
                ]
            )
        )


commentDecoder : Decoder Comment
commentDecoder =
    (D.succeed CommentData
        |> D.required "id" D.string
        |> D.required "message" D.string
        |> D.required "file_key" D.string
        |> D.required "client_meta" positionDecoder
        |> D.required "user" userDecoder
        |> D.required "created_at" dateDecoder
        |> D.required "resolved_at" (D.nullable dateDecoder)
        |> D.required "order_id" D.string
    )
        |> D.map Comment


replyDecoder : Decoder Comment
replyDecoder =
    (D.succeed ReplyData
        |> D.required "id" D.string
        |> D.required "message" D.string
        |> D.required "file_key" D.string
        |> D.required "parent_id" D.string
        |> D.required "user" userDecoder
        |> D.required "created_at" dateDecoder
        |> D.required "resolved_at" (D.nullable dateDecoder)
    )
        |> D.map Reply


encodeComment : { message : String, position : Position } -> E.Value
encodeComment comment =
    let
        position =
            case comment.position of
                AbsolutePosition point ->
                    encodePoint point

                RelativePositionTo frameId point ->
                    E.object
                        [ ( "node_id", E.string frameId )
                        , ( "node_offset", encodePoint point )
                        ]
    in
    E.object
        [ ( "message", E.string comment.message )
        , ( "client_meta", position )
        ]



-- USER


{-| A description of a user.
-}
type alias User =
    { handle : String
    , imageUrl : String
    }


userDecoder : Decoder User
userDecoder =
    D.succeed User
        |> D.required "handle" D.string
        |> D.required "img_url" D.string



-- MISC DECODERS


dateDecoder : Decoder Time.Posix
dateDecoder =
    Iso8601.decoder

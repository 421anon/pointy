module Components.Markdown exposing (plain, toHtml)

import Array exposing (Array)
import Components.Markdown.Math as Math
import Html exposing (Html)
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer


toHtml : (String -> Html msg) -> String -> List (Html msg)
toHtml toText raw =
    let
        protected =
            Math.protect raw
    in
    case Markdown.Parser.parse protected.markdown of
        Err _ ->
            [ Html.text raw ]

        Ok blocks ->
            Markdown.Renderer.render (renderer protected.spans toText) blocks
                |> Result.withDefault [ Html.text raw ]


plain : String -> List (Html msg)
plain =
    toHtml Html.text


renderer : Array Math.Span -> (String -> Html msg) -> Markdown.Renderer.Renderer (Html msg)
renderer mathSpans toText =
    let
        base =
            Markdown.Renderer.defaultHtmlRenderer

        restoreMath =
            Math.restore mathSpans
    in
    { base
        | html = htmlRenderer
        , text = Math.viewText mathSpans toText
        , codeSpan = restoreMath >> base.codeSpan
        , codeBlock = \block -> base.codeBlock { block | body = restoreMath block.body }
        , link = \link -> base.link { link | destination = restoreMath link.destination }
        , image = \image -> base.image { image | alt = restoreMath image.alt, src = restoreMath image.src }
    }


htmlRenderer : Markdown.Html.Renderer (List (Html msg) -> Html msg)
htmlRenderer =
    passthroughTags
        |> List.map (\tagName -> Markdown.Html.tag tagName (Html.node tagName []))
        |> Markdown.Html.oneOf


passthroughTags : List String
passthroughTags =
    [ "b"
    , "blockquote"
    , "br"
    , "code"
    , "dd"
    , "del"
    , "details"
    , "div"
    , "dl"
    , "dt"
    , "em"
    , "h1"
    , "h2"
    , "h3"
    , "h4"
    , "h5"
    , "h6"
    , "hr"
    , "i"
    , "ins"
    , "kbd"
    , "li"
    , "mark"
    , "ol"
    , "p"
    , "pre"
    , "s"
    , "samp"
    , "small"
    , "span"
    , "strong"
    , "sub"
    , "summary"
    , "sup"
    , "table"
    , "tbody"
    , "td"
    , "tfoot"
    , "th"
    , "thead"
    , "tr"
    , "u"
    , "ul"
    ]

module Components.Markdown exposing (plain, toHtml)


import Html exposing (Html)
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer


toHtml : (String -> Html msg) -> String -> List (Html msg)
toHtml toText raw =
    case Markdown.Parser.parse raw of
        Err _ ->
            [ Html.text raw ]

        Ok blocks ->
            Markdown.Renderer.render (renderer toText) blocks
                |> Result.withDefault [ Html.text raw ]


plain : String -> List (Html msg)
plain =
    toHtml Html.text


renderer : (String -> Html msg) -> Markdown.Renderer.Renderer (Html msg)
renderer toText =
    let
        base =
            Markdown.Renderer.defaultHtmlRenderer
    in
    { base | html = htmlRenderer, text = toText }


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

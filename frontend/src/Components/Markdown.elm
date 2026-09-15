module Components.Markdown exposing (plain, toHtml)

{-| Markdown rendering for text the app does not author itself: agent replies and
field notices.

Agent replies stream in token by token, so a partially written one can fail to
parse (a trailing `<div>` with no closing tag, for example). Parse failures and
raw HTML outside the allow-list fall back to the raw text, so the message stays
readable instead of disappearing.

-}

import Html exposing (Html)
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer


{-| Render `raw` as GFM markdown, with `toText` rendering every text run.
-}
toHtml : (String -> Html msg) -> String -> List (Html msg)
toHtml toText raw =
    case Markdown.Parser.parse raw of
        Err _ ->
            [ Html.text raw ]

        Ok blocks ->
            Markdown.Renderer.render (renderer toText) blocks
                |> Result.withDefault [ Html.text raw ]


{-| Render `raw` as GFM markdown with plain text runs.
-}
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


{-| Raw HTML renders only for the tags below, and without attributes, so agent
output cannot set classes, styles, or handlers on the app's DOM.
-}
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

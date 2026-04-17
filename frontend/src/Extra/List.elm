module Extra.List exposing (..)


prefixes : List a -> List (List a)
prefixes list =
    List.indexedMap (\i _ -> List.take (i + 1) list) list
        |> (::) []

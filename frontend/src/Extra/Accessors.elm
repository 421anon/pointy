module Extra.Accessors exposing (..)

import Accessors exposing (A_Lens, An_Optic, Lens, Traversal, all, each, get, lens, name, over, set, traversal)
import Basics.Extra exposing (flip)


type alias A_Traversal s a =
    Traversal s a a a


remkL : A_Lens pr a b -> Lens ls a b x y
remkL l =
    lens (name l) (get l) (flip <| set l)


remkT : An_Optic pr ls s a -> Traversal s a x y
remkT t =
    traversal (name t) (all t) (over t)


orElseT : An_Optic pr ls s a -> An_Optic pr ls s a -> Traversal s a x y
orElseT t1 t2 =
    traversal (name t1 ++ " or " ++ name t2)
        (\s ->
            if List.isEmpty (all t1 s) then
                all t2 s

            else
                all t1 s
        )
        (\f s ->
            if List.isEmpty (all t1 s) then
                over t2 f s

            else
                over t1 f s
        )


by : (a -> b) -> b -> Traversal (List a) a x y
by getkey key =
    each << where_ (getkey >> (==) key)


where_ : (a -> Bool) -> Traversal a a x y
where_ pred =
    traversal "where"
        (\s ->
            if pred s then
                [ s ]

            else
                []
        )
        (\f s ->
            if pred s then
                f s

            else
                s
        )

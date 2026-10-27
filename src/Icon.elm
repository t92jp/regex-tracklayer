module Icon exposing (Icon(..), view)

import Html exposing (Html)
import Html.Attributes as A
import Svg
import Svg.Attributes as SA


type Icon
    = Undo
    | Trash
    | Copy
    | Check
    | Cross
    | Fork
    | Tidy
    | Lazy
    | Plus
    | Signal
    | RedSignal
    | GreenSignal
    | Left
    | Right


view : Icon -> Html msg
view icon =
    Svg.svg [ SA.class "i", A.attribute "aria-hidden" "true" ]
        [ Svg.use [ A.attribute "href" ("#i-" ++ name icon) ] [] ]


name : Icon -> String
name icon =
    case icon of
        Undo ->
            "undo"

        Trash ->
            "trash"

        Copy ->
            "copy"

        Check ->
            "check"

        Cross ->
            "x"

        Fork ->
            "fork"

        Tidy ->
            "tidy"

        Lazy ->
            "lazy"

        Plus ->
            "plus"

        Signal ->
            "signal"

        RedSignal ->
            "sigR"

        GreenSignal ->
            "sigG"

        Left ->
            "left"

        Right ->
            "right"

module Panel exposing (Action(..), valueInputId, view)

import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Icon exposing (Icon)
import Json.Decode as D
import Track exposing (..)


type Action
    = ChoosePreset Preset
    | ToggleLazy
    | ToggleNegated
    | SetLight Light
    | SetWatch Watch
    | Delete
    | MakeBranch
    | AddLane
    | RemoveLane
    | EditText String
    | EditMin Int
    | EditMax (Maybe Int)
    | EnterPressed
    | FocusLeft


view : { track : Track, selection : Maybe Selection, choosingCustomRepeat : Bool } -> Html Action
view { track, selection, choosingCustomRepeat } =
    case selection of
        Just (OnGap gap) ->
            if canRemoveLane track.line gap then
                panel [ row [ chip Icon.Cross "Remove this lane" "danger" RemoveLane ] ]

            else
                hidden

        Just (OnStation spot) ->
            case stopAt spot track.line of
                Just (SignalStop g) ->
                    panel (signalRows g)

                Just (StationStop s) ->
                    panel (stationRows choosingCustomRepeat s)

                Nothing ->
                    hidden

        Nothing ->
            hidden


valueInputId : String
valueInputId =
    "val"


maxCount : Int
maxCount =
    999


panel : List (Html Action) -> Html Action
panel =
    Html.div [ A.id "panel", E.on "focusout" (D.succeed FocusLeft) ]


hidden : Html msg
hidden =
    Html.div [ A.id "panel", A.hidden True ] []


canRemoveLane : Line -> Spot -> Bool
canRemoveLane root gap =
    case List.reverse gap.line of
        ( i, BranchLane _ ) :: outer ->
            lineAt gap.line root == Just [] && laneCount { line = List.reverse outer, index = i } root > 1

        _ ->
            False


signalRows : Signal -> List (Html Action)
signalRows g =
    [ row
        [ segment
            [ iconToggle (g.light == Red) (SetLight Red) Icon.RedSignal "Red signal" "Red: stop if this matches" [ A.attribute "data-look" "neg" ]
            , iconToggle (g.light == Green) (SetLight Green) Icon.GreenSignal "Green signal" "Green: go only if this matches" [ A.attribute "data-look" "pos" ]
            ]
        , segment
            [ iconToggle (g.watch == Ahead) (SetWatch Ahead) Icon.Right "Watch ahead" "Watch ahead (lookahead)" [ A.attribute "data-look" "ahead" ]
            , iconToggle (g.watch == Behind) (SetWatch Behind) Icon.Left "Watch behind" "Watch behind (lookbehind)" [ A.attribute "data-look" "behind" ]
            ]
        , spacer
        , chip Icon.Trash "Delete" "danger" Delete
        ]
    ]


stationRows : Bool -> Station -> List (Html Action)
stationRows choosingCustomRepeat s =
    let
        current =
            preset s.repeat

        customizing =
            choosingCustomRepeat || current == Custom

        canBeLazy =
            current /= Once && s.repeat.max /= Just s.repeat.min
    in
    valueRow s.kind
        ++ row
            ([ segment
                (List.map (\p -> toggle (current == p && not customizing) (ChoosePreset p) [] [ Html.text (presetSymbol p) ]) [ Once, Optional, AnyTimes, AtLeastOnce ]
                    ++ [ toggle customizing (ChoosePreset Custom) [] [ Html.text (presetSymbol Custom) ] ]
                )
             ]
                ++ (if canBeLazy then
                        [ iconToggle s.lazy ToggleLazy Icon.Lazy "Exit first (lazy)" "Switch: exit first (lazy)" [ A.attribute "aria-pressed" (boolText s.lazy) ] ]

                    else
                        []
                   )
                ++ [ spacer
                   , case s.kind of
                        Branch _ ->
                            chip Icon.Plus "Add lane" "" AddLane

                        _ ->
                            chip Icon.Fork "Make a branch" "" MakeBranch
                   , chip Icon.Trash "Delete" "danger" Delete
                   ]
            )
        :: (if customizing then
                [ row
                    [ numberInput "rmin" "Minimum" "0" [] (String.fromInt s.repeat.min) (parseCount 0 >> Maybe.withDefault 0 >> EditMin)
                    , Html.span [ A.class "dash" ] [ Html.text "–" ]
                    , numberInput "rmax" "Maximum" "1" [ A.placeholder "∞" ] (Maybe.map String.fromInt s.repeat.max |> Maybe.withDefault "") (parseCount 1 >> EditMax)
                    ]
                ]

            else
                []
           )


valueRow : Kind -> List (Html Action)
valueRow kind =
    case kind of
        Text str ->
            [ row [ textInput "Text" str ] ]

        CharSet set ->
            [ row
                [ textInput "Character set" set.chars
                , toggle set.negated ToggleNegated [ A.title "Negate (anything but)", A.attribute "aria-label" "Negate" ] [ Html.text "^" ]
                ]
            ]

        _ ->
            []


presetSymbol : Preset -> String
presetSymbol p =
    case p of
        Once ->
            "1"

        Optional ->
            "?"

        AnyTimes ->
            "*"

        AtLeastOnce ->
            "+"

        Custom ->
            "{ }"


row : List (Html msg) -> Html msg
row =
    Html.div [ A.class "row" ]


segment : List (Html msg) -> Html msg
segment =
    Html.div [ A.class "seg" ]


spacer : Html msg
spacer =
    Html.span [ A.class "sp" ] []


toggle : Bool -> msg -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
toggle on msg attrs =
    Html.button (A.classList [ ( "b", True ), ( "on", on ) ] :: E.onClick msg :: attrs)


iconToggle : Bool -> msg -> Icon -> String -> String -> List (Html.Attribute msg) -> Html msg
iconToggle on msg icon ariaLabel title attrs =
    Html.button
        ([ A.class
            (if on then
                "b ic on"

             else
                "b ic"
            )
         , A.title title
         , A.attribute "aria-label" ariaLabel
         , E.onClick msg
         ]
            ++ attrs
        )
        [ Icon.view icon ]


chip : Icon -> String -> String -> msg -> Html msg
chip icon title extra msg =
    Html.button [ A.class ("b " ++ extra), A.title title, A.attribute "aria-label" title, E.onClick msg ] [ Icon.view icon ]


initialValueOnly : String -> Html.Attribute msg
initialValueOnly =
    A.attribute "value"


textInput : String -> String -> Html Action
textInput ariaLabel value =
    Html.input
        [ A.id valueInputId
        , A.class "txt"
        , A.type_ "text"
        , initialValueOnly value
        , A.attribute "autocomplete" "off"
        , A.attribute "autocapitalize" "off"
        , A.attribute "autocorrect" "off"
        , A.spellcheck False
        , A.attribute "aria-label" ariaLabel
        , E.onInput EditText
        , E.on "keydown"
            (D.field "key" D.string
                |> D.andThen
                    (\key ->
                        if key == "Enter" then
                            D.succeed EnterPressed

                        else
                            D.fail "not Enter"
                    )
            )
        ]
        []


numberInput : String -> String -> String -> List (Html.Attribute Action) -> String -> (String -> Action) -> Html Action
numberInput id ariaLabel lowest extra value onInput =
    Html.input
        ([ A.id id
         , A.class "num"
         , A.type_ "number"
         , A.min lowest
         , A.max (String.fromInt maxCount)
         , A.attribute "inputmode" "numeric"
         , initialValueOnly value
         , A.attribute "aria-label" ariaLabel
         , E.onInput onInput
         ]
            ++ extra
        )
        []


parseCount : Int -> String -> Maybe Int
parseCount lowest text =
    if text == "" then
        Nothing

    else
        Just (leadingInt text |> Maybe.withDefault lowest |> clamp lowest maxCount)


leadingInt : String -> Maybe Int
leadingInt text =
    let
        trimmed =
            String.trimLeft text

        ( sign, rest ) =
            if String.startsWith "-" trimmed then
                ( -1, String.dropLeft 1 trimmed )

            else if String.startsWith "+" trimmed then
                ( 1, String.dropLeft 1 trimmed )

            else
                ( 1, trimmed )

        digits chars =
            case chars of
                c :: more ->
                    if Char.isDigit c then
                        c :: digits more

                    else
                        []

                [] ->
                    []
    in
    String.toList rest |> digits |> String.fromList |> String.toInt |> Maybe.map ((*) sign)


boolText : Bool -> String
boolText b =
    if b then
        "true"

    else
        "false"

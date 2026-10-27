module Check exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as A
import Json.Decode as D
import Json.Encode as E
import Pattern
import Save
import Tidy
import Track exposing (..)


type alias Result =
    { name : String, failure : Maybe String }


expect : String -> a -> a -> Result
expect name expected actual =
    { name = name
    , failure =
        if expected == actual then
            Nothing

        else
            Just ("expected: " ++ Debug.toString expected ++ "\n    actual:   " ++ Debug.toString actual)
    }


text : String -> Stop
text str =
    station (Text str)


branch : List Line -> Stop
branch lanes =
    station (Branch lanes)


at : Int -> Spot
at i =
    { line = [], index = i }


pattern : Line -> String
pattern line =
    Pattern.toString { anchoredStart = False, anchoredEnd = False, line = line }


abc : Line
abc =
    [ text "a", text "b", text "c" ]


editing : List Result
editing =
    [ expect "insert" "axbc" (pattern (insert (at 1) (text "x") abc))
    , expect "remove" "bc" (pattern (remove (at 0) abc))
    , expect "branchWith puts the new stop beside the old one" "(?:a|x)bc" (pattern (branchWith (text "x") (at 0) abc))
    , expect "wrapInBranch adds an empty lane" "(?:a|)bc" (pattern (wrapInBranch (at 0) abc))
    , expect "addLane" "a|b|x" (pattern (addLane [ text "x" ] (at 0) [ branch [ [ text "a" ], [ text "b" ] ] ]))
    , expect "removeLane unwraps a branch left with one lane" "ac" (pattern (removeLane (at 0) 1 [ branch [ [ text "a" ], [ text "b" ] ], text "c" ]))
    , expect "removeLane keeps a repeated branch"
        "(?:a)+"
        (pattern (removeLane (at 0) 1 [ StationStop { kind = Branch [ [ text "a" ], [ text "b" ] ], repeat = { min = 1, max = Nothing }, lazy = False, detour = [], loop = [] } ]))
    , expect "move forward" (Just ( "bca", at 2 )) (move (at 0) (IntoGap (at 3)) abc |> Maybe.map (Tuple.mapFirst pattern))
    , expect "move backward" (Just ( "cab", at 0 )) (move (at 2) (IntoGap (at 0)) abc |> Maybe.map (Tuple.mapFirst pattern))
    , expect "move to an adjacent gap does nothing" Nothing (move (at 1) (IntoGap (at 2)) abc)
    , expect "move into a later branch shifts its index"
        (Just ( "xa|b", { line = [ ( 0, BranchLane 0 ) ], index = 0 } ))
        (move (at 0) (IntoGap { line = [ ( 1, BranchLane 0 ) ], index = 0 }) [ text "x", branch [ [ text "a" ], [ text "b" ] ] ] |> Maybe.map (Tuple.mapFirst pattern))
    , expect "move into itself is refused"
        Nothing
        (move (at 0) (IntoGap { line = [ ( 0, BranchLane 0 ) ], index = 0 }) [ branch [ [ text "a" ], [ text "b" ] ] ])
    , expect "move into a new lane"
        (Just ( "a|b|x", { line = [ ( 0, BranchLane 2 ) ], index = 0 } ))
        (move (at 0) (IntoNewLane (at 1)) [ text "x", branch [ [ text "a" ], [ text "b" ] ] ] |> Maybe.map (Tuple.mapFirst pattern))
    , expect "railShape" [ LoopBack, SkipOver, RunParallel ] [ railShape 2 1 False, railShape 0 2 False, railShape 0 2 True ]
    , expect "drawRail loops a single station" (Just ( "ab+c", OnStation (at 1) )) (drawRail LoopBack [] 2 1 abc |> Maybe.map (Tuple.mapFirst pattern))
    , expect "drawRail makes two stations skippable" (Just ( "(?:ab)?c", OnStation (at 0) )) (drawRail SkipOver [] 0 2 abc |> Maybe.map (Tuple.mapFirst pattern))
    , expect "drawRail adds a parallel lane"
        (Just ( "(?:a|)bc", OnGap { line = [ ( 0, BranchLane 1 ) ], index = 0 } ))
        (drawRail RunParallel [] 0 1 abc |> Maybe.map (Tuple.mapFirst pattern))
    , expect "drawRail over no stations does nothing" Nothing (drawRail SkipOver [] 1 1 abc)
    , expect "pruneLanes drops side lines that cannot be used"
        [ text "a" ]
        (pruneLanes [ StationStop { kind = Text "a", repeat = once, lazy = False, detour = [ text "b" ], loop = [ text "c" ] } ])
    , expect "tidy joins texts" [ text "abc" ] (Tidy.tidy abc)
    , expect "tidy turns single-character lanes into a set" "[ab]" (pattern (Tidy.tidy [ branch [ [ text "a" ], [ text "b" ] ] ]))
    , expect "save and load round trip" (Just Track.pluralNounsExample) (Save.decode (Save.encode { track = Track.pluralNounsExample, testText = "" }) |> Maybe.map .track)
    , expect "saves of another version are ignored" Nothing (Save.decode "{\"version\":0,\"track\":{},\"testText\":\"\"}")
    ]


type alias Case =
    { input : Track
    , pattern : String
    , tidied : Track
    , tidiedPattern : String
    }


caseDecoder : D.Decoder Case
caseDecoder =
    D.map4 Case
        (D.field "input" Save.trackDecoder)
        (D.field "pattern" D.string)
        (D.field "tidied" Save.trackDecoder)
        (D.field "tidiedPattern" D.string)


fixture : Int -> Case -> List Result
fixture i c =
    let
        track =
            { anchoredStart = c.input.anchoredStart, anchoredEnd = c.input.anchoredEnd, line = pruneLanes c.input.line }

        tidied =
            Tidy.tidy track.line

        name what =
            "fixture #" ++ String.fromInt i ++ " " ++ what
    in
    [ expect (name "pattern") c.pattern (Pattern.toString track)
    , expect (name "tidy") (E.encode 0 (Save.encodeTrack c.tidied)) (E.encode 0 (Save.encodeTrack { track | line = tidied }))
    , expect (name "tidied pattern") c.tidiedPattern (Pattern.toString { track | line = tidied })
    , expect (name "save round trip") (Ok track) (D.decodeValue Save.trackDecoder (Save.encodeTrack track) |> Result.mapError D.errorToString)
    ]


fixtures : D.Value -> List Result
fixtures value =
    case D.decodeValue (D.list caseDecoder) value of
        Ok cases ->
            List.concat (List.indexedMap fixture cases)

        Err err ->
            [ { name = "fixtures.js", failure = Just (D.errorToString err) } ]


view : List Result -> Html msg
view results =
    let
        failures =
            List.filterMap (\r -> Maybe.map (\f -> r.name ++ "\n    " ++ f) r.failure) results

        summary =
            String.fromInt (List.length results) ++ " checks, " ++ String.fromInt (List.length failures) ++ " failures"
    in
    Html.main_ [ A.style "font" "14px/1.5 ui-monospace, monospace", A.style "padding" "16px" ]
        (Html.h1
            [ A.id "summary"
            , A.style "color"
                (if List.isEmpty failures then
                    "#12805A"

                 else
                    "#C23B2A"
                )
            ]
            [ Html.text summary ]
            :: List.map (\f -> Html.pre [ A.class "failure", A.style "white-space" "pre-wrap" ] [ Html.text f ]) failures
        )


main : Program D.Value (List Result) msg
main =
    Browser.element
        { init = \flags -> ( editing ++ fixtures flags, Cmd.none )
        , view = view
        , update = \_ results -> ( results, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }

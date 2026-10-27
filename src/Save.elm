module Save exposing (Saved, decode, encode, encodeTrack, trackDecoder)

import Json.Decode as D
import Json.Encode as E
import Track exposing (..)


type alias Saved =
    { track : Track, testText : String }


formatVersion : Int
formatVersion =
    1


encode : Saved -> String
encode saved =
    E.encode 0
        (E.object
            [ ( "version", E.int formatVersion )
            , ( "track", encodeTrack saved.track )
            , ( "testText", E.string saved.testText )
            ]
        )


decode : String -> Maybe Saved
decode json =
    D.decodeString
        (D.field "version" D.int
            |> D.andThen
                (\version ->
                    if version == formatVersion then
                        D.map2 Saved (D.field "track" trackDecoder) (D.field "testText" D.string)

                    else
                        D.fail ("unsupported version " ++ String.fromInt version)
                )
        )
        json
        |> Result.toMaybe


classNames : List ( String, CharClass )
classNames =
    [ ( "digit", Digit ), ( "word", WordChar ), ( "space", Whitespace ), ( "any", AnyChar ) ]


lightNames : List ( String, Light )
lightNames =
    [ ( "red", Red ), ( "green", Green ) ]


watchNames : List ( String, Watch )
watchNames =
    [ ( "ahead", Ahead ), ( "behind", Behind ) ]


nameIn : List ( String, a ) -> a -> String
nameIn names value =
    List.filter (\( _, v ) -> v == value) names
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.withDefault ""


namedIn : List ( String, a ) -> D.Decoder a
namedIn names =
    D.string
        |> D.andThen
            (\name ->
                case List.filter (\( n, _ ) -> n == name) names of
                    ( _, value ) :: _ ->
                        D.succeed value

                    [] ->
                        D.fail ("unknown name: " ++ name)
            )


encodeTrack : Track -> E.Value
encodeTrack track =
    E.object
        [ ( "anchoredStart", E.bool track.anchoredStart )
        , ( "anchoredEnd", E.bool track.anchoredEnd )
        , ( "line", encodeLine track.line )
        ]


encodeLine : Line -> E.Value
encodeLine =
    E.list encodeStop


encodeStop : Stop -> E.Value
encodeStop stop =
    case stop of
        StationStop s ->
            E.object (kindFields s.kind ++ nonDefaultStationFields s)

        SignalStop g ->
            E.object
                [ ( "kind", E.string "signal" )
                , ( "light", E.string (nameIn lightNames g.light) )
                , ( "watch", E.string (nameIn watchNames g.watch) )
                , ( "line", encodeLine g.line )
                ]


kindFields : Kind -> List ( String, E.Value )
kindFields kind =
    case kind of
        Text text ->
            [ ( "kind", E.string "text" ), ( "text", E.string text ) ]

        Class c ->
            [ ( "kind", E.string "class" ), ( "class", E.string (nameIn classNames c) ) ]

        CharSet set ->
            [ ( "kind", E.string "set" ), ( "chars", E.string set.chars ), ( "negated", E.bool set.negated ) ]

        Branch lanes ->
            [ ( "kind", E.string "branch" ), ( "lanes", E.list encodeLine lanes ) ]


nonDefaultStationFields : Station -> List ( String, E.Value )
nonDefaultStationFields s =
    List.concat
        [ if s.repeat == once then
            []

          else
            [ ( "min", E.int s.repeat.min ), ( "max", Maybe.map E.int s.repeat.max |> Maybe.withDefault E.null ) ]
        , if s.lazy then
            [ ( "lazy", E.bool True ) ]

          else
            []
        , if List.isEmpty s.detour then
            []

          else
            [ ( "detour", encodeLine s.detour ) ]
        , if List.isEmpty s.loop then
            []

          else
            [ ( "loop", encodeLine s.loop ) ]
        ]


trackDecoder : D.Decoder Track
trackDecoder =
    D.map3 Track
        (D.field "anchoredStart" D.bool)
        (D.field "anchoredEnd" D.bool)
        (D.field "line" lineDecoder)


lineDecoder : D.Decoder Line
lineDecoder =
    D.list (D.lazy (\_ -> stopDecoder))


stopDecoder : D.Decoder Stop
stopDecoder =
    D.field "kind" D.string
        |> D.andThen
            (\kind ->
                case kind of
                    "text" ->
                        stationDecoder (D.map Text (D.field "text" D.string))

                    "class" ->
                        stationDecoder (D.map Class (D.field "class" (namedIn classNames)))

                    "set" ->
                        stationDecoder
                            (D.map2 (\chars negated -> CharSet { chars = chars, negated = negated })
                                (D.field "chars" D.string)
                                (D.field "negated" D.bool)
                            )

                    "branch" ->
                        stationDecoder (D.map Branch (D.field "lanes" (D.list lineDecoder)))

                    "signal" ->
                        D.map3 Signal
                            (D.field "light" (namedIn lightNames))
                            (D.field "watch" (namedIn watchNames))
                            (D.field "line" lineDecoder)
                            |> D.map SignalStop

                    _ ->
                        D.fail ("unknown kind: " ++ kind)
            )


stationDecoder : D.Decoder Kind -> D.Decoder Stop
stationDecoder kindDecoder =
    D.map5 (\kind repeat lazy detour loop -> StationStop { kind = kind, repeat = repeat, lazy = lazy, detour = detour, loop = loop })
        kindDecoder
        (D.oneOf
            [ D.map2 Repeat (D.field "min" D.int) (D.field "max" (D.nullable D.int))
            , D.succeed once
            ]
        )
        (fieldOr "lazy" D.bool False)
        (fieldOr "detour" lineDecoder [])
        (fieldOr "loop" lineDecoder [])


fieldOr : String -> D.Decoder a -> a -> D.Decoder a
fieldOr name decoder default =
    D.oneOf [ D.field name decoder, D.succeed default ]

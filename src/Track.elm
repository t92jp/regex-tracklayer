module Track exposing
    ( Track, Line, Stop(..), Station, Kind(..), CharClass(..), Signal, Light(..), Watch(..)
    , pluralNounsExample, empty, station, Tool(..), newStop, classCode
    , Repeat, Preset(..), once, effectiveBounds, canSkip, canLoop, countVaries, quantifier, quantifierOf, preset, presetRepeat, loopCountLabel
    , Lane(..), LinePath, Spot, Selection(..), laneIn, lineAt, stopAt, updateLine, updateStop, updateStation
    , insert, remove, branchWith, wrapInBranch, addLane, laneCount, removeLane
    , Destination(..), move, RailShape(..), railShape, drawRail, pruneLanes
    )


type alias Track =
    { anchoredStart : Bool
    , anchoredEnd : Bool
    , line : Line
    }


type alias Line =
    List Stop


type Stop
    = StationStop Station
    | SignalStop Signal


type alias Station =
    { kind : Kind
    , repeat : Repeat
    , lazy : Bool
    , detour : Line
    , loop : Line
    }


type Kind
    = Text String
    | Class CharClass
    | CharSet { chars : String, negated : Bool }
    | Branch (List Line)


type CharClass
    = Digit
    | WordChar
    | Whitespace
    | AnyChar


type alias Signal =
    { light : Light
    , watch : Watch
    , line : Line
    }


type Light
    = Red
    | Green


type Watch
    = Ahead
    | Behind


empty : Track
empty =
    { anchoredStart = False, anchoredEnd = False, line = [] }


station : Kind -> Stop
station kind =
    StationStop { kind = kind, repeat = once, lazy = False, detour = [], loop = [] }


pluralNounsExample : Track
pluralNounsExample =
    let
        wordEdge watch =
            SignalStop { light = Red, watch = watch, line = [ station (Class WordChar) ] }

        text str =
            station (Text str)

        anyOf chars =
            station (CharSet { chars = chars, negated = False })

        noneOf chars =
            station (CharSet { chars = chars, negated = True })

        letters =
            StationStop { kind = CharSet { chars = "a-z", negated = False }, repeat = { min = 1, max = Nothing }, lazy = False, detour = [], loop = [] }
    in
    { anchoredStart = False
    , anchoredEnd = False
    , line =
        [ wordEdge Behind
        , letters
        , station
            (Branch
                [ [ station (Branch [ [ anyOf "sxzo" ], [ text "ch" ], [ text "sh" ] ]), text "es" ]
                , [ noneOf "aeiou", text "ies" ]
                , [ text "ves" ]
                , [ noneOf "s", text "s" ]
                ]
            )
        , wordEdge Ahead
        ]
    }


type Tool
    = TextTool
    | ClassTool CharClass
    | SetTool
    | SignalTool


newStop : Tool -> Stop
newStop tool =
    case tool of
        TextTool ->
            station (Text "a")

        ClassTool c ->
            station (Class c)

        SetTool ->
            station (CharSet { chars = "a-z", negated = False })

        SignalTool ->
            SignalStop { light = Red, watch = Ahead, line = [] }


classCode : CharClass -> String
classCode c =
    case c of
        Digit ->
            "\\d"

        WordChar ->
            "\\w"

        Whitespace ->
            "\\s"

        AnyChar ->
            "."


type alias Repeat =
    { min : Int, max : Maybe Int }


once : Repeat
once =
    { min = 1, max = Just 1 }


effectiveBounds : Repeat -> ( Int, Maybe Int )
effectiveBounds r =
    let
        lo =
            Basics.max 0 r.min
    in
    ( lo, Maybe.map (\hi -> Basics.max hi (Basics.max lo 1)) r.max )


canSkip : Repeat -> Bool
canSkip r =
    Tuple.first (effectiveBounds r) == 0


canLoop : Repeat -> Bool
canLoop r =
    case Tuple.second (effectiveBounds r) of
        Nothing ->
            True

        Just hi ->
            hi > 1


countVaries : Repeat -> Bool
countVaries r =
    let
        ( lo, hi ) =
            effectiveBounds r
    in
    hi /= Just lo


quantifier : Repeat -> String
quantifier r =
    let
        ( lo, hi ) =
            effectiveBounds r
    in
    quantifierOf lo hi


quantifierOf : Int -> Maybe Int -> String
quantifierOf lo hi =
    case ( lo, hi ) of
        ( 1, Just 1 ) ->
            ""

        ( 0, Just 1 ) ->
            "?"

        ( 0, Nothing ) ->
            "*"

        ( 1, Nothing ) ->
            "+"

        ( _, Nothing ) ->
            "{" ++ String.fromInt lo ++ ",}"

        ( _, Just h ) ->
            if lo == h then
                "{" ++ String.fromInt lo ++ "}"

            else
                "{" ++ String.fromInt lo ++ "," ++ String.fromInt h ++ "}"


type Preset
    = Once
    | Optional
    | AnyTimes
    | AtLeastOnce
    | Custom


preset : Repeat -> Preset
preset r =
    case effectiveBounds r of
        ( 1, Just 1 ) ->
            Once

        ( 0, Just 1 ) ->
            Optional

        ( 0, Nothing ) ->
            AnyTimes

        ( 1, Nothing ) ->
            AtLeastOnce

        _ ->
            Custom


presetRepeat : Preset -> Maybe Repeat
presetRepeat p =
    case p of
        Once ->
            Just once

        Optional ->
            Just { min = 0, max = Just 1 }

        AnyTimes ->
            Just { min = 0, max = Nothing }

        AtLeastOnce ->
            Just { min = 1, max = Nothing }

        Custom ->
            Nothing


loopCountLabel : Repeat -> String
loopCountLabel r =
    case effectiveBounds r of
        ( lo, Nothing ) ->
            if lo > 1 then
                String.fromInt lo ++ "+"

            else
                ""

        ( lo, Just hi ) ->
            if lo == hi then
                String.fromInt lo ++ "×"

            else
                String.fromInt lo ++ "–" ++ String.fromInt hi


type Lane
    = BranchLane Int
    | Detour
    | Loop
    | SignalLine


type alias LinePath =
    List ( Int, Lane )


type alias Spot =
    { line : LinePath, index : Int }


type Selection
    = OnStation Spot
    | OnGap Spot


laneIn : Spot -> Lane -> LinePath
laneIn spot lane =
    spot.line ++ [ ( spot.index, lane ) ]


laneOf : Lane -> Stop -> Maybe Line
laneOf lane stop =
    case ( stop, lane ) of
        ( StationStop s, BranchLane j ) ->
            case s.kind of
                Branch lanes ->
                    getAt j lanes

                _ ->
                    Nothing

        ( StationStop s, Detour ) ->
            Just s.detour

        ( StationStop s, Loop ) ->
            Just s.loop

        ( SignalStop g, SignalLine ) ->
            Just g.line

        _ ->
            Nothing


mapLane : Lane -> (Line -> Line) -> Stop -> Stop
mapLane lane f stop =
    case ( stop, lane ) of
        ( StationStop s, BranchLane j ) ->
            case s.kind of
                Branch lanes ->
                    StationStop { s | kind = Branch (updateAt j f lanes) }

                _ ->
                    stop

        ( StationStop s, Detour ) ->
            StationStop { s | detour = f s.detour }

        ( StationStop s, Loop ) ->
            StationStop { s | loop = f s.loop }

        ( SignalStop g, SignalLine ) ->
            SignalStop { g | line = f g.line }

        _ ->
            stop


lineAt : LinePath -> Line -> Maybe Line
lineAt path line =
    case path of
        [] ->
            Just line

        ( i, lane ) :: rest ->
            getAt i line
                |> Maybe.andThen (laneOf lane)
                |> Maybe.andThen (lineAt rest)


updateLine : LinePath -> (Line -> Line) -> Line -> Line
updateLine path f line =
    case path of
        [] ->
            f line

        ( i, lane ) :: rest ->
            updateAt i (mapLane lane (updateLine rest f)) line


stopAt : Spot -> Line -> Maybe Stop
stopAt spot root =
    lineAt spot.line root |> Maybe.andThen (getAt spot.index)


updateStop : Spot -> (Stop -> Stop) -> Line -> Line
updateStop spot f =
    updateLine spot.line (updateAt spot.index f)


updateStation : Spot -> (Station -> Station) -> Line -> Line
updateStation spot f =
    updateStop spot
        (\stop ->
            case stop of
                StationStop s ->
                    StationStop (f s)

                SignalStop _ ->
                    stop
        )


insert : Spot -> Stop -> Line -> Line
insert gap stop =
    updateLine gap.line (insertAt gap.index stop)


remove : Spot -> Line -> Line
remove spot =
    updateLine spot.line (removeAt spot.index)


branchWith : Stop -> Spot -> Line -> Line
branchWith new spot =
    updateStop spot (\old -> station (Branch [ [ old ], [ new ] ]))


wrapInBranch : Spot -> Line -> Line
wrapInBranch spot =
    updateStop spot (\old -> station (Branch [ [ old ], [] ]))


addLane : Line -> Spot -> Line -> Line
addLane content branch =
    updateStation branch
        (\s ->
            case s.kind of
                Branch lanes ->
                    { s | kind = Branch (lanes ++ [ content ]) }

                _ ->
                    s
        )


laneCount : Spot -> Line -> Int
laneCount branch root =
    case stopAt branch root of
        Just (StationStop { kind }) ->
            case kind of
                Branch lanes ->
                    List.length lanes

                _ ->
                    0

        _ ->
            0


removeLane : Spot -> Int -> Line -> Line
removeLane branch j root =
    let
        removed =
            updateStation branch
                (\s ->
                    case s.kind of
                        Branch lanes ->
                            { s | kind = Branch (removeAt j lanes) }

                        _ ->
                            s
                )
                root
    in
    case stopAt branch removed of
        Just (StationStop s) ->
            case s.kind of
                Branch [ only ] ->
                    if isOnce s.repeat && List.isEmpty s.detour && List.isEmpty s.loop then
                        updateLine branch.line (splice branch.index 1 only) removed

                    else
                        removed

                _ ->
                    removed

        _ ->
            removed


type Destination
    = IntoGap Spot
    | IntoNewLane Spot


move : Spot -> Destination -> Line -> Maybe ( Line, Spot )
move from destination root =
    case stopAt from root of
        Nothing ->
            Nothing

        Just stop ->
            case destination of
                IntoGap gap ->
                    if isNestedIn from gap.line || isGapBeside from gap then
                        Nothing

                    else
                        let
                            target =
                                afterRemoving from gap
                        in
                        Just ( root |> remove from |> insert target stop, target )

                IntoNewLane branch ->
                    if branch == from || isNestedIn from branch.line then
                        Nothing

                    else
                        let
                            target =
                                afterRemoving from branch

                            moved =
                                root |> remove from |> addLane [ stop ] target
                        in
                        Just ( moved, { line = laneIn target (BranchLane (laneCount target moved - 1)), index = 0 } )


isNestedIn : Spot -> LinePath -> Bool
isNestedIn spot path =
    let
        n =
            List.length spot.line
    in
    List.take n path == spot.line && Maybe.map Tuple.first (getAt n path) == Just spot.index


isGapBeside : Spot -> Spot -> Bool
isGapBeside stop gap =
    gap.line == stop.line && (gap.index == stop.index || gap.index == stop.index + 1)


afterRemoving : Spot -> Spot -> Spot
afterRemoving removed spot =
    let
        n =
            List.length removed.line

        shift i =
            if i > removed.index then
                i - 1

            else
                i
    in
    if spot.line == removed.line then
        { spot | index = shift spot.index }

    else if List.take n spot.line == removed.line then
        { spot
            | line =
                List.take n spot.line
                    ++ List.indexedMap
                        (\k ( i, lane ) ->
                            if k == 0 then
                                ( shift i, lane )

                            else
                                ( i, lane )
                        )
                        (List.drop n spot.line)
        }

    else
        spot


type RailShape
    = SkipOver
    | RunParallel
    | LoopBack


railShape : Int -> Int -> Bool -> RailShape
railShape from to below =
    if to < from then
        LoopBack

    else if below then
        RunParallel

    else
        SkipOver


drawRail : RailShape -> LinePath -> Int -> Int -> Line -> Maybe ( Line, Selection )
drawRail shape path from to root =
    let
        lo =
            Basics.min from to

        hi =
            Basics.max from to

        span =
            lineAt path root |> Maybe.withDefault [] |> List.drop lo |> List.take (hi - lo)

        here =
            { line = path, index = lo }

        replaceSpan stop =
            updateLine path (splice lo (hi - lo) [ stop ]) root

        repeatSpan repeat =
            case span of
                [ StationStop _ ] ->
                    updateStation here (\st -> { st | repeat = repeat st.repeat }) root

                _ ->
                    replaceSpan (StationStop { kind = Branch [ span ], repeat = repeat once, lazy = False, detour = [], loop = [] })
    in
    if List.isEmpty span then
        Nothing

    else
        case shape of
            LoopBack ->
                Just ( repeatSpan (\r -> { r | max = Nothing }), OnStation here )

            SkipOver ->
                Just ( repeatSpan (\r -> { r | min = 0 }), OnStation here )

            RunParallel ->
                Just ( replaceSpan (station (Branch [ span, [] ])), OnGap { line = laneIn here (BranchLane 1), index = 0 } )


pruneLanes : Line -> Line
pruneLanes =
    List.map
        (\stop ->
            case stop of
                StationStop s ->
                    StationStop
                        { s
                            | kind =
                                case s.kind of
                                    Branch lanes ->
                                        Branch (List.map pruneLanes lanes)

                                    other ->
                                        other
                            , detour =
                                if canSkip s.repeat then
                                    pruneLanes s.detour

                                else
                                    []
                            , loop =
                                if canLoop s.repeat then
                                    pruneLanes s.loop

                                else
                                    []
                        }

                SignalStop g ->
                    SignalStop { g | line = pruneLanes g.line }
        )


isOnce : Repeat -> Bool
isOnce r =
    effectiveBounds r == ( 1, Just 1 )


getAt : Int -> List a -> Maybe a
getAt i xs =
    if i < 0 then
        Nothing

    else
        List.head (List.drop i xs)


updateAt : Int -> (a -> a) -> List a -> List a
updateAt i f =
    List.indexedMap
        (\k x ->
            if k == i then
                f x

            else
                x
        )


insertAt : Int -> a -> List a -> List a
insertAt i x xs =
    List.take i xs ++ x :: List.drop i xs


removeAt : Int -> List a -> List a
removeAt i xs =
    List.take i xs ++ List.drop (i + 1) xs


splice : Int -> Int -> List a -> List a -> List a
splice i n ys xs =
    List.take i xs ++ ys ++ List.drop (i + n) xs

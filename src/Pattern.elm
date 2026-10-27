module Pattern exposing (Role(..), Token, signalTokens, toString, tokens)

import Track exposing (..)


type alias Token =
    { text : String, role : Role }


type Role
    = TextRole
    | ClassRole
    | SetRole
    | GroupRole
    | SignalRole
    | AnchorRole


type Context
    = Alone
    | WithNeighbors


toString : Track -> String
toString track =
    String.concat (List.map .text (tokens track))


tokens : Track -> List Token
tokens track =
    let
        context =
            if track.anchoredStart || track.anchoredEnd then
                WithNeighbors

            else
                Alone
    in
    tokenIf track.anchoredStart "^" AnchorRole
        ++ lineTokens context track.line
        ++ tokenIf track.anchoredEnd "$" AnchorRole


lineTokens : Context -> Line -> List Token
lineTokens context line =
    case ( context, line ) of
        ( Alone, [ StationStop ({ kind } as s) ] ) ->
            case kind of
                Branch lanes ->
                    if quantifier s.repeat == "" then
                        alternatives lanes

                    else
                        stationTokens s

                _ ->
                    stationTokens s

        _ ->
            List.concatMap stopTokens line


stopTokens : Stop -> List Token
stopTokens stop =
    case stop of
        StationStop s ->
            stationTokens s

        SignalStop g ->
            signalTokens g


alternatives : List Line -> List Token
alternatives lanes =
    oneOf (List.map (lineTokens Alone) lanes)


oneOf : List (List Token) -> List Token
oneOf routes =
    List.concat (List.intersperse [ Token "|" GroupRole ] routes)


group : Role -> List Token -> List Token
group role inner =
    Token "(?:" role :: inner ++ [ Token ")" role ]


signalTokens : Signal -> List Token
signalTokens g =
    case lineTokens Alone g.line of
        [] ->
            []

        body ->
            let
                lookbehindMark =
                    if g.watch == Behind then
                        "<"

                    else
                        ""

                assertion =
                    if g.light == Red then
                        "!"

                    else
                        "="
            in
            Token ("(?" ++ lookbehindMark ++ assertion) SignalRole :: body ++ [ Token ")" SignalRole ]


type alias Core =
    { tokens : List Token, takesQuantifierDirectly : Bool }


coreOf : Kind -> Core
coreOf kind =
    case kind of
        Text str ->
            Core [ Token (escape str) TextRole ] (String.length str == 1)

        Class c ->
            Core [ Token (classCode c) ClassRole ] True

        CharSet set ->
            let
                negation =
                    if set.negated then
                        "^"

                    else
                        ""
            in
            Core [ Token ("[" ++ negation ++ set.chars ++ "]") SetRole ] True

        Branch lanes ->
            Core (group GroupRole (alternatives lanes)) True


roleOf : Kind -> Role
roleOf kind =
    case kind of
        Text _ ->
            TextRole

        Class _ ->
            ClassRole

        CharSet _ ->
            SetRole

        Branch _ ->
            GroupRole


stationTokens : Station -> List Token
stationTokens s =
    case s.kind of
        Text "" ->
            []

        _ ->
            let
                core =
                    coreOf s.kind

                ( lo, hi ) =
                    effectiveBounds s.repeat

                lazy =
                    s.lazy && countVaries s.repeat

                hasLoop =
                    canLoop s.repeat && not (List.isEmpty s.loop)

                hasDetour =
                    lo == 0 && not (List.isEmpty s.detour)

                quantifierFor min max =
                    case quantifierOf min max of
                        "" ->
                            ""

                        q ->
                            if lazy && max /= Just min then
                                q ++ "?"

                            else
                                q

                repeatedCore min max =
                    case quantifierFor min max of
                        "" ->
                            core.tokens

                        q ->
                            if core.takesQuantifierDirectly then
                                core.tokens ++ [ Token q (roleOf s.kind) ]

                            else
                                group TextRole core.tokens ++ [ Token q (roleOf s.kind) ]

                throughLoop =
                    let
                        extraLaps =
                            quantifierFor (Basics.max lo 1 - 1) (Maybe.map (\h -> h - 1) hi)
                    in
                    core.tokens
                        ++ group GroupRole (lineTokens WithNeighbors s.loop ++ core.tokens)
                        ++ tokenIf (extraLaps /= "") extraLaps GroupRole

                mainRoute =
                    if hasLoop then
                        throughLoop

                    else if hasDetour then
                        repeatedCore (Basics.max lo 1) hi

                    else
                        repeatedCore lo hi

                detourRoute =
                    lineTokens Alone s.detour

                optional =
                    if lazy then
                        "??"

                    else
                        "?"
            in
            if hasDetour && lazy then
                group GroupRole (oneOf [ detourRoute, mainRoute ])

            else if hasDetour then
                group GroupRole (oneOf [ mainRoute, detourRoute ])

            else if lo == 0 && hasLoop then
                group GroupRole mainRoute ++ [ Token optional GroupRole ]

            else
                mainRoute


tokenIf : Bool -> String -> Role -> List Token
tokenIf condition text role =
    if condition then
        [ Token text role ]

    else
        []


escape : String -> String
escape =
    String.toList
        >> List.concatMap
            (\c ->
                if String.contains (String.fromChar c) ".*+?^${}()|[]\\/" then
                    [ '\\', c ]

                else
                    [ c ]
            )
        >> String.fromList

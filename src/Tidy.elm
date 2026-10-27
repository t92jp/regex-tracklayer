module Tidy exposing (canTidy, tidy)

import Pattern
import Track exposing (..)


tidy : Line -> Line
tidy =
    repeatUntilStable 12 (simplify >> pruneLanes)


canTidy : Line -> Bool
canTidy line =
    tidy line /= line


repeatUntilStable : Int -> (a -> a) -> a -> a
repeatUntilStable maxRounds step value =
    let
        next =
            step value
    in
    if maxRounds <= 1 || next == value then
        next

    else
        repeatUntilStable (maxRounds - 1) step next


simplify : Line -> Line
simplify line =
    List.foldl simplifyStop [] line |> List.reverse


simplifyStop : Stop -> List Stop -> List Stop
simplifyStop stop reversedResult =
    case stop of
        SignalStop signal ->
            let
                simplified =
                    { signal | line = simplify signal.line }
            in
            if List.isEmpty (Pattern.signalTokens simplified) then
                reversedResult

            else
                appendMerging (SignalStop simplified) reversedResult

        StationStop original ->
            let
                s =
                    simplifyInnerLines original
            in
            case s.kind of
                Text "" ->
                    reversedResult

                Branch lanes ->
                    simplifyBranch s lanes reversedResult

                _ ->
                    appendMerging (StationStop s) reversedResult


simplifyInnerLines : Station -> Station
simplifyInnerLines s =
    { s
        | detour = simplify s.detour
        , loop = simplify s.loop
        , kind =
            case s.kind of
                Branch lanes ->
                    Branch (List.map simplify lanes)

                other ->
                    other
    }


appendMerging : Stop -> List Stop -> List Stop
appendMerging stop reversedResult =
    case ( stop, reversedResult ) of
        ( StationStop next, (StationStop previous) :: earlier ) ->
            case joinTexts previous next |> orElse (\() -> sumRepeats previous next) of
                Just merged ->
                    StationStop merged :: earlier

                Nothing ->
                    stop :: reversedResult

        _ ->
            stop :: reversedResult


appendAllMerging : List Stop -> List Stop -> List Stop
appendAllMerging stops reversedResult =
    List.foldl appendMerging reversedResult stops


joinTexts : Station -> Station -> Maybe Station
joinTexts previous next =
    case ( previous.kind, next.kind ) of
        ( Text a, Text b ) ->
            if isPlain previous && isPlain next then
                Just { previous | kind = Text (a ++ b) }

            else
                Nothing

        _ ->
            Nothing


sumRepeats : Station -> Station -> Maybe Station
sumRepeats previous next =
    let
        sameCharacter =
            singleCharacterKind previous /= Nothing && singleCharacterKind previous == singleCharacterKind next

        ( a1, b1 ) =
            effectiveBounds previous.repeat

        ( a2, b2 ) =
            effectiveBounds next.repeat
    in
    if sameCharacter && isGreedyMainLine previous && isGreedyMainLine next then
        Just { previous | repeat = { min = a1 + a2, max = Maybe.map2 (+) b1 b2 } }

    else
        Nothing


type Collapsed
    = Unwrapped Line
    | Replaced Stop
    | Vanished


simplifyBranch : Station -> List Line -> List Stop -> List Stop
simplifyBranch branch lanes reversedResult =
    let
        standalone =
            isPlain branch

        ( prefix, afterPrefix ) =
            factorOutSharedPrefix standalone (unique lanes)

        ( suffix, afterSuffix ) =
            if standalone then
                factorOutSharedSuffix afterPrefix

            else
                ( [], afterPrefix )

        withPrefix =
            appendAllMerging prefix reversedResult
    in
    case collapse (emptyLastLaneToOptional standalone { branch | kind = Branch afterSuffix }) of
        Unwrapped lane ->
            withPrefix |> appendAllMerging lane |> appendAllMerging suffix

        Replaced stop ->
            withPrefix |> appendMerging stop |> appendAllMerging suffix

        Vanished ->
            withPrefix


factorOutSharedPrefix : Bool -> List Line -> ( Line, List Line )
factorOutSharedPrefix standalone lanes =
    case sharedPrefixLength lanes of
        0 ->
            ( [], lanes )

        k ->
            let
                shared =
                    List.take k (firstLane lanes)

                rest =
                    List.map (List.drop k) lanes
            in
            if standalone then
                ( shared, rest )

            else
                ( [], [ shared ++ [ station (Branch rest) ] ] )


factorOutSharedSuffix : List Line -> ( Line, List Line )
factorOutSharedSuffix lanes =
    case sharedPrefixLength (List.map List.reverse lanes) of
        0 ->
            ( [], lanes )

        k ->
            ( takeLast k (firstLane lanes), List.map (dropLast k) lanes )


emptyLastLaneToOptional : Bool -> Station -> Station
emptyLastLaneToOptional standalone branch =
    case List.reverse (lanesOf branch) of
        [] :: others ->
            if standalone && not (List.isEmpty others) && List.all (not << List.isEmpty) others then
                { branch | kind = Branch (List.reverse others), repeat = { min = 0, max = Just 1 } }

            else
                branch

        _ ->
            branch


collapse : Station -> Collapsed
collapse branch =
    case lanesOf branch of
        [ only ] ->
            if isPlain branch then
                Unwrapped only

            else
                case only of
                    [ StationStop inner ] ->
                        moveRepeatInto branch inner |> Maybe.withDefault (Replaced (StationStop branch))

                    _ ->
                        Replaced (StationStop branch)

        several ->
            case ( several, charSetOf several ) of
                ( _ :: _ :: _, Just chars ) ->
                    Replaced (StationStop { branch | kind = CharSet { chars = chars, negated = False } })

                _ ->
                    Replaced (StationStop branch)


moveRepeatInto : Station -> Station -> Maybe Collapsed
moveRepeatInto branch inner =
    let
        ( a1, b1 ) =
            effectiveBounds inner.repeat

        ( a2, b2 ) =
            effectiveBounds branch.repeat

        hi =
            Maybe.map2 (*) b1 b2
    in
    if isGreedyMainLine branch && isGreedyMainLine inner && a1 <= 1 then
        if hi == Just 0 then
            Just Vanished

        else
            Just (Replaced (StationStop { inner | repeat = { min = a1 * a2, max = hi } }))

    else
        Nothing


charSetOf : List Line -> Maybe String
charSetOf lanes =
    lanes
        |> List.map
            (\lane ->
                case lane of
                    [ StationStop s ] ->
                        asCharSetMember s

                    _ ->
                        Nothing
            )
        |> List.foldr (Maybe.map2 (++)) (Just "")


asCharSetMember : Station -> Maybe String
asCharSetMember s =
    if not (isPlain s) then
        Nothing

    else
        case s.kind of
            Text str ->
                if String.length str /= 1 then
                    Nothing

                else if String.contains str "]\\^-" then
                    Just ("\\" ++ str)

                else
                    Just str

            Class AnyChar ->
                Nothing

            Class c ->
                Just (classCode c)

            CharSet set ->
                if set.negated || set.chars == "" then
                    Nothing

                else
                    Just set.chars

            Branch _ ->
                Nothing


isPlain : Station -> Bool
isPlain s =
    effectiveBounds s.repeat == ( 1, Just 1 ) && hasNoSideLines s


isGreedyMainLine : Station -> Bool
isGreedyMainLine s =
    hasNoSideLines s && not s.lazy


hasNoSideLines : Station -> Bool
hasNoSideLines s =
    List.isEmpty s.detour && List.isEmpty s.loop


singleCharacterKind : Station -> Maybe Kind
singleCharacterKind s =
    case s.kind of
        Text str ->
            if String.length str == 1 then
                Just s.kind

            else
                Nothing

        Branch _ ->
            Nothing

        _ ->
            Just s.kind


lanesOf : Station -> List Line
lanesOf s =
    case s.kind of
        Branch lanes ->
            lanes

        _ ->
            []


sharedPrefixLength : List Line -> Int
sharedPrefixLength lanes =
    if List.length lanes < 2 then
        0

    else
        let
            first =
                firstLane lanes

            shortest =
                List.minimum (List.map List.length lanes) |> Maybe.withDefault 0

            stopAtIndex k lane =
                List.head (List.drop k lane)

            allSameAt k =
                List.all (\lane -> stopAtIndex k lane == stopAtIndex k first) lanes

            longestRun k =
                if k < shortest && allSameAt k then
                    longestRun (k + 1)

                else
                    k

            onlyLastLaneBecomesEmpty k =
                List.map (List.drop k) lanes
                    |> List.reverse
                    |> List.drop 1
                    |> List.all (not << List.isEmpty)

            shortenUntilValid k =
                if k > 0 && not (onlyLastLaneBecomesEmpty k) then
                    shortenUntilValid (k - 1)

                else
                    k
        in
        shortenUntilValid (longestRun 0)


firstLane : List Line -> Line
firstLane =
    List.head >> Maybe.withDefault []


orElse : (() -> Maybe a) -> Maybe a -> Maybe a
orElse fallback first =
    case first of
        Just _ ->
            first

        Nothing ->
            fallback ()


unique : List a -> List a
unique =
    List.foldl
        (\x seen ->
            if List.member x seen then
                seen

            else
                seen ++ [ x ]
        )
        []


takeLast : Int -> List a -> List a
takeLast k xs =
    List.drop (List.length xs - k) xs


dropLast : Int -> List a -> List a
dropLast k xs =
    List.take (List.length xs - k) xs

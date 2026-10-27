module Diagram exposing
    ( Settings, Pointer, pointerDecoder
    , Hotspot(..), Grip(..), Event(..), hotspotId
    , Decor, Mark(..), view, label, stopClass
    , Targets, StationTarget, PointTarget, targets
    )

import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as D
import Svg exposing (Svg)
import Svg.Attributes as SA
import Track exposing (..)


type alias Settings =
    { gapWidth : Float }


curveRadius : Float
curveRadius =
    8


stationHeight : Float
stationHeight =
    34


margin : Float
margin =
    6


terminalWidth : Float
terminalWidth =
    56


type Part
    = Rail String
    | ThinRail String
    | RailMask String
    | Stub String
    | LoopArrow String
    | LoopCount { x : Float, y : Float, text : String }
    | StationBox { spot : Spot, x : Float, y : Float, w : Float, h : Float, kind : Kind }
    | Gap { spot : Spot, x : Float, y : Float, w : Float, empty : Bool, reversed : Bool }
    | BranchFrame { spot : Spot, x : Float, top : Float, w : Float, h : Float }
    | LaneAdder { branch : Spot, x : Float, y : Float }
    | SignalPost { spot : Spot, signal : Signal, x : Float, y : Float, bubbleY : Float, bubble : String, rail : String }


type alias Box =
    { width : Float
    , above : Float
    , below : Float
    , place : Float -> Float -> List Part
    }


type alias Layout =
    { width : Float, height : Float, baseline : Float, parts : List Part }


layout : Settings -> Track -> Layout
layout settings track =
    let
        box =
            lineBox settings [] False track.line

        above =
            max box.above 17

        below =
            max box.below 17
    in
    { width = margin * 2 + terminalWidth * 2 + box.width
    , height = 12 + above + below + 12
    , baseline = 12 + above
    , parts = box.place (margin + terminalWidth) (12 + above)
    }


lineBox : Settings -> LinePath -> Bool -> Line -> Box
lineBox settings linePath reversed line =
    let
        at i =
            { line = linePath, index = i }

        items =
            if List.isEmpty line then
                [ gapBox settings (at 0) True False ]

            else
                List.concat (List.indexedMap (\i stop -> [ gapBox settings (at i) False reversed, stopBox settings (at i) stop ]) line)
                    ++ [ gapBox settings (at (List.length line)) False reversed ]

        ordered =
            if reversed then
                List.reverse items

            else
                items
    in
    { width = List.foldl (\b sum -> sum + b.width) 0 ordered
    , above = List.foldl (\b h -> max h b.above) 14 ordered
    , below = List.foldl (\b h -> max h b.below) 14 ordered
    , place =
        \x y ->
            List.foldl (\b ( parts, left ) -> ( parts ++ b.place left y, left + b.width )) ( [], x ) ordered
                |> Tuple.first
    }


gapBox : Settings -> Spot -> Bool -> Bool -> Box
gapBox settings spot empty reversed =
    let
        w =
            if empty then
                56

            else
                settings.gapWidth
    in
    { width = w
    , above = 17
    , below = 17
    , place = \x y -> [ Gap { spot = spot, x = x, y = y, w = w, empty = empty, reversed = reversed } ]
    }


stopBox : Settings -> Spot -> Stop -> Box
stopBox settings spot stop =
    case stop of
        SignalStop g ->
            signalBox settings spot g

        StationStop s ->
            withSideLines settings spot s <|
                case s.kind of
                    Branch lanes ->
                        branchBox settings spot lanes

                    _ ->
                        stationBox spot s.kind


stationBox : Spot -> Kind -> Box
stationBox spot kind =
    let
        w =
            max 36 (textWidth (label kind) + 24)
    in
    { width = w
    , above = stationHeight / 2
    , below = stationHeight / 2
    , place = \x y -> [ StationBox { spot = spot, x = x, y = y, w = w, h = stationHeight, kind = kind } ]
    }


branchBox : Settings -> Spot -> List Line -> Box
branchBox settings spot lanes =
    let
        pad =
            20

        laneGap =
            10

        boxes =
            List.indexedMap (\j lane -> lineBox settings (laneIn spot (BranchLane j)) False lane) lanes

        widest =
            List.foldl (\b w -> max w b.width) 0 boxes

        width =
            widest + 2 * pad

        offsets =
            List.map2 Tuple.pair boxes (List.drop 1 boxes)
                |> List.foldl (\( prev, next ) ys -> ys ++ [ lastOr 0 ys + prev.below + laneGap + next.above ]) [ 0 ]

        lastOffset =
            lastOr 0 offsets

        firstAbove =
            List.head boxes |> Maybe.map .above |> Maybe.withDefault 14

        adderY =
            lastOffset + (List.reverse boxes |> List.head |> Maybe.map .below |> Maybe.withDefault 14) + laneGap + 8
    in
    { width = width
    , above = firstAbove + 12
    , below = adderY + 14
    , place =
        \x y ->
            let
                top =
                    y - firstAbove - 10

                bottom =
                    y + adderY + 10

                rails =
                    if List.length boxes > 1 then
                        let
                            lastY =
                                y + lastOffset

                            middle =
                                offsets
                                    |> List.drop 1
                                    |> List.reverse
                                    |> List.drop 1
                                    |> List.reverse
                                    |> List.map
                                        (\dy ->
                                            Rail (path [ M (x + curveRadius) (y + dy - curveRadius), Arc 0 curveRadius curveRadius, H (x + width - 2 * curveRadius), Arc 0 curveRadius (-curveRadius) ])
                                        )
                        in
                        Rail (path [ M x y, Arc 1 curveRadius curveRadius, V (lastY - curveRadius), Arc 0 curveRadius curveRadius, H (x + width - 2 * curveRadius), Arc 0 curveRadius (-curveRadius), V (y + curveRadius), Arc 1 curveRadius (-curveRadius) ])
                            :: middle
                            ++ [ Stub (path [ M (x + curveRadius) (lastY - curveRadius), V (y + adderY - 9) ]) ]

                    else
                        []

                laneParts =
                    List.map2 (\b dy -> b.place (x + pad + (widest - b.width) / 2) (y + dy)) boxes offsets
                        |> List.concat
            in
            BranchFrame { spot = spot, x = x + 1, top = top, w = width - 2, h = bottom - top }
                :: rails
                ++ laneParts
                ++ [ LaneAdder { branch = spot, x = x + curveRadius, y = y + adderY } ]
    }


signalBox : Settings -> Spot -> Signal -> Box
signalBox settings spot g =
    let
        inner =
            lineBox settings (laneIn spot SignalLine) False g.line

        post =
            15

        width =
            inner.width + 45

        bubbleAbove =
            inner.above + 4

        bubbleBelow =
            inner.below + 4

        bubbleOffset =
            -(post + 9 + bubbleBelow)
    in
    { width = width
    , above = -bubbleOffset + bubbleAbove
    , below = 17
    , place =
        \x y ->
            let
                bubbleY =
                    y + bubbleOffset

                postX =
                    if g.watch == Behind then
                        x + width - 18

                    else
                        x + 18

                left =
                    x + 2

                right =
                    x + width - 2

                top =
                    bubbleY - bubbleAbove

                bottom =
                    bubbleY + bubbleBelow

                bubble =
                    path
                        [ M (left + 8) top
                        , H (right - 8)
                        , Arc 1 8 8
                        , V (bottom - 8)
                        , Arc 1 (-8) 8
                        , H (postX + 6)
                        , L postX (y - post - 1)
                        , L (postX - 6) bottom
                        , H (left + 8)
                        , Arc 1 (-8) (-8)
                        , V (top + 8)
                        , Arc 1 8 (-8)
                        , Close
                        ]

                railInBubble =
                    if g.watch == Behind then
                        path [ M (left + 6) bubbleY, H (postX - 6) ]

                    else
                        path [ M (postX + 6) bubbleY, H (right - 6) ]

                innerX =
                    if g.watch == Behind then
                        postX - 15 - inner.width

                    else
                        postX + 15
            in
            SignalPost { spot = spot, signal = g, x = postX, y = y, bubbleY = bubbleY, bubble = bubble, rail = railInBubble }
                :: inner.place innerX bubbleY
    }


withSideLines : Settings -> Spot -> Station -> Box -> Box
withSideLines settings spot s inner =
    let
        sidePad =
            16

        detour =
            if canSkip s.repeat then
                Just (lineBox settings (laneIn spot Detour) False s.detour)

            else
                Nothing

        loop =
            if canLoop s.repeat then
                Just (lineBox settings (laneIn spot Loop) True s.loop)

            else
                Nothing

        count =
            if loop /= Nothing then
                loopCountLabel s.repeat

            else
                ""

        width =
            max inner.width (max (widthOf detour) (widthOf loop)) + 2 * sidePad

        detourOffset =
            Maybe.map (\b -> -(inner.above + 8 + b.below)) detour |> Maybe.withDefault 0

        loopOffset =
            Maybe.map (\b -> inner.below + 8 + b.above) loop |> Maybe.withDefault 0

        lazy =
            s.lazy && countVaries s.repeat
    in
    case ( detour, loop ) of
        ( Nothing, Nothing ) ->
            inner

        _ ->
            { width = width
            , above = Maybe.map (\b -> -detourOffset + b.above) detour |> Maybe.withDefault inner.above
            , below =
                Maybe.map
                    (\b ->
                        loopOffset
                            + b.below
                            + (if count == "" then
                                0

                               else
                                16
                              )
                    )
                    loop
                    |> Maybe.withDefault inner.below
            , place =
                \x y ->
                    let
                        thinMain =
                            if lazy && detour /= Nothing then
                                [ RailMask (path [ M (x + 1) y, H (x + width - 1) ]), ThinRail (path [ M x y, H (x + width) ]) ]

                            else
                                []

                        detourParts =
                            case detour of
                                Just b ->
                                    let
                                        dy =
                                            y + detourOffset
                                    in
                                    Rail (path [ M x y, Arc 0 curveRadius (-curveRadius), V (dy + curveRadius), Arc 1 curveRadius (-curveRadius), H (x + width - 2 * curveRadius), Arc 1 curveRadius curveRadius, V (y - curveRadius), Arc 0 curveRadius curveRadius ])
                                        :: b.place (x + (width - b.width) / 2) dy

                                Nothing ->
                                    []

                        loopParts =
                            case loop of
                                Just b ->
                                    let
                                        ly =
                                            y + loopOffset

                                        ax =
                                            x + width - curveRadius

                                        ay =
                                            y + loopOffset / 2

                                        back =
                                            path [ M (x + width - 2 * curveRadius) y, Arc 1 curveRadius curveRadius, V (ly - curveRadius), Arc 1 (-curveRadius) curveRadius, H (x + 2 * curveRadius), Arc 1 (-curveRadius) (-curveRadius), V (y + curveRadius), Arc 1 curveRadius (-curveRadius) ]
                                    in
                                    (if lazy then
                                        ThinRail back

                                     else
                                        Rail back
                                    )
                                        :: LoopArrow (path [ M (ax - 4.5) (ay - 3), L (ax + 4.5) (ay - 3), L ax (ay + 4), Close ])
                                        :: b.place (x + (width - b.width) / 2) ly
                                        ++ (if count == "" then
                                                []

                                            else
                                                [ LoopCount { x = x + width / 2, y = ly + b.below + 10, text = count } ]
                                           )

                                Nothing ->
                                    []
                    in
                    thinMain ++ detourParts ++ loopParts ++ inner.place (x + (width - inner.width) / 2) y
            }


widthOf : Maybe Box -> Float
widthOf =
    Maybe.map .width >> Maybe.withDefault 0


label : Kind -> String
label kind =
    case kind of
        Text "" ->
            "…"

        Text str ->
            String.replace " " "·" str

        Class c ->
            classCode c

        CharSet set ->
            "["
                ++ (if set.negated then
                        "^"

                    else
                        ""
                   )
                ++ set.chars
                ++ "]"

        Branch _ ->
            ""


stopClass : Stop -> String
stopClass stop =
    case stop of
        StationStop { kind } ->
            kindClass kind

        SignalStop _ ->
            "look"


kindClass : Kind -> String
kindClass kind =
    case kind of
        Text _ ->
            "lit"

        Class _ ->
            "cls"

        CharSet _ ->
            "set"

        Branch _ ->
            "alt"


textWidth : String -> Float
textWidth =
    String.foldl
        (\c w ->
            w
                + (if Char.toCode c > 0x2E7F then
                    13

                   else
                    7.9
                  )
        )
        0


lastOr : a -> List a -> a
lastOr fallback xs =
    List.reverse xs |> List.head |> Maybe.withDefault fallback


type Segment
    = M Float Float
    | L Float Float
    | H Float
    | HBy Float
    | V Float
    | Arc Int Float Float
    | Close


path : List Segment -> String
path =
    List.map
        (\seg ->
            case seg of
                M x y ->
                    "M" ++ num x ++ " " ++ num y

                L x y ->
                    "L" ++ num x ++ " " ++ num y

                H x ->
                    "H" ++ num x

                HBy dx ->
                    "h" ++ num dx

                V y ->
                    "V" ++ num y

                Arc sweep dx dy ->
                    "a" ++ num curveRadius ++ " " ++ num curveRadius ++ " 0 0 " ++ String.fromInt sweep ++ " " ++ num dx ++ " " ++ num dy

                Close ->
                    "z"
        )
        >> String.concat


num : Float -> String
num =
    String.fromFloat


plusSign : Float -> Float -> String
plusSign x y =
    "M" ++ num (x - 3) ++ " " ++ num y ++ "h6M" ++ num x ++ " " ++ num (y - 3) ++ "v6"


type alias StationTarget =
    { spot : Spot, x : Float, y : Float, w : Float, h : Float }


type alias PointTarget =
    { spot : Spot, x : Float, y : Float }


type alias Targets =
    { stations : List StationTarget, gaps : List PointTarget, adders : List PointTarget }


targets : Settings -> Track -> Targets
targets settings track =
    targetsOf (layout settings track).parts


targetsOf : List Part -> Targets
targetsOf parts =
    List.foldr
        (\part t ->
            case part of
                StationBox b ->
                    { t | stations = { spot = b.spot, x = b.x, y = b.y, w = b.w, h = b.h } :: t.stations }

                Gap g ->
                    { t | gaps = { spot = g.spot, x = g.x + g.w / 2, y = g.y } :: t.gaps }

                LaneAdder a ->
                    { t | adders = { spot = a.branch, x = a.x, y = a.y } :: t.adders }

                _ ->
                    t
        )
        { stations = [], gaps = [], adders = [] }
        parts


type alias Pointer =
    { x : Float, y : Float, id : Int, touch : Bool, button : Int }


pointerDecoder : D.Decoder Pointer
pointerDecoder =
    D.map5 Pointer
        (D.field "clientX" D.float)
        (D.field "clientY" D.float)
        (D.field "pointerId" D.int)
        (D.field "pointerType" (D.map ((==) "touch") D.string))
        (D.field "button" D.int)


type Hotspot
    = StartTerminal
    | EndTerminal
    | StationAt Spot
    | GapAt Spot
    | AdderAt Spot


type Grip
    = GripStation Spot
    | GripGap Spot


type Event
    = Clicked Hotspot
    | KeyPressed Hotspot
    | Grabbed Grip Pointer
    | FocusIn String
    | FocusOut String
    | ContextMenuBlocked


hotspotId : Hotspot -> String
hotspotId hotspot =
    case hotspot of
        StartTerminal ->
            "d-start"

        EndTerminal ->
            "d-end"

        StationAt spot ->
            "d-station-" ++ spotKey spot

        GapAt spot ->
            "d-gap-" ++ spotKey spot

        AdderAt spot ->
            "d-adder-" ++ spotKey spot


spotKey : Spot -> String
spotKey spot =
    let
        lane l =
            case l of
                BranchLane j ->
                    "b" ++ String.fromInt j

                Detour ->
                    "d"

                Loop ->
                    "l"

                SignalLine ->
                    "s"
    in
    String.join "/" (List.map (\( i, l ) -> String.fromInt i ++ lane l) spot.line ++ [ String.fromInt spot.index ])


hotspotAttrs : (Event -> msg) -> Hotspot -> String -> List (Svg.Attribute msg)
hotspotAttrs toMsg hotspot ariaLabel =
    let
        id =
            hotspotId hotspot
    in
    [ A.attribute "data-a"
        (case hotspot of
            StartTerminal ->
                "start"

            EndTerminal ->
                "goal"

            StationAt _ ->
                "n"

            GapAt _ ->
                "s"

            AdderAt _ ->
                "addlane"
        )
    , A.id id
    , A.attribute "tabindex" "0"
    , A.attribute "role" "button"
    , A.attribute "aria-label" ariaLabel
    , E.stopPropagationOn "click" (D.succeed ( toMsg (Clicked hotspot), True ))
    , E.preventDefaultOn "keydown"
        (D.field "key" D.string
            |> D.andThen
                (\key ->
                    if key == "Enter" || key == " " then
                        D.succeed ( toMsg (KeyPressed hotspot), True )

                    else
                        D.fail "not an activation key"
                )
        )
    , E.on "focus" (D.succeed (toMsg (FocusIn id)))
    , E.on "blur" (D.succeed (toMsg (FocusOut id)))
    ]


onGrab : (Event -> msg) -> Grip -> Svg.Attribute msg
onGrab toMsg grip =
    E.on "pointerdown" (D.map (toMsg << Grabbed grip) pointerDecoder)


type alias Decor =
    { selection : Maybe Selection
    , lifted : Maybe Spot
    , heldLong : Maybe Grip
    , dragging : Bool
    , mark : Maybe Mark
    }


type Mark
    = MarkGap Spot
    | MarkStation Spot
    | MarkAdder Spot
    | MarkRail { fromX : Float, toX : Float, y : Float, below : Bool }


view : (Event -> msg) -> Settings -> Decor -> Track -> Html msg
view toMsg settings decor track =
    let
        l =
            layout settings track
    in
    Svg.svg
        [ SA.id "rail"
        , SA.class
            (if decor.dragging then
                "dragging"

             else
                ""
            )
        , A.attribute "role" "group"
        , A.attribute "aria-label" "Track diagram"
        , SA.width (num l.width)
        , SA.height (num l.height)
        , SA.viewBox ("0 0 " ++ num l.width ++ " " ++ num l.height)
        , E.preventDefaultOn "contextmenu" (D.succeed ( toMsg ContextMenuBlocked, True ))
        ]
        (Svg.defs [] (fadeOut l "start" (margin + 26) (margin + terminalWidth - 6) ++ fadeOut l "goal" (l.width - margin - 26) (l.width - margin - terminalWidth + 6))
            :: rail (path [ M (margin + terminalWidth - 2) l.baseline, H (l.width - margin - terminalWidth + 2) ])
            ++ List.concatMap (viewPart toMsg decor) l.parts
            ++ [ terminal toMsg l True track.anchoredStart, terminal toMsg l False track.anchoredEnd ]
            ++ (if decor.dragging then
                    [ Svg.g [ SA.id "mk", A.attribute "pointer-events" "none" ] (marks (targetsOf l.parts) decor.mark) ]

                else
                    []
               )
        )


rail : String -> List (Svg msg)
rail d =
    [ Svg.path [ SA.class "tie", SA.d d ] [], Svg.path [ SA.class "rail", SA.d d ] [] ]


classes : List ( String, Bool ) -> Svg.Attribute msg
classes =
    List.filter Tuple.second >> List.map Tuple.first >> String.join " " >> SA.class


viewPart : (Event -> msg) -> Decor -> Part -> List (Svg msg)
viewPart toMsg decor part =
    case part of
        Rail d ->
            rail d

        ThinRail d ->
            [ Svg.path [ SA.class "thin", SA.d d ] [] ]

        RailMask d ->
            [ Svg.path [ SA.class "offmask", SA.d d ] [] ]

        Stub d ->
            [ Svg.path [ SA.class "stub", SA.d d ] [] ]

        LoopArrow d ->
            [ Svg.path [ SA.class "arw", SA.d d ] [] ]

        LoopCount c ->
            [ Svg.text_ [ SA.class "lbl", SA.x (num c.x), SA.y (num c.y), SA.textAnchor "middle" ] [ Svg.text c.text ] ]

        StationBox b ->
            [ viewStation toMsg decor b ]

        Gap g ->
            [ viewGap toMsg decor g ]

        BranchFrame f ->
            [ Svg.g
                (classes
                    [ ( "alt", True )
                    , ( "sel", decor.selection == Just (OnStation f.spot) )
                    , ( "lift", decor.lifted == Just f.spot )
                    , ( "armed", decor.heldLong == Just (GripStation f.spot) )
                    ]
                    :: hotspotAttrs toMsg (StationAt f.spot) "Branch"
                )
                [ Svg.rect [ SA.class "frame", SA.x (num f.x), SA.y (num f.top), SA.width (num f.w), SA.height (num f.h), SA.rx "14", onGrab toMsg (GripStation f.spot) ] [] ]
            ]

        LaneAdder a ->
            [ Svg.g (SA.class "add" :: hotspotAttrs toMsg (AdderAt a.branch) "Add lane")
                [ Svg.circle [ SA.class "hit", SA.cx (num a.x), SA.cy (num a.y), SA.r "14" ] []
                , Svg.circle [ SA.class "dot", SA.cx (num a.x), SA.cy (num a.y), SA.r "8" ] []
                , Svg.path [ SA.d (plusSign a.x a.y) ] []
                ]
            ]

        SignalPost p ->
            [ viewSignal toMsg decor p ]


viewStation : (Event -> msg) -> Decor -> { spot : Spot, x : Float, y : Float, w : Float, h : Float, kind : Kind } -> Svg msg
viewStation toMsg decor b =
    let
        selected =
            decor.selection == Just (OnStation b.spot)

        css =
            kindClass b.kind

        negated =
            case b.kind of
                CharSet set ->
                    set.negated

                _ ->
                    False

        name =
            label b.kind

        badgeX =
            b.x + b.w - 3

        badgeY =
            b.y - b.h / 2 + 3
    in
    Svg.g
        ([ classes
            [ ( "node k-" ++ css, True )
            , ( "neg", negated )
            , ( "sel", selected )
            , ( "lift", decor.lifted == Just b.spot )
            , ( "armed", decor.heldLong == Just (GripStation b.spot) )
            ]
         , A.attribute "style" ("--c:var(--c-" ++ css ++ ")")
         , onGrab toMsg (GripStation b.spot)
         ]
            ++ hotspotAttrs toMsg
                (StationAt b.spot)
                (if negated then
                    name ++ " (negated)"

                 else
                    name
                )
        )
        ((if selected then
            [ Svg.rect [ SA.class "halo", SA.x (num (b.x - 4)), SA.y (num (b.y - b.h / 2 - 4)), SA.width (num (b.w + 8)), SA.height (num (b.h + 8)), SA.rx "11" ] [] ]

          else
            []
         )
            ++ [ Svg.rect [ SA.class "box", SA.x (num b.x), SA.y (num (b.y - b.h / 2)), SA.width (num b.w), SA.height (num b.h), SA.rx "7" ] []
               , Svg.text_ [ SA.x (num (b.x + b.w / 2)), SA.y (num (b.y + 0.5)), SA.textAnchor "middle" ] [ Svg.text name ]
               ]
            ++ (if negated then
                    [ Svg.g [ SA.class "nb" ]
                        [ Svg.circle [ SA.cx (num badgeX), SA.cy (num badgeY), SA.r "8" ] []
                        , Svg.rect [ SA.x (num (badgeX - 4.5)), SA.y (num (badgeY - 1.5)), SA.width "9", SA.height "3", SA.rx "1.5" ] []
                        ]
                    ]

                else
                    []
               )
        )


viewGap : (Event -> msg) -> Decor -> { spot : Spot, x : Float, y : Float, w : Float, empty : Bool, reversed : Bool } -> Svg msg
viewGap toMsg decor g =
    let
        selected =
            decor.selection == Just (OnGap g.spot)

        cx =
            g.x + g.w / 2

        direction =
            if g.reversed then
                -1

            else
                1
    in
    Svg.g
        (classes [ ( "slot", True ), ( "empty", g.empty ), ( "sel", selected ), ( "armed", decor.heldLong == Just (GripGap g.spot) ) ]
            :: hotspotAttrs toMsg (GapAt g.spot) "Drop here"
            ++ (if g.empty then
                    []

                else
                    [ onGrab toMsg (GripGap g.spot) ]
               )
        )
        [ Svg.rect [ SA.class "hit", SA.x (num g.x), SA.y (num (g.y - 20)), SA.width (num g.w), SA.height "40" ] []
        , if g.empty then
            Svg.rect [ SA.class "ph", SA.x (num (g.x + 3)), SA.y (num (g.y - 15)), SA.width (num (g.w - 6)), SA.height "30", SA.rx "7" ] []

          else
            Svg.polygon
                [ SA.class "sarw"
                , SA.points (num (cx - 4 * direction) ++ "," ++ num (g.y - 5) ++ " " ++ num (cx + 4 * direction) ++ "," ++ num g.y ++ " " ++ num (cx - 4 * direction) ++ "," ++ num (g.y + 5))
                ]
                []
        , Svg.circle
            [ SA.cx (num cx)
            , SA.cy (num g.y)
            , SA.r
                (if selected then
                    "8"

                 else
                    "6"
                )
            ]
            []
        , Svg.path [ SA.d (plusSign cx g.y) ] []
        ]


viewSignal : (Event -> msg) -> Decor -> { spot : Spot, signal : Signal, x : Float, y : Float, bubbleY : Float, bubble : String, rail : String } -> Svg msg
viewSignal toMsg decor p =
    let
        selected =
            decor.selection == Just (OnStation p.spot)

        red =
            p.signal.light == Red

        description =
            (if red then
                "Red signal: stop if "

             else
                "Green signal: go only if "
            )
                ++ (if p.signal.watch == Behind then
                        "the text so far ends with this"

                    else
                        "the text ahead starts with this"
                   )

        lamps cy height r =
            [ Svg.circle [ classes [ ( "lr", True ), ( "on", red ) ], SA.cx (num p.x), SA.cy (num (cy - height / 4)), SA.r (num r) ] []
            , Svg.circle [ classes [ ( "lgn", True ), ( "on", not red ) ], SA.cx (num p.x), SA.cy (num (cy + height / 4)), SA.r (num r) ] []
            ]
    in
    Svg.g
        ([ classes
            [ ( "node look", True )
            , ( "sel", selected )
            , ( "lift", decor.lifted == Just p.spot )
            , ( "armed", decor.heldLong == Just (GripStation p.spot) )
            ]
         , A.attribute "style"
            (if red then
                "--c:var(--c-look)"

             else
                "--c:var(--good)"
            )
         , onGrab toMsg (GripStation p.spot)
         ]
            ++ hotspotAttrs toMsg (StationAt p.spot) description
        )
        ([ Svg.path [ SA.class "bubble", SA.d p.bubble ] []
         , Svg.path [ SA.class "brail", SA.d p.rail ] []
         , Svg.rect [ SA.class "sig", SA.x (num (p.x - 5.5)), SA.y (num (p.bubbleY - 10)), SA.width "11", SA.height "20", SA.rx "5.5" ] []
         , Svg.g [ SA.class "mini" ] (lamps p.bubbleY 20 2.8)
         ]
            ++ (if selected then
                    [ Svg.rect [ SA.class "halo", SA.x (num (p.x - 13)), SA.y (num (p.y - 19)), SA.width "26", SA.height "38", SA.rx "13" ] [] ]

                else
                    []
               )
            ++ Svg.rect [ SA.class "box", SA.x (num (p.x - 9)), SA.y (num (p.y - 15)), SA.width "18", SA.height "30", SA.rx "9" ] []
            :: lamps p.y 30 4.6
            ++ [ Svg.title [] [ Svg.text description ] ]
        )


terminal : (Event -> msg) -> Layout -> Bool -> Bool -> Svg msg
terminal toMsg l isStart anchored =
    let
        y =
            l.baseline

        ( symbol, id, hotspot ) =
            if isStart then
                ( "^", "start", StartTerminal )

            else
                ( "$", "goal", EndTerminal )

        edge =
            if isStart then
                margin

            else
                l.width - margin

        direction =
            if isStart then
                1

            else
                -1

        inner =
            edge + direction * terminalWidth

        plateX =
            edge + direction * 14

        stopX =
            edge + direction * 36

        description =
            case ( isStart, anchored ) of
                ( True, True ) ->
                    "Terminus: start of string"

                ( True, False ) ->
                    "Open start: can begin anywhere"

                ( False, True ) ->
                    "Terminus: end of string"

                ( False, False ) ->
                    "Open end: can stop anywhere"

        bufferStop =
            Svg.g [ SA.class "bstop" ]
                [ Svg.path [ SA.class "bleg", SA.d (path [ M (stopX + direction * 4) (y - 9), L (stopX + direction * 14) y, M (stopX + direction * 4) (y + 9), L (stopX + direction * 14) y ]) ] []
                , Svg.rect [ SA.x (num (stopX - 4)), SA.y (num (y - 14)), SA.width "8", SA.height "28", SA.rx "2" ] []
                , Svg.path [ SA.class "bstripe", SA.d (path [ M (stopX - 4) (y - 7), HBy 8, M (stopX - 4) y, HBy 8, M (stopX - 4) (y + 7), HBy 8 ]) ] []
                ]
    in
    Svg.g
        (classes [ ( "term", True ), ( "on", anchored ), ( "off", not anchored ) ]
            :: A.attribute "aria-pressed"
                (if anchored then
                    "true"

                 else
                    "false"
                )
            :: hotspotAttrs toMsg hotspot (symbol ++ " " ++ description)
        )
        ([ Svg.title [] [ Svg.text (symbol ++ " " ++ description) ]
         , Svg.rect [ SA.class "hit", SA.x (num (min edge inner)), SA.y (num (y - 22)), SA.width (num terminalWidth), SA.height "44" ] []
         ]
            ++ (if anchored then
                    rail (path [ M stopX y, H inner ]) ++ [ bufferStop ]

                else
                    [ Svg.g [ SA.mask ("url(#fade-" ++ id ++ ")") ] (rail (path [ M (edge + direction * 26) y, H inner ]))
                    , Svg.g [ SA.class "ghost" ] [ bufferStop ]
                    ]
               )
            ++ [ Svg.rect [ SA.class "plate", SA.x (num (plateX - 14)), SA.y (num (y - 14)), SA.width "28", SA.height "28", SA.rx "7" ] []
               , Svg.text_ [ SA.class "tlab", SA.x (num plateX), SA.y (num (y + 1)), SA.textAnchor "middle" ] [ Svg.text symbol ]
               ]
        )


fadeOut : Layout -> String -> Float -> Float -> List (Svg msg)
fadeOut l id x1 x2 =
    [ Svg.linearGradient [ SA.id ("fg-" ++ id), SA.gradientUnits "userSpaceOnUse", SA.x1 (num x1), SA.y1 "0", SA.x2 (num x2), SA.y2 "0" ]
        [ Svg.stop [ SA.offset "0", SA.stopColor "#fff", SA.stopOpacity "0" ] []
        , Svg.stop [ SA.offset "1", SA.stopColor "#fff" ] []
        ]
    , Svg.mask [ SA.id ("fade-" ++ id), SA.maskUnits "userSpaceOnUse", SA.x "0", SA.y "0", SA.width (num l.width), SA.height (num l.height) ]
        [ Svg.rect [ SA.x "0", SA.y "0", SA.width (num l.width), SA.height (num l.height), SA.fill ("url(#fg-" ++ id ++ ")") ] [] ]
    ]


marks : Targets -> Maybe Mark -> List (Svg msg)
marks t mark =
    let
        find spot =
            List.filter (\target -> target.spot == spot) >> List.head
    in
    case mark of
        Just (MarkGap spot) ->
            find spot t.gaps
                |> Maybe.map (\g -> [ Svg.rect [ SA.class "mkb", SA.x (num (g.x - 2.5)), SA.y (num (g.y - 23)), SA.width "5", SA.height "46", SA.rx "2.5" ] [] ])
                |> Maybe.withDefault []

        Just (MarkStation spot) ->
            find spot t.stations
                |> Maybe.map (\b -> [ Svg.rect [ SA.class "mkw", SA.x (num (b.x - 6)), SA.y (num (b.y - b.h / 2 - 6)), SA.width (num (b.w + 12)), SA.height (num (b.h + 12)), SA.rx "11" ] [] ])
                |> Maybe.withDefault []

        Just (MarkAdder spot) ->
            find spot t.adders
                |> Maybe.map (\a -> [ Svg.circle [ SA.class "mkl", SA.cx (num a.x), SA.cy (num a.y), SA.r "13" ] [] ])
                |> Maybe.withDefault []

        Just (MarkRail r) ->
            railMark r

        Nothing ->
            []


railMark : { fromX : Float, toX : Float, y : Float, below : Bool } -> List (Svg msg)
railMark r =
    let
        x1 =
            min r.fromX r.toX

        x2 =
            max r.fromX r.toX

        back =
            r.toX < r.fromX

        ( s, near, far ) =
            if back || r.below then
                ( 1, 1, 0 )

            else
                ( -1, 0, 1 )

        bridge =
            path [ M x1 r.y, Arc near curveRadius (s * curveRadius), V (r.y + s * (30 - curveRadius)), Arc far curveRadius (s * curveRadius), H (x2 - 2 * curveRadius), Arc far curveRadius (-s * curveRadius), V (r.y + s * curveRadius), Arc near curveRadius (-s * curveRadius) ]

        mid =
            (x1 + x2) / 2
    in
    [ Svg.rect [ SA.class "mkspan", SA.x (num x1), SA.y (num (r.y - 24)), SA.width (num (x2 - x1)), SA.height "48", SA.rx "10" ] []
    , Svg.path [ SA.class "mkr", SA.d bridge ] []
    , Svg.circle [ SA.class "mkdot", SA.cx (num r.fromX), SA.cy (num r.y), SA.r "5" ] []
    , Svg.circle [ SA.class "mkdot", SA.cx (num r.toX), SA.cy (num r.y), SA.r "5" ] []
    ]
        ++ (if back then
                [ Svg.path [ SA.class "mkarw", SA.d (path [ M (mid + 4) (r.y + 30 - 4.5), L (mid - 3) (r.y + 30), L (mid + 4) (r.y + 30 + 4.5), Close ]) ] [] ]

            else
                []
           )

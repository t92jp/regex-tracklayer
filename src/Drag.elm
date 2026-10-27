module Drag exposing
    ( Drag, Cargo(..), Drop(..), RailDrop, Point, Rects, Released(..)
    , idle, press, waitingForHold, longPressed, move, measured, release
    , holding, dragging, carried, carryingStation, overTray, decor
    , measure, autoScroll
    )

import Browser.Dom as Dom
import Diagram exposing (Grip(..), Mark(..), Pointer)
import Task exposing (Task)
import Track exposing (Selection, Spot, Tool(..))


type Drag
    = Idle
    | Pressing Press
    | Dragging Carry


type alias Press =
    { cargo : Cargo, pointerId : Int, start : Point, canDrag : Bool, heldLong : Bool, holdTimer : Int }


type alias Carry =
    { cargo : Cargo
    , pointerId : Int
    , pointer : Point
    , drop : Maybe Drop
    , targets : Diagram.Targets
    , anchor : Maybe Diagram.PointTarget
    , heldLong : Bool
    }


type Cargo
    = NewStop Tool
    | Existing Spot
    | RailFrom Spot


type Drop
    = ToTray
    | ToStation Spot
    | ToGap Spot
    | ToNewLane Spot
    | ToRail RailDrop


type alias RailDrop =
    { from : Spot, to : Spot, below : Bool, fromX : Float, toX : Float, y : Float }


type alias Point =
    { x : Float, y : Float }


type alias Rect =
    { left : Float, top : Float, right : Float, bottom : Float }


type alias Rects =
    { tray : Rect, stage : Rect, rail : Rect }


idle : Drag
idle =
    Idle


press : Int -> Cargo -> Pointer -> Drag -> Drag
press timer cargo pointer drag =
    if pointer.button /= 0 || drag /= Idle then
        drag

    else
        let
            needsHold =
                case cargo of
                    NewStop _ ->
                        False

                    _ ->
                        pointer.touch
        in
        Pressing { cargo = cargo, pointerId = pointer.id, start = { x = pointer.x, y = pointer.y }, canDrag = not needsHold, heldLong = False, holdTimer = timer }


waitingForHold : Drag -> Maybe Int
waitingForHold drag =
    case drag of
        Pressing p ->
            if p.canDrag then
                Nothing

            else
                Just p.holdTimer

        _ ->
            Nothing


longPressed : Int -> Drag -> Maybe Drag
longPressed timer drag =
    case drag of
        Pressing p ->
            if p.holdTimer == timer then
                Just (Pressing { p | canDrag = True, heldLong = True })

            else
                Nothing

        _ ->
            Nothing


move : (() -> Diagram.Targets) -> Pointer -> Drag -> ( Drag, Maybe Point )
move computeTargets pointer drag =
    let
        point =
            { x = pointer.x, y = pointer.y }
    in
    case drag of
        Pressing p ->
            if pointer.id /= p.pointerId then
                ( drag, Nothing )

            else if not p.canDrag then
                if distance p.start point > 8 then
                    ( Idle, Nothing )

                else
                    ( drag, Nothing )

            else if distance p.start point < 6 then
                ( drag, Nothing )

            else
                let
                    targets =
                        computeTargets ()
                in
                ( Dragging
                    { cargo = p.cargo
                    , pointerId = p.pointerId
                    , pointer = point
                    , drop = Nothing
                    , targets = targets
                    , anchor =
                        case p.cargo of
                            RailFrom gap ->
                                targets.gaps |> List.filter (\g -> g.spot == gap) |> List.head

                            _ ->
                                Nothing
                    , heldLong = p.heldLong
                    }
                , Just point
                )

        Dragging carry ->
            if pointer.id /= carry.pointerId then
                ( drag, Nothing )

            else
                ( Dragging { carry | pointer = point }, Just point )

        Idle ->
            ( drag, Nothing )


measured : Rects -> Point -> Drag -> Drag
measured rects point drag =
    case drag of
        Dragging carry ->
            Dragging { carry | drop = dropAt carry.targets carry rects point }

        _ ->
            drag


type Released
    = NotThisPointer
    | LetGo
    | Dropped (Maybe ( Cargo, Drop ))


release : Bool -> Pointer -> Drag -> Released
release cancelled pointer drag =
    case drag of
        Pressing p ->
            if pointer.id == p.pointerId then
                LetGo

            else
                NotThisPointer

        Dragging carry ->
            if pointer.id /= carry.pointerId then
                NotThisPointer

            else if cancelled then
                Dropped Nothing

            else
                Dropped (Maybe.map (Tuple.pair carry.cargo) carry.drop)

        Idle ->
            NotThisPointer


holding : Drag -> Bool
holding drag =
    case drag of
        Pressing p ->
            p.canDrag

        Dragging _ ->
            True

        Idle ->
            False


dragging : Drag -> Bool
dragging drag =
    case drag of
        Dragging _ ->
            True

        _ ->
            False


carried : Drag -> Maybe ( Cargo, Point )
carried drag =
    case drag of
        Dragging carry ->
            Just ( carry.cargo, carry.pointer )

        _ ->
            Nothing


carryingStation : Drag -> Bool
carryingStation drag =
    case carried drag of
        Just ( Existing _, _ ) ->
            True

        _ ->
            False


overTray : Drag -> Bool
overTray drag =
    case drag of
        Dragging carry ->
            carry.drop == Just ToTray

        _ ->
            False


decor : Maybe Selection -> Drag -> Diagram.Decor
decor selection drag =
    let
        base =
            { selection = selection, lifted = Nothing, heldLong = Nothing, dragging = False, mark = Nothing }
    in
    case drag of
        Pressing p ->
            if p.heldLong then
                { base | heldLong = grip p.cargo }

            else
                base

        Dragging carry ->
            { base
                | lifted =
                    case carry.cargo of
                        Existing spot ->
                            Just spot

                        _ ->
                            Nothing
                , heldLong =
                    if carry.heldLong then
                        grip carry.cargo

                    else
                        Nothing
                , dragging = True
                , mark = Maybe.andThen mark carry.drop
            }

        Idle ->
            base


grip : Cargo -> Maybe Grip
grip cargo =
    case cargo of
        Existing spot ->
            Just (GripStation spot)

        RailFrom gap ->
            Just (GripGap gap)

        NewStop _ ->
            Nothing


mark : Drop -> Maybe Mark
mark drop =
    case drop of
        ToTray ->
            Nothing

        ToStation spot ->
            Just (MarkStation spot)

        ToGap gap ->
            Just (MarkGap gap)

        ToNewLane branch ->
            Just (MarkAdder branch)

        ToRail r ->
            Just (MarkRail { fromX = r.fromX, toX = r.toX, y = r.y, below = r.below })


dropAt : Diagram.Targets -> Carry -> Rects -> Point -> Maybe Drop
dropAt targets carry rects point =
    let
        inDiagram =
            { x = point.x - rects.rail.left, y = point.y - rects.rail.top }

        nearest measureDistance limit candidates =
            List.foldl
                (\c ( best, bestDistance ) ->
                    let
                        d =
                            measureDistance c
                    in
                    if d < bestDistance then
                        ( Just c, d )

                    else
                        ( best, bestDistance )
                )
                ( Nothing, limit )
                candidates
                |> Tuple.first

        onStation () =
            case carry.cargo of
                NewStop tool ->
                    if tool /= SignalTool then
                        targets.stations
                            |> List.filter (\s -> inDiagram.x >= s.x - 6 && inDiagram.x <= s.x + s.w + 6 && abs (inDiagram.y - s.y) <= s.h / 2 + 6)
                            |> List.head
                            |> Maybe.map (ToStation << .spot)

                    else
                        Nothing

                _ ->
                    Nothing

        onAdder () =
            nearest (\a -> distance { x = a.x, y = a.y } inDiagram) 22 targets.adders
                |> Maybe.map (ToNewLane << .spot)

        inGap () =
            nearest (\g -> distanceFavoringRow { x = g.x, y = g.y } inDiagram) 90 targets.gaps
                |> Maybe.map (ToGap << .spot)
    in
    case carry.cargo of
        RailFrom _ ->
            railDrop targets rects carry.anchor inDiagram point

        Existing _ ->
            if inside rects.tray point then
                Just ToTray

            else if inside rects.stage point then
                firstJust [ onStation, onAdder, inGap ]

            else
                Nothing

        NewStop _ ->
            if inside rects.stage point && not (inside rects.tray point) then
                firstJust [ onStation, onAdder, inGap ]

            else
                Nothing


railDrop : Diagram.Targets -> Rects -> Maybe Diagram.PointTarget -> Point -> Point -> Maybe Drop
railDrop targets rects anchor inDiagram onScreen =
    case anchor of
        Just a ->
            if not (inside rects.stage onScreen) || abs (inDiagram.y - a.y) > 150 || abs (inDiagram.x - a.x) < minimumRailLength then
                Nothing

            else
                targets.gaps
                    |> List.filter (\g -> g.spot.line == a.spot.line && g.spot /= a.spot)
                    |> List.foldl
                        (\g best ->
                            case best of
                                Just b ->
                                    if abs (inDiagram.x - g.x) < abs (inDiagram.x - b.x) then
                                        Just g

                                    else
                                        best

                                Nothing ->
                                    Just g
                        )
                        Nothing
                    |> Maybe.map (\b -> ToRail { from = a.spot, to = b.spot, below = inDiagram.y > a.y + 14, fromX = a.x, toX = b.x, y = a.y })

        Nothing ->
            Nothing


firstJust : List (() -> Maybe a) -> Maybe a
firstJust tries =
    case tries of
        try :: rest ->
            case try () of
                Just found ->
                    Just found

                Nothing ->
                    firstJust rest

        [] ->
            Nothing


inside : Rect -> Point -> Bool
inside r p =
    p.x >= r.left && p.x <= r.right && p.y >= r.top && p.y <= r.bottom


minimumRailLength : Float
minimumRailLength =
    24


distanceFavoringRow : Point -> Point -> Float
distanceFavoringRow a b =
    distance { x = a.x, y = a.y * 1.25 } { x = b.x, y = b.y * 1.25 }


distance : Point -> Point -> Float
distance a b =
    sqrt ((a.x - b.x) ^ 2 + (a.y - b.y) ^ 2)


measure : Task Dom.Error (Maybe Rects)
measure =
    Task.map3 Rects (rectOf "tray") (rectOf "stage") (rectOf "rail") |> Task.map Just


rectOf : String -> Task Dom.Error Rect
rectOf id =
    Dom.getElement id
        |> Task.map
            (\{ element, viewport } ->
                let
                    left =
                        element.x - viewport.x

                    top =
                        element.y - viewport.y
                in
                { left = left, top = top, right = left + element.width, bottom = top + element.height }
            )


autoScroll : Point -> Task Dom.Error (Maybe Rects)
autoScroll point =
    rectOf "stage"
        |> Task.andThen
            (\stage ->
                let
                    edge =
                        44

                    dx =
                        if point.x < stage.left + edge && point.x > stage.left - 60 then
                            -10

                        else if point.x > stage.right - edge && point.x < stage.right + 60 then
                            10

                        else
                            0

                    dy =
                        if point.y < stage.top + edge && point.y > stage.top - 70 then
                            -10

                        else if point.y > stage.bottom - edge then
                            10

                        else
                            0
                in
                if point.y < stage.top - 70 || point.y > stage.bottom + 10 then
                    Task.succeed Nothing

                else
                    Dom.getViewportOf "stage"
                        |> Task.andThen (\{ viewport } -> Dom.setViewportOf "stage" (viewport.x + dx) (viewport.y + dy))
                        |> Task.andThen (\_ -> measure)
            )

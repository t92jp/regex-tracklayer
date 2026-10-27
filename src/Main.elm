port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Diagram exposing (Grip(..), Hotspot(..), Pointer)
import Drag exposing (Drag)
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Html.Keyed as Keyed
import Icon
import Json.Decode as D
import Panel
import Pattern exposing (Role(..))
import Process
import Save
import Task
import Tidy
import Time
import Track exposing (..)


port save : String -> Cmd msg


port checkPattern : { checkId : Int, pattern : String, text : String } -> Cmd msg


port copyText : String -> Cmd msg


port selectText : String -> Cmd msg


port vibrate : () -> Cmd msg


port restoreFocus : String -> Cmd msg


port patternChecked : ({ checkId : Int, error : String } -> msg) -> Sub msg


port matchesFound : ({ checkId : Int, timedOut : Bool, ranges : List (List Int) } -> msg) -> Sub msg


type alias Model =
    { track : Track
    , history : List Snapshot
    , selection : Maybe Selection
    , choosingCustomRepeat : Bool
    , typingInPanel : Bool
    , testText : String
    , settings : Diagram.Settings
    , patternError : Maybe String
    , test : Test
    , latestCheckId : Int
    , tidyable : Bool
    , drag : Drag
    , copiedMarkTimer : Maybe Int
    , ignoreClicksTimer : Maybe Int
    , focusedDiagramId : Maybe String
    , lastTimerId : Int
    }


type alias Snapshot =
    { track : Track, selection : Maybe Selection }


type Test
    = NotShown
    | Matched String (List ( Int, Int ))
    | Failed String
    | TimedOut


type alias Flags =
    { saved : Maybe String, coarsePointer : Bool }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        { track, testText } =
            flags.saved
                |> Maybe.andThen Save.decode
                |> Maybe.withDefault { track = Track.pluralNounsExample, testText = pluralNounsTestText }
    in
    refresh
        { track = track
        , history = []
        , selection =
            if List.isEmpty track.line then
                Just (OnGap { line = [], index = 0 })

            else
                Nothing
        , choosingCustomRepeat = False
        , typingInPanel = False
        , testText = testText
        , settings =
            { gapWidth =
                if flags.coarsePointer then
                    gapWidthForTouch

                else
                    gapWidthForMouse
            }
        , patternError = Nothing
        , test = NotShown
        , latestCheckId = 0
        , tidyable = False
        , drag = Drag.idle
        , copiedMarkTimer = Nothing
        , ignoreClicksTimer = Nothing
        , focusedDiagramId = Nothing
        , lastTimerId = 0
        }


pluralNounsTestText : String
pluralNounsTestText =
    "cats, boxes, churches, potatoes, babies, boys, leaves, knives, a dog, the class"


gapWidthForMouse : Float
gapWidthForMouse =
    22


gapWidthForTouch : Float
gapWidthForTouch =
    30


historyLimit : Int
historyLimit =
    60


longPressDelay : Float
longPressDelay =
    280


copiedMarkDuration : Float
copiedMarkDuration =
    1200


ignoreClicksAfterDrop : Float
ignoreClicksAfterDrop =
    300


autoScrollInterval : Float
autoScrollInterval =
    30


revealMargin : Float
revealMargin =
    24


firstCustomRepeat : Repeat
firstCustomRepeat =
    { min = 2, max = Just 3 }


persist : Model -> ( Model, Cmd Msg )
persist model =
    ( model, save (Save.encode { track = model.track, testText = model.testText }) )


type Msg
    = DiagramEvent Diagram.Event
    | BackgroundClicked
    | ToolClicked Tool
    | ToolGrabbed Tool Pointer
    | PanelAction Panel.Action
    | TidyClicked
    | UndoClicked
    | ClearClicked
    | CopyClicked
    | TestTextChanged String
    | PatternChecked { checkId : Int, error : String }
    | MatchesFound { checkId : Int, timedOut : Bool, ranges : List (List Int) }
    | PointerMoved Pointer
    | PointerReleased Bool Pointer
    | Measured Drag.Point (Result Dom.Error (Maybe Drag.Rects))
    | AutoScrollTick
    | TimerFired Timer Int
    | NoOp


type Timer
    = LongPress
    | CopiedMark
    | ClickGuard


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DiagramEvent event ->
            onDiagram event model

        BackgroundClicked ->
            if model.ignoreClicksTimer /= Nothing || model.selection == Nothing then
                ( model, Cmd.none )

            else
                select Nothing model

        ToolClicked tool ->
            if model.ignoreClicksTimer /= Nothing then
                ( model, Cmd.none )

            else
                placeNew tool (nextGap model) model

        ToolGrabbed tool pointer ->
            grab (Drag.NewStop tool) pointer model

        PanelAction action ->
            onPanel action model

        TidyClicked ->
            if model.tidyable then
                commit (\_ -> Tidy.tidy model.track.line) { model | selection = Nothing }

            else
                ( model, Cmd.none )

        UndoClicked ->
            case model.history of
                previous :: older ->
                    { model | track = previous.track, selection = previous.selection, history = older, choosingCustomRepeat = False }
                        |> persist
                        |> andThen refresh

                [] ->
                    ( model, Cmd.none )

        ClearClicked ->
            changeTrack (\_ -> Track.empty) model
                |> andThen (select (Just (OnGap { line = [], index = 0 })))

        CopyClicked ->
            case Pattern.toString model.track of
                "" ->
                    ( model, Cmd.none )

                pattern ->
                    let
                        ( timer, started ) =
                            startTimer CopiedMark copiedMarkDuration model
                    in
                    ( { model | copiedMarkTimer = Just timer, lastTimerId = timer }, Cmd.batch [ copyText pattern, started ] )

        TestTextChanged text ->
            { model | testText = text } |> persist |> andThen checkTest

        PatternChecked result ->
            if result.checkId /= model.latestCheckId then
                ( model, Cmd.none )

            else if result.error == "" then
                ( { model | patternError = Nothing }, Cmd.none )

            else
                ( { model
                    | patternError = Just result.error
                    , test =
                        if testVisible model then
                            Failed (withoutPrefix "Invalid regular expression: " result.error)

                        else
                            model.test
                  }
                , Cmd.none
                )

        MatchesFound result ->
            if result.checkId /= model.latestCheckId then
                ( model, Cmd.none )

            else if result.timedOut then
                ( { model | test = TimedOut }, Cmd.none )

            else
                ( { model | test = Matched model.testText (List.filterMap pair result.ranges) }, Cmd.none )

        PointerMoved pointer ->
            let
                ( drag, toMeasure ) =
                    Drag.move (\() -> Diagram.targets model.settings model.track) pointer model.drag
            in
            ( { model | drag = drag }
            , Maybe.map (\point -> Task.attempt (Measured point) Drag.measure) toMeasure |> Maybe.withDefault Cmd.none
            )

        PointerReleased cancelled pointer ->
            release cancelled pointer model

        Measured point (Ok (Just rects)) ->
            ( { model | drag = Drag.measured rects point model.drag }, Cmd.none )

        Measured _ _ ->
            ( model, Cmd.none )

        AutoScrollTick ->
            case Drag.carried model.drag of
                Just ( _, point ) ->
                    ( model, Task.attempt (Measured point) (Drag.autoScroll point) )

                Nothing ->
                    ( model, Cmd.none )

        TimerFired LongPress timer ->
            case Drag.longPressed timer model.drag of
                Just armed ->
                    ( { model | drag = armed }, vibrate () )

                Nothing ->
                    ( model, Cmd.none )

        TimerFired CopiedMark timer ->
            if model.copiedMarkTimer == Just timer then
                ( { model | copiedMarkTimer = Nothing }, Cmd.none )

            else
                ( model, Cmd.none )

        TimerFired ClickGuard timer ->
            if model.ignoreClicksTimer == Just timer then
                ( { model | ignoreClicksTimer = Nothing }, Cmd.none )

            else
                ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


andThen : (Model -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
andThen f ( model, cmd ) =
    let
        ( next, more ) =
            f model
    in
    ( next, Cmd.batch [ cmd, more ] )


startTimer : Timer -> Float -> Model -> ( Int, Cmd Msg )
startTimer timer ms model =
    let
        number =
            model.lastTimerId + 1
    in
    ( number, Task.perform (\_ -> TimerFired timer number) (Process.sleep ms) )


changeTrack : (Track -> Track) -> Model -> ( Model, Cmd Msg )
changeTrack f model =
    { model | history = remember model, track = f model.track }
        |> persist
        |> andThen refresh


remember : Model -> List Snapshot
remember model =
    List.take historyLimit ({ track = model.track, selection = model.selection } :: model.history)


commit : (Line -> Line) -> Model -> ( Model, Cmd Msg )
commit f =
    changeTrack (\track -> { track | line = f track.line })


refresh : Model -> ( Model, Cmd Msg )
refresh model =
    let
        track =
            model.track

        line =
            pruneLanes track.line
    in
    { model | track = { track | line = line }, tidyable = Tidy.canTidy line }
        |> dropMissingSelection
        |> afterRedraw
        |> andThen checkTest


checkTest : Model -> ( Model, Cmd Msg )
checkTest model =
    let
        checkId =
            model.latestCheckId + 1
    in
    ( { model
        | latestCheckId = checkId
        , test =
            if testVisible model then
                model.test

            else
                NotShown
      }
    , checkPattern { checkId = checkId, pattern = Pattern.toString model.track, text = model.testText }
    )


testVisible : Model -> Bool
testVisible model =
    model.testText /= "" && Pattern.toString model.track /= ""


placeNew : Tool -> Spot -> Model -> ( Model, Cmd Msg )
placeNew tool gap model =
    let
        stop =
            newStop tool
    in
    commit (insert gap stop) model |> andThen (selectAdded stop gap)


nextGap : Model -> Spot
nextGap model =
    case model.selection of
        Just (OnStation spot) ->
            { spot | index = spot.index + 1 }

        Just (OnGap gap) ->
            gap

        Nothing ->
            { line = [], index = List.length model.track.line }


addEmptyLane : Spot -> Model -> ( Model, Cmd Msg )
addEmptyLane branch model =
    commit (addLane [] branch) model
        |> andThen (\m -> select (Just (OnGap { line = laneIn branch (BranchLane (laneCount branch m.track.line - 1)), index = 0 })) m)


select : Maybe Selection -> Model -> ( Model, Cmd Msg )
select selection model =
    { model | selection = selection, choosingCustomRepeat = False }
        |> dropMissingSelection
        |> afterRedraw


selectAdded : Stop -> Spot -> Model -> ( Model, Cmd Msg )
selectAdded stop spot model =
    let
        hasValue =
            case stop of
                StationStop { kind } ->
                    case kind of
                        Text _ ->
                            True

                        CharSet _ ->
                            True

                        _ ->
                            False

                SignalStop _ ->
                    False
    in
    select (Just (OnStation spot)) model
        |> andThen
            (\m ->
                if hasValue then
                    ( m, selectText Panel.valueInputId )

                else
                    ( m, Cmd.none )
            )


dropMissingSelection : Model -> Model
dropMissingSelection model =
    let
        exists =
            case model.selection of
                Just (OnStation spot) ->
                    stopAt spot model.track.line /= Nothing

                Just (OnGap gap) ->
                    lineAt gap.line model.track.line
                        |> Maybe.map (\line -> gap.index <= List.length line)
                        |> Maybe.withDefault False

                Nothing ->
                    True
    in
    if exists then
        model

    else
        { model | selection = Nothing }


afterRedraw : Model -> ( Model, Cmd Msg )
afterRedraw model =
    ( model
    , Cmd.batch
        [ Maybe.map restoreFocus model.focusedDiagramId |> Maybe.withDefault Cmd.none
        , Maybe.map reveal (revealTarget model) |> Maybe.withDefault Cmd.none
        ]
    )


revealTarget : Model -> Maybe String
revealTarget model =
    case model.selection of
        Just (OnGap gap) ->
            Just (Diagram.hotspotId (GapAt gap))

        Just (OnStation spot) ->
            case stopAt spot model.track.line of
                Just (StationStop { kind }) ->
                    case kind of
                        Branch _ ->
                            Nothing

                        _ ->
                            Just (Diagram.hotspotId (StationAt spot))

                Just (SignalStop _) ->
                    Just (Diagram.hotspotId (StationAt spot))

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


reveal : String -> Cmd Msg
reveal id =
    Task.map3
        (\target stage viewport ->
            let
                scrollToShow scroll pos size boxPos boxSize =
                    if pos < boxPos + revealMargin then
                        scroll - (boxPos + revealMargin - pos)

                    else if pos + size > boxPos + boxSize - revealMargin then
                        scroll + (pos + size - (boxPos + boxSize - revealMargin))

                    else
                        scroll

                a =
                    target.element

                b =
                    stage.element
            in
            if a.width == 0 then
                Task.succeed ()

            else
                Dom.setViewportOf "stage"
                    (scrollToShow viewport.viewport.x a.x a.width b.x b.width)
                    (scrollToShow viewport.viewport.y a.y a.height b.y b.height)
        )
        (Dom.getElement id)
        (Dom.getElement "stage")
        (Dom.getViewportOf "stage")
        |> Task.andThen identity
        |> Task.attempt (\_ -> NoOp)


onDiagram : Diagram.Event -> Model -> ( Model, Cmd Msg )
onDiagram event model =
    case event of
        Diagram.Clicked hotspot ->
            if model.ignoreClicksTimer /= Nothing then
                ( model, Cmd.none )

            else
                activate hotspot model

        Diagram.KeyPressed hotspot ->
            activate hotspot model

        Diagram.Grabbed (GripStation spot) pointer ->
            grab (Drag.Existing spot) pointer model

        Diagram.Grabbed (GripGap gap) pointer ->
            grab (Drag.RailFrom gap) pointer model

        Diagram.FocusIn id ->
            ( { model | focusedDiagramId = Just id }, Cmd.none )

        Diagram.FocusOut id ->
            if model.focusedDiagramId == Just id then
                ( { model | focusedDiagramId = Nothing }, Cmd.none )

            else
                ( model, Cmd.none )

        Diagram.ContextMenuBlocked ->
            ( model, Cmd.none )


activate : Hotspot -> Model -> ( Model, Cmd Msg )
activate hotspot model =
    case hotspot of
        StartTerminal ->
            changeTrack (\t -> { t | anchoredStart = not t.anchoredStart }) model

        EndTerminal ->
            changeTrack (\t -> { t | anchoredEnd = not t.anchoredEnd }) model

        StationAt spot ->
            select (Just (OnStation spot)) model

        GapAt gap ->
            select (Just (OnGap gap)) model

        AdderAt branch ->
            addEmptyLane branch model


onPanel : Panel.Action -> Model -> ( Model, Cmd Msg )
onPanel action model =
    case ( action, model.selection ) of
        ( Panel.FocusLeft, _ ) ->
            ( { model | typingInPanel = False }, Cmd.none )

        ( Panel.EnterPressed, _ ) ->
            ( model, Task.attempt (\_ -> NoOp) (Dom.blur Panel.valueInputId) )

        ( Panel.RemoveLane, Just (OnGap gap) ) ->
            case List.reverse gap.line of
                ( i, BranchLane j ) :: outer ->
                    commit (removeLane { line = List.reverse outer, index = i } j) model
                        |> andThen (select Nothing)

                _ ->
                    ( model, Cmd.none )

        ( _, Just (OnStation spot) ) ->
            editStation action spot model

        _ ->
            ( model, Cmd.none )


editStation : Panel.Action -> Spot -> Model -> ( Model, Cmd Msg )
editStation action spot model =
    let
        setRepeat repeat =
            commit (updateStation spot (\s -> { s | repeat = repeat }))

        setSignal f =
            commit
                (updateStop spot
                    (\stop ->
                        case stop of
                            SignalStop g ->
                                SignalStop (f g)

                            _ ->
                                stop
                    )
                )
                model
    in
    case action of
        Panel.ChoosePreset Custom ->
            let
                customizing =
                    { model | choosingCustomRepeat = True }
            in
            case stopAt spot model.track.line of
                Just (StationStop s) ->
                    if preset s.repeat == Once then
                        setRepeat firstCustomRepeat customizing

                    else
                        ( customizing, Cmd.none )

                _ ->
                    ( customizing, Cmd.none )

        Panel.ChoosePreset p ->
            setRepeat (presetRepeat p |> Maybe.withDefault once) { model | choosingCustomRepeat = False }

        Panel.ToggleLazy ->
            commit (updateStation spot (\s -> { s | lazy = not s.lazy })) model

        Panel.ToggleNegated ->
            commit
                (updateStation spot
                    (\s ->
                        case s.kind of
                            CharSet set ->
                                { s | kind = CharSet { set | negated = not set.negated } }

                            _ ->
                                s
                    )
                )
                model

        Panel.SetLight light ->
            setSignal (\g -> { g | light = light })

        Panel.SetWatch watch ->
            setSignal (\g -> { g | watch = watch })

        Panel.Delete ->
            commit (remove spot) model |> andThen (select (Just (OnGap spot)))

        Panel.MakeBranch ->
            commit (wrapInBranch spot) model
                |> andThen (select (Just (OnGap { line = laneIn spot (BranchLane 1), index = 0 })))

        Panel.AddLane ->
            addEmptyLane spot model

        Panel.EditText text ->
            typeInto spot
                (\s ->
                    case s.kind of
                        Text _ ->
                            { s | kind = Text text }

                        CharSet set ->
                            { s | kind = CharSet { set | chars = text } }

                        _ ->
                            s
                )
                model

        Panel.EditMin lo ->
            typeInto spot (\s -> { s | repeat = { min = lo, max = s.repeat.max } }) model

        Panel.EditMax hi ->
            typeInto spot (\s -> { s | repeat = { min = s.repeat.min, max = hi } }) model

        _ ->
            ( model, Cmd.none )


typeInto : Spot -> (Station -> Station) -> Model -> ( Model, Cmd Msg )
typeInto spot f model =
    let
        started =
            if model.typingInPanel then
                model

            else
                { model | typingInPanel = True, history = remember model }

        track =
            started.track
    in
    { started | track = { track | line = updateStation spot f track.line } }
        |> persist
        |> andThen refresh


grab : Drag.Cargo -> Pointer -> Model -> ( Model, Cmd Msg )
grab cargo pointer model =
    let
        ( timer, holdTimer ) =
            startTimer LongPress longPressDelay model

        drag =
            Drag.press timer cargo pointer model.drag
    in
    if drag == model.drag then
        ( model, Cmd.none )

    else
        ( { model | drag = drag, lastTimerId = timer }
        , if Drag.waitingForHold drag == Nothing then
            Cmd.none

          else
            holdTimer
        )


release : Bool -> Pointer -> Model -> ( Model, Cmd Msg )
release cancelled pointer model =
    case Drag.release cancelled pointer model.drag of
        Drag.NotThisPointer ->
            ( model, Cmd.none )

        Drag.LetGo ->
            ( { model | drag = Drag.idle }, Cmd.none )

        Drag.Dropped dropped ->
            let
                ( timer, guardEnds ) =
                    startTimer ClickGuard ignoreClicksAfterDrop model

                finished =
                    ( { model | drag = Drag.idle, ignoreClicksTimer = Just timer, lastTimerId = timer }, guardEnds )
            in
            case dropped of
                Just ( cargo, drop ) ->
                    finished |> andThen (applyDrop cargo drop)

                Nothing ->
                    finished


applyDrop : Drag.Cargo -> Drag.Drop -> Model -> ( Model, Cmd Msg )
applyDrop cargo drop model =
    case ( cargo, drop ) of
        ( Drag.NewStop tool, Drag.ToStation spot ) ->
            let
                stop =
                    newStop tool
            in
            commit (branchWith stop spot) model
                |> andThen (selectAdded stop { line = laneIn spot (BranchLane 1), index = 0 })

        ( Drag.NewStop tool, Drag.ToGap gap ) ->
            placeNew tool gap model

        ( Drag.NewStop tool, Drag.ToNewLane branch ) ->
            let
                stop =
                    newStop tool
            in
            commit (addLane [ stop ] branch) model
                |> andThen (\m -> selectAdded stop { line = laneIn branch (BranchLane (laneCount branch m.track.line - 1)), index = 0 } m)

        ( Drag.Existing spot, Drag.ToTray ) ->
            commit (remove spot) { model | selection = Nothing }

        ( Drag.Existing spot, Drag.ToGap gap ) ->
            moveStop spot (IntoGap gap) model

        ( Drag.Existing spot, Drag.ToNewLane branch ) ->
            moveStop spot (IntoNewLane branch) model

        ( Drag.RailFrom _, Drag.ToRail r ) ->
            case drawRail (railShape r.from.index r.to.index r.below) r.from.line r.from.index r.to.index model.track.line of
                Just ( line, selection ) ->
                    commit (\_ -> line) model |> andThen (select (Just selection))

                Nothing ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


moveStop : Spot -> Destination -> Model -> ( Model, Cmd Msg )
moveStop from destination model =
    case move from destination model.track.line of
        Just ( line, at ) ->
            commit (\_ -> line) model |> andThen (select (Just (OnStation at)))

        Nothing ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ patternChecked PatternChecked
        , matchesFound MatchesFound
        , if Drag.dragging model.drag then
            Time.every autoScrollInterval (\_ -> AutoScrollTick)

          else
            Sub.none
        ]


view : Model -> Html Msg
view model =
    H.div
        ([ A.id "app"
         , E.preventDefaultOn "touchmove" (D.succeed ( NoOp, Drag.holding model.drag ))
         , E.on "pointerup" (D.map (PointerReleased False) Diagram.pointerDecoder)
         , E.on "pointercancel" (D.map (PointerReleased True) Diagram.pointerDecoder)
         ]
            ++ (if model.drag /= Drag.idle then
                    [ E.on "pointermove" (D.map PointerMoved Diagram.pointerDecoder) ]

                else
                    []
               )
        )
        [ header model
        , H.main_ [ A.id "stage", E.onClick BackgroundClicked ]
            [ Diagram.view DiagramEvent model.settings (Drag.decor model.selection model.drag) model.track ]
        , dock model
        , ghost model
        ]


header : Model -> Html Msg
header model =
    H.header [ A.class "top" ]
        [ H.h1 [] [ H.span [] [ H.text "Regex Tracklayer" ] ]
        , H.div [ A.class "tools" ]
            [ iconButton "tidy" "Tidy up track" Icon.Tidy (not model.tidyable) TidyClicked
            , iconButton "undo" "Undo" Icon.Undo (List.isEmpty model.history) UndoClicked
            , iconButton "clear" "Clear all" Icon.Trash False ClearClicked
            ]
        ]


iconButton : String -> String -> Icon.Icon -> Bool -> Msg -> Html Msg
iconButton id title icon disabled msg =
    H.button [ A.class "b ic", A.id id, A.attribute "aria-label" title, A.title title, A.disabled disabled, E.onClick msg ] [ Icon.view icon ]


dock : Model -> Html Msg
dock model =
    Keyed.node "section"
        [ A.id "dock" ]
        [ ( "tray", tray model )
        , ( "panel-" ++ selectionKey model.selection
          , Panel.view { track = model.track, selection = model.selection, choosingCustomRepeat = model.choosingCustomRepeat } |> H.map PanelAction
          )
        , ( "out", output model )
        , ( "test", tester model )
        ]


selectionKey : Maybe Selection -> String
selectionKey selection =
    case selection of
        Just (OnStation spot) ->
            Diagram.hotspotId (StationAt spot)

        Just (OnGap gap) ->
            Diagram.hotspotId (GapAt gap)

        Nothing ->
            "none"


tools : List ( Tool, String )
tools =
    [ ( TextTool, "Text" )
    , ( ClassTool Digit, "Digit" )
    , ( ClassTool WordChar, "Word character" )
    , ( ClassTool Whitespace, "Whitespace" )
    , ( ClassTool AnyChar, "Any character" )
    , ( SetTool, "Character set" )
    , ( SignalTool, "Signal (lookahead / lookbehind)" )
    ]


toolFace : Tool -> List (Html msg)
toolFace tool =
    case tool of
        TextTool ->
            [ H.text "abc" ]

        ClassTool c ->
            [ H.text (classCode c) ]

        SetTool ->
            [ H.text "[ ]" ]

        SignalTool ->
            [ Icon.view Icon.Signal ]


tray : Model -> Html Msg
tray model =
    H.div
        [ A.id "tray"
        , A.classList [ ( "mv", Drag.carryingStation model.drag ), ( "del", Drag.overTray model.drag ) ]
        , A.attribute "role" "toolbar"
        , A.attribute "aria-label" "Stations"
        ]
        (List.map
            (\( tool, title ) ->
                H.button
                    [ A.class ("tk " ++ Diagram.stopClass (newStop tool))
                    , A.title title
                    , E.onClick (ToolClicked tool)
                    , E.on "pointerdown" (D.map (ToolGrabbed tool) Diagram.pointerDecoder)
                    ]
                    (toolFace tool)
            )
            tools
            ++ [ H.span [ A.class "tz", A.attribute "aria-hidden" "true" ] [ Icon.view Icon.Trash ] ]
        )


output : Model -> Html Msg
output model =
    let
        tokens =
            Pattern.tokens model.track
    in
    H.div [ A.class "out" ]
        [ H.code
            [ A.id "out"
            , A.classList [ ( "bad", model.patternError /= Nothing ) ]
            , A.title (Maybe.withDefault "" model.patternError)
            , A.attribute "aria-live" "polite"
            ]
            (if List.isEmpty tokens then
                [ H.span [ A.class "mute" ] [ H.text "—" ] ]

             else
                List.map (\token -> H.span [ A.class (roleClass token.role) ] [ H.text token.text ]) tokens
            )
        , iconButton "copy"
            "Copy"
            (if model.copiedMarkTimer == Nothing then
                Icon.Copy

             else
                Icon.Check
            )
            (List.isEmpty tokens)
            CopyClicked
        ]


roleClass : Role -> String
roleClass role =
    case role of
        TextRole ->
            "k-lit"

        ClassRole ->
            "k-cls"

        SetRole ->
            "k-set"

        GroupRole ->
            "k-alt"

        SignalRole ->
            "k-look"

        AnchorRole ->
            "k-term"


tester : Model -> Html Msg
tester model =
    H.div [ A.class "test" ]
        [ H.input
            [ A.id "t"
            , A.type_ "text"
            , A.maxlength 120
            , A.placeholder "Try a string"
            , A.attribute "autocomplete" "off"
            , A.attribute "autocapitalize" "off"
            , A.attribute "autocorrect" "off"
            , A.spellcheck False
            , A.attribute "aria-label" "Test string"
            , A.class (testClass model.test)
            , A.value model.testText
            , E.onInput TestTextChanged
            ]
            []
        , H.div [ A.id "res", A.hidden (model.test == NotShown) ] (testResult model.test)
        ]


testClass : Test -> String
testClass test =
    case test of
        NotShown ->
            ""

        Matched _ [] ->
            "ng"

        Matched _ _ ->
            "ok"

        _ ->
            "ng"


testResult : Test -> List (Html msg)
testResult test =
    case test of
        NotShown ->
            []

        Matched text ranges ->
            let
                ( pieces, rest ) =
                    List.foldl
                        (\( start, length ) ( acc, from ) ->
                            ( acc ++ [ H.text (String.slice from start text), H.mark [] [ H.text (String.slice start (start + length) text) ] ]
                            , start + length
                            )
                        )
                        ( [], 0 )
                        ranges
            in
            pieces ++ [ H.text (String.dropLeft rest text) ]

        Failed message ->
            [ H.text message ]

        TimedOut ->
            [ H.text "Timed out" ]


ghost : Model -> Html Msg
ghost model =
    let
        at point cls face =
            H.div
                [ A.class ("tk dg " ++ cls)
                , A.style "left" (String.fromFloat point.x ++ "px")
                , A.style "top" (String.fromFloat point.y ++ "px")
                ]
                face
    in
    case Drag.carried model.drag of
        Just ( Drag.NewStop tool, point ) ->
            at point (Diagram.stopClass (newStop tool)) (toolFace tool)

        Just ( Drag.Existing spot, point ) ->
            case stopAt spot model.track.line of
                Just ((StationStop { kind }) as stop) ->
                    at point
                        (Diagram.stopClass stop)
                        (case kind of
                            Branch _ ->
                                [ Icon.view Icon.Fork ]

                            _ ->
                                [ H.text (Diagram.label kind) ]
                        )

                Just ((SignalStop _) as stop) ->
                    at point (Diagram.stopClass stop) [ Icon.view Icon.Signal ]

                Nothing ->
                    H.text ""

        _ ->
            H.text ""


pair : List Int -> Maybe ( Int, Int )
pair list =
    case list of
        [ a, b ] ->
            Just ( a, b )

        _ ->
            Nothing


withoutPrefix : String -> String -> String
withoutPrefix prefix text =
    if String.startsWith prefix text then
        String.dropLeft (String.length prefix) text

    else
        text


main : Program Flags Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

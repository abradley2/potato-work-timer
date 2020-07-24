module Pages.Top exposing (Model, Msg, Params, page)

import Array exposing (Array)
import Element as El
import Element.Background as Bg
import Element.Border as Bd
import Element.Events as Ev
import Element.Font as Font
import Element.Input as In
import Html exposing (i)
import Html.Attributes as A
import Process
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url as Url exposing (Url)
import String exposing (padLeft)
import Task
import Time exposing (Posix, millisToPosix, now, posixToMillis)


primaryColor : El.Color
primaryColor =
    El.rgb255 0 209 178


primaryLightColor : El.Color
primaryLightColor =
    El.rgb255 235 255 252


edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


page : Page Params Model Msg
page =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


progressBar : Mode -> Posix -> El.Element Msg
progressBar mode currentTime =
    let
        width =
            300

        progressWidth =
            width * modeElapsed mode (modeDuration mode) currentTime

        progressWidthMin =
            if progressWidth < 16 then
                16

            else
                progressWidth
    in
    El.column
        []
        [ El.el
            [ Bg.color primaryLightColor
            , El.width (El.px width)
            , El.height (El.px 16)
            , Bd.rounded 20
            , El.inFront
                (El.el
                    [ Bg.color primaryColor
                    , El.width (El.px (round progressWidthMin))
                    , El.height (El.px 16)
                    , Bd.rounded 20
                    ]
                    (El.text "")
                )
            ]
            (El.text "")
        ]



-- INIT


type alias Params =
    ()


type alias Model =
    { time : Posix
    , mode : Mode
    , modeStart : Maybe Posix
    , taskChecks : Array Bool
    }


prologueTimeView : Mode -> El.Element Msg
prologueTimeView mode =
    timeView mode (millisToPosix (modeDuration mode)) (millisToPosix (modeDuration mode))


displayTimeLeft : Posix -> Mode -> El.Element Msg
displayTimeLeft startPosix mode =
    case mode of
        Pomodoro (OnGoing val) ->
            timeView mode startPosix val

        Pomodoro Prologue ->
            prologueTimeView mode

        Pomodoro Epilogue ->
            El.el
                [ Font.color primaryColor
                ]
                (El.text "Enter checks for productivity")

        ShortBreak (OnGoing val) ->
            timeView mode startPosix val

        ShortBreak Prologue ->
            prologueTimeView mode

        LongBreak (OnGoing val) ->
            timeView mode startPosix val

        LongBreak Prologue ->
            prologueTimeView mode

        _ ->
            El.text ""


timeView : Mode -> Posix -> Posix -> El.Element Msg
timeView mode currentPosix startPosix =
    modeDuration mode
        - (posixToMillis currentPosix - posixToMillis startPosix)
        |> (\v -> v // 1000)
        |> (\v -> ( v // 60, remainderBy 60 v ))
        |> Tuple.mapBoth
            (String.fromInt >> padLeft 2 '0')
            (String.fromInt >> padLeft 2 '0')
        |> (\( minutes, seconds ) ->
                El.el
                    [ Font.color primaryColor
                    , Font.semiBold
                    , Font.size 40
                    ]
                    (El.text (minutes ++ ":" ++ seconds))
           )


init : Url Params -> ( Model, Cmd Msg )
init { params } =
    ( { time = Time.millisToPosix 0
      , mode = Pomodoro Epilogue
      , modeStart = Nothing
      , taskChecks = Array.fromList [ False, False, False, False ]
      }
    , Task.perform
        GetTime
        now
    )


type ModeStatus
    = Prologue
    | OnGoing Posix
    | Epilogue


type Mode
    = Pomodoro ModeStatus
    | ShortBreak ModeStatus
    | LongBreak ModeStatus


modeDuration : Mode -> Int
modeDuration mode =
    case mode of
        Pomodoro _ ->
            1000 * 60 * 25 + 900

        ShortBreak _ ->
            1000 * 60 * 5 + 900

        LongBreak _ ->
            1000 * 60 * 10 + 900


modeDurationPosix : Mode -> Posix
modeDurationPosix =
    modeDuration >> millisToPosix


modeElapsed : Mode -> Int -> Posix -> Float
modeElapsed mode duration currentTime =
    case mode of
        Pomodoro Prologue ->
            0.0

        Pomodoro (OnGoing startTime) ->
            let
                start =
                    posixToMillis startTime |> toFloat

                current =
                    posixToMillis currentTime |> toFloat
            in
            (current - start) / toFloat duration

        Pomodoro Epilogue ->
            1.0

        ShortBreak m ->
            modeElapsed (Pomodoro m) duration currentTime

        LongBreak m ->
            modeElapsed (Pomodoro m) duration currentTime



-- UPDATE


type Msg
    = GetTime Posix
    | SwitchMode Mode
    | RequestModeSwitch (Posix -> Mode)
    | CheckTask Int Bool


checkTimeUp : Posix -> Posix -> Mode -> Bool
checkTimeUp currentTime startTime mode =
    (posixToMillis currentTime - posixToMillis startTime)
        > modeDuration mode


checkTime : Mode -> Posix -> Maybe Mode
checkTime currentMode currentTime =
    case currentMode of
        Pomodoro (OnGoing startTime) ->
            if checkTimeUp currentTime startTime currentMode then
                Just (ShortBreak Prologue)

            else
                Nothing

        ShortBreak (OnGoing startTime) ->
            if checkTimeUp currentTime startTime currentMode then
                Just (Pomodoro Prologue)

            else
                Nothing

        LongBreak (OnGoing startTime) ->
            if checkTimeUp currentTime startTime currentMode then
                Just (Pomodoro Prologue)

            else
                Nothing

        _ ->
            Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CheckTask taskNum isChecked ->
            ( { model
                | taskChecks =
                    model.taskChecks
                        |> Array.set taskNum isChecked
              }
            , Cmd.none
            )

        GetTime newTime ->
            ( { model
                | time = newTime
                , mode =
                    checkTime model.mode newTime
                        |> Maybe.withDefault model.mode
              }
            , Process.sleep 100
                |> Task.andThen (always Time.now)
                |> Task.perform GetTime
            )

        SwitchMode newMode ->
            ( { model
                | mode = newMode
                , taskChecks =
                    case newMode of
                        LongBreak _ ->
                            Array.fromList [ False, False, False, False ]

                        _ ->
                            model.taskChecks
              }
            , Cmd.none
            )

        RequestModeSwitch timeToMode ->
            ( { model | modeStart = Just model.time }
            , Task.perform
                (timeToMode >> SwitchMode)
                Time.now
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


checkCount : Array Bool -> Int
checkCount =
    Array.toList
        >> List.filter ((==) True)
        >> List.length



-- VIEW


view : Model -> Document Msg
view model =
    { title = "I'm a tomato"
    , body =
        [ El.column
            [ El.centerY
            , El.width El.fill
            ]
            [ El.el
                [ El.centerX
                ]
                (displayTimeLeft model.time model.mode)
            , El.row
                [ El.centerX
                , El.spacing 20
                , El.height (El.px 100)
                ]
                (case model.mode of
                    Pomodoro Epilogue ->
                        [ checkbox (CheckTask 0) (model.taskChecks |> Array.get 0 |> Maybe.withDefault False)
                        , checkbox (CheckTask 1) (model.taskChecks |> Array.get 1 |> Maybe.withDefault False)
                        , checkbox (CheckTask 2) (model.taskChecks |> Array.get 2 |> Maybe.withDefault False)
                        , checkbox (CheckTask 3) (model.taskChecks |> Array.get 3 |> Maybe.withDefault False)
                        , In.button
                            []
                            { label =
                                El.row
                                    []
                                    [ El.el
                                        [ Font.color primaryColor
                                        , Font.size 16
                                        ]
                                        (El.text "Next ")
                                    , El.html (i [ A.class "fa fa-play", A.style "color" "rgb(0 209 178)" ] [])
                                    ]
                            , onPress =
                                Just
                                    (SwitchMode
                                        (if checkCount model.taskChecks == 4 then
                                            LongBreak (OnGoing model.time)

                                         else
                                            ShortBreak (OnGoing model.time)
                                        )
                                    )
                            }
                        ]

                    _ ->
                        []
                )
            , El.el
                [ El.centerX
                , El.centerY
                , El.height (El.px 70)
                ]
                (progressBar model.mode model.time)
            , El.wrappedRow
                [ El.spacing 20
                , El.centerX
                , El.centerY
                ]
                [ In.button
                    (case model.mode of
                        Pomodoro _ ->
                            activeButton

                        _ ->
                            passiveButton
                                (Ev.onClick <|
                                    RequestModeSwitch (OnGoing >> Pomodoro)
                                )
                    )
                    { onPress = Nothing, label = El.text "Potato" }
                , In.button
                    (case model.mode of
                        ShortBreak _ ->
                            activeButton

                        _ ->
                            passiveButton
                                (Ev.onClick <|
                                    RequestModeSwitch (OnGoing >> ShortBreak)
                                )
                    )
                    { onPress = Nothing, label = El.text "Short Break" }
                , In.button
                    (case model.mode of
                        LongBreak _ ->
                            activeButton

                        _ ->
                            passiveButton
                                (Ev.onClick <|
                                    RequestModeSwitch (OnGoing >> LongBreak)
                                )
                    )
                    { onPress = Nothing, label = El.text "Long Break" }
                ]
            , El.row
                [ El.height (El.px 200)
                , El.width El.fill
                ]
                []
            ]
        ]
    }


activeButton : List (El.Attribute msg)
activeButton =
    [ Bg.color primaryColor
    , Font.color (El.rgb 255 255 255)
    , El.paddingXY 16 12
    , Bd.rounded 5
    , El.width (El.px 140)
    , Font.center
    ]


passiveButton : El.Attribute msg -> List (El.Attribute msg)
passiveButton onClick =
    [ Bg.color primaryLightColor
    , Font.color (El.rgb 0 148 126)
    , El.paddingXY 16 12
    , Bd.rounded 5
    , El.width (El.px 140)
    , Font.center
    , onClick
    ]


checkbox : (Bool -> msg) -> Bool -> El.Element msg
checkbox onChange isChecked =
    let
        icon =
            if isChecked then
                "fa fa-check-square-o"

            else
                "fa fa-square-o"
    in
    El.column
        []
        [ In.checkbox
            [ El.width (El.px 30)
            ]
            { icon =
                El.html
                    (i [ A.class icon, A.style "color" "rgb(0 209 178)" ] [])
                    |> always
            , label = In.labelHidden "Task One"
            , checked = isChecked
            , onChange = onChange
            }
        ]

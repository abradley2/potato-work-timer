module Pages.Top exposing (Model, Msg, Params, page)

import Element as El
import Element.Background as Bg
import Element.Border as Bd
import Element.Events as Ev
import Element.Font as Font
import Element.Input as In
import Process
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url as Url exposing (Url)
import String exposing (padLeft, padRight)
import Task
import Time exposing (Posix, now, posixToMillis)


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



-- INIT


type alias Params =
    ()


type alias Model =
    { time : Posix
    , mode : Mode
    , modeStart : Maybe Posix
    }


displayTimeLeft : Posix -> Mode -> El.Element Msg
displayTimeLeft startPosix mode =
    case mode of
        Pomodoro (OnGoing val) ->
            timeView mode startPosix val

        ShortBreak (OnGoing val) ->
            timeView mode startPosix val

        LongBreak (OnGoing val) ->
            timeView mode startPosix val

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
      , mode = Pomodoro Prologue
      , modeStart = Nothing
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



-- UPDATE


type Msg
    = GetTime Posix
    | SwitchMode Mode
    | RequestModeSwitch (Posix -> Mode)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetTime newTime ->
            ( { model | time = newTime }
            , Process.sleep 100
                |> Task.andThen (always Time.now)
                |> Task.perform GetTime
            )

        SwitchMode newMode ->
            ( { model
                | mode = newMode
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
                , El.centerY
                , El.height (El.px 100)
                ]
                (displayTimeLeft model.time model.mode)
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

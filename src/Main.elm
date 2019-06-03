module Main exposing (main)

--import Html.Attributes exposing (..)
--import Html.Events exposing (..)

import Browser exposing (Document)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import List.Extra exposing (..)
import Task
import Time
import TimeZone exposing (asia__tokyo)



-- MAIN


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias StopwatchId =
    Int


type alias Stopwatch =
    { id : StopwatchId
    , name : String
    , logs : List LogRecord
    }


type alias LogRecord =
    { startTime : Time.Posix
    , endTime : Time.Posix
    , elapsedMillis : Int
    }


getElapsedSecond : Stopwatch -> Int
getElapsedSecond sw =
    (\s -> s // 1000) << List.sum <| List.map .elapsedMillis sw.logs


defaultStopwatch : StopwatchId -> Stopwatch
defaultStopwatch id =
    { id = id
    , name = ""
    , logs = []
    }


type alias WorkingState =
    { stopwatchId : StopwatchId
    , startTime : Time.Posix
    }


type alias Model =
    { stopwatches : List Stopwatch
    , workingState : Maybe WorkingState
    , currentStopwatchId : StopwatchId
    , currentTime : Maybe Time.Posix
    }


isWorking : Model -> StopwatchId -> Bool
isWorking model stopwatchId =
    case model.workingState of
        Just ws ->
            stopwatchId == ws.stopwatchId

        Nothing ->
            False


updateStopwatchByCond : (Stopwatch -> Bool) -> (Stopwatch -> Stopwatch) -> Model -> Model
updateStopwatchByCond cond f model =
    { model
        | stopwatches =
            List.map
                (\sw ->
                    if cond sw then
                        f sw

                    else
                        sw
                )
                model.stopwatches
    }


getStopwatchById : StopwatchId -> Model -> Maybe Stopwatch
getStopwatchById stopwatchId model =
    find (\sw -> stopwatchId == sw.id) model.stopwatches


type alias Hour =
    Int


type alias Minute =
    Int


type alias Second =
    Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( { stopwatches = []
      , workingState = Nothing
      , currentStopwatchId = 1
      , currentTime = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = DoNothing
    | Tick Time.Posix
    | AddStopwatch
    | RemoveStopwatch StopwatchId
    | Start StopwatchId
    | Stop StopwatchId
    | UpdateName StopwatchId String
    | ResetTime StopwatchId
    | InputTime String
    | ApplyTime


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            ( model
            , Cmd.none
            )

        Tick time ->
            ( { model | currentTime = Just time }
            , Cmd.none
            )

        AddStopwatch ->
            ( { model
                | stopwatches = model.stopwatches ++ [ defaultStopwatch model.currentStopwatchId ]
                , currentStopwatchId = model.currentStopwatchId + 1
              }
            , Cmd.none
            )

        RemoveStopwatch stopwatchId ->
            let
                nextWorkingState =
                    if isWorking model stopwatchId then
                        Nothing

                    else
                        model.workingState

                nextStopwatches =
                    List.filter (\sw -> sw.id /= stopwatchId) model.stopwatches
            in
            ( { model | workingState = nextWorkingState, stopwatches = nextStopwatches }
            , Cmd.none
            )

        -- 動いているのがあれば止めてからstartする
        Start stopwatchId ->
            let
                stoppedModel =
                    updateStop model (Maybe.withDefault -1 <| Maybe.map .stopwatchId model.workingState)

                startedModel =
                    updateStart stoppedModel stopwatchId
            in
            ( startedModel, Cmd.none )

        -- 該当stopwatchが動いてれば止める
        Stop stopwatchId ->
            ( updateStop model stopwatchId, Cmd.none )

        UpdateName stopwatchId name ->
            ( updateStopwatchByCond (\sw -> sw.id == stopwatchId) (\sw -> { sw | name = name }) model
            , Cmd.none
            )

        ResetTime stopwatchId ->
            ( updateStopwatchByCond (\sw -> sw.id == stopwatchId) (\sw -> { sw | logs = [] }) model
            , Cmd.none
            )

        InputTime time ->
            --let
            --  timeLength = String.length time
            --in
            --  if timeLength > 6 then  -- "hhmmdd"
            --    ( model
            --    , Cmd.none
            --    )
            --  else
            --    ( { model | inputTime = String.left 6 time }
            --    , Cmd.none
            --    )
            ( model, Cmd.none )

        ApplyTime ->
            --case strToTime model.inputTime of
            --  Just time ->
            --    ( { model | time = time }
            --    , Cmd.none
            --    )
            --  Nothing ->
            --    ( model
            --    , Cmd.none
            --    )
            ( model, Cmd.none )


updateStart : Model -> StopwatchId -> Model
updateStart model stopwatchId =
    let
        nextWorkingState =
            Maybe.map (\currentTime -> { stopwatchId = stopwatchId, startTime = currentTime }) model.currentTime
    in
    { model | workingState = nextWorkingState }



-- 該当stopwatchが動いてれば止める


updateStop : Model -> StopwatchId -> Model
updateStop model stopwatchId =
    let
        nextWorkingState =
            case model.workingState of
                Just ws ->
                    if ws.stopwatchId == stopwatchId then
                        Nothing

                    else
                        Just ws

                Nothing ->
                    Nothing

        log =
            Maybe.map2
                (\ws currentTime ->
                    { startTime = ws.startTime
                    , endTime = currentTime
                    , elapsedMillis = timeDiffMillis currentTime ws.startTime
                    }
                )
                model.workingState
                model.currentTime

        logList =
            Maybe.withDefault [] (Maybe.map List.singleton log)

        newModel =
            updateStopwatchByCond (\sw -> sw.id == stopwatchId)
                (\sw ->
                    { sw | logs = logList ++ sw.logs }
                )
                model
    in
    { newModel | workingState = nextWorkingState }



--strToTime : String -> Maybe Int
--strToTime str =
--  let
--    hour   = String.left  2 str
--    minute = String.left  4 str |> String.right 2
--    second = String.right 2 str
--  in
--    Maybe.map3 hmsToTime
--      (String.toInt hour)
--      (String.toInt minute)
--      (String.toInt second)
--hmsToTime : Hour -> Minute -> Second -> Int
--hmsToTime h m s =
--  h * 60 * 60 +
--  m * 60 +
--  s
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 10 Tick



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Stopwatch"
    , body =
        [ let
            workingSecond =
                Maybe.withDefault 0 << Maybe.map2 timeDiffSecond model.currentTime <| Maybe.map .startTime model.workingState
          in
          layout [] <|
            row [ spacing 30, alignTop ]
                (List.map
                    (\stopwatch ->
                        let
                            elapsedSecond =
                                getElapsedSecond stopwatch

                            timeSecond =
                                if isWorking model stopwatch.id then
                                    elapsedSecond + workingSecond

                                else
                                    elapsedSecond

                            hour =
                                to2digit <| String.fromInt (timeSecond // (60 * 60))

                            minute =
                                to2digit <| String.fromInt (modBy 60 (timeSecond // 60))

                            second =
                                to2digit <| String.fromInt (modBy 60 timeSecond)

                            ( buttonText, buttonColor, buttonAction ) =
                                if isWorking model stopwatch.id then
                                    ( "■stop", stopBGColor, Stop )

                                else
                                    ( "▶start", startBGColor, Start )

                            buttonProp =
                                { onPress = Just (buttonAction stopwatch.id)
                                , label = centerEL [] (text buttonText)
                                }
                        in
                        column [ centerX, spacing 15, alignTop ]
                            [ row [ spacing 20 ]
                                -- title row
                                [ Input.text []
                                    { onChange = UpdateName stopwatch.id
                                    , text = stopwatch.name
                                    , placeholder = Just (Input.placeholder [] (text "title"))
                                    , label = Input.labelHidden "msg"
                                    }
                                , Input.button [ Font.color (rgb255 255 0 0) ]
                                    { onPress = Just (RemoveStopwatch stopwatch.id)
                                    , label = text "✖"
                                    }
                                ]
                            , row [ centerX ]
                                -- stopwatch row
                                [ centerEL [ Font.size 55 ]
                                    (text (hour ++ ":" ++ minute ++ ":" ++ second))
                                ]
                            , row [ centerX, spacing 10 ]
                                -- button row
                                [ Input.button (normalButtonSize ++ [ Background.color buttonColor, centerX ])
                                    buttonProp
                                , Input.button (normalButtonSize ++ [ Background.color (rgb255 244 235 66), centerX ])
                                    { onPress = Just (ResetTime stopwatch.id)
                                    , label = centerEL [] (text "reset")
                                    }
                                ]
                            , column [ centerX ]
                                -- log row
                                (List.map
                                    (\log ->
                                        centerEL []
                                            (text <| (toTimeString log.startTime ++ " ~ " ++ toTimeString log.endTime))
                                    )
                                    stopwatch.logs
                                )
                            ]
                    )
                    model.stopwatches
                    ++ [ el [ alignTop, padding 10 ]
                            -- add button
                            (Input.button (normalButtonSize ++ [ Background.color (rgb255 22 167 237), centerX ])
                                { onPress = Just AddStopwatch
                                , label = centerEL [] (text "add")
                                }
                            )
                       ]
                )
        ]
    }


to2digit : String -> String
to2digit s =
    if String.length s >= 2 then
        s

    else
        "0" ++ s


toTimeFormat : String -> String
toTimeFormat time =
    let
        hour =
            String.left 2 time

        minute =
            String.left 4 time |> String.right 2

        second =
            String.right 2 time
    in
    hour ++ ":" ++ minute ++ ":" ++ second



-- UTIL


timeDiffMillis : Time.Posix -> Time.Posix -> Int
timeDiffMillis t1 t2 =
    Time.posixToMillis t1 - Time.posixToMillis t2


timeDiffSecond : Time.Posix -> Time.Posix -> Int
timeDiffSecond t1 t2 =
    (\s -> s // 1000) <| timeDiffMillis t1 t2



-- 何故かポイントフリースタイルにできない


normalButtonSize =
    [ width (px 100), height (px 50) ]


centerEL : List (Attribute msg) -> Element msg -> Element msg
centerEL l e =
    el ([ centerX, centerY ] ++ l) e


toTimeString : Time.Posix -> String
toTimeString posix =
    let
        timezone =
            asia__tokyo ()

        hour =
            posix
                |> Time.toHour timezone
                |> String.fromInt
                |> String.pad 2 '0'

        minute =
            posix
                |> Time.toMinute timezone
                |> String.fromInt
                |> String.pad 2 '0'

        second =
            posix
                |> Time.toSecond timezone
                |> String.fromInt
                |> String.pad 2 '0'
    in
    [ hour, minute, second ]
        |> String.join ":"



-- DATA


startBGColor =
    rgb255 66 137 244


stopBGColor =
    rgb255 244 31 31

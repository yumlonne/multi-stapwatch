port module Main exposing (main)

import Browser exposing (Document)
import Browser.Events
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Json.Decode
import Json.Encode
import Json.Encode.Extra
import List.Extra exposing (..)
import Maybe.Extra exposing (..)
import Task exposing (Task)
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
    , log : Log
    }


type alias Log =
    { records : List LogRecord

    -- for VIEW
    , input : Maybe String
    }


type LogRecord
    = Normal NormalLogRecord
    | Manual ManualInput


type alias NormalLogRecord =
    { startTime : Time.Posix
    , endTime : Time.Posix
    , elapsedMillis : Int
    }


type alias ManualInput =
    { offsetSecond : Int }


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



-- ENCODERS


logRecordEncoder : LogRecord -> Json.Encode.Value
logRecordEncoder logRecord =
    let
        ( objectName, encoder ) =
            case logRecord of
                Normal normalLogRecord ->
                    ( "normal"
                    , Json.Encode.object
                        [ ( "startTime", Json.Encode.int (Time.posixToMillis normalLogRecord.startTime) )
                        , ( "endTime", Json.Encode.int (Time.posixToMillis normalLogRecord.endTime) )
                        , ( "elapsedMillis", Json.Encode.int normalLogRecord.elapsedMillis )
                        ]
                    )

                Manual manualInput ->
                    ( "manual"
                    , Json.Encode.object
                        [ ( "offsetSecond", Json.Encode.int manualInput.offsetSecond ) ]
                    )
    in
    Json.Encode.object
        [ ( objectName, encoder ) ]


stopwatchEncoder : Stopwatch -> Json.Encode.Value
stopwatchEncoder sw =
    Json.Encode.object
        [ ( "id", Json.Encode.int sw.id )
        , ( "name", Json.Encode.string sw.name )
        , ( "log", Json.Encode.list logRecordEncoder sw.log.records )
        ]


workingStateEncoder : WorkingState -> Json.Encode.Value
workingStateEncoder workingState =
    Json.Encode.object
        [ ( "stopwatchId", Json.Encode.int workingState.stopwatchId )
        , ( "startTime", Json.Encode.int (Time.posixToMillis workingState.startTime) )
        ]


modelEncoder : Model -> Json.Encode.Value
modelEncoder model =
    Json.Encode.object
        [ ( "stopwatches", Json.Encode.list stopwatchEncoder model.stopwatches )
        , ( "workingState", Json.Encode.Extra.maybe workingStateEncoder model.workingState )
        , ( "currentStopwatchId", Json.Encode.int model.currentStopwatchId )

        -- currentTimeは不要
        ]


logRecordToString : LogRecord -> String
logRecordToString logRecord =
    case logRecord of
        Normal normalLogRecord ->
            toTimeString normalLogRecord.startTime ++ " ~ " ++ toTimeString normalLogRecord.endTime

        Manual manualInput ->
            "manual input: " ++ String.fromInt manualInput.offsetSecond ++ " sec."


logRecordToElapsedMillis : LogRecord -> Int
logRecordToElapsedMillis logRecord =
    case logRecord of
        Normal normalLogRecord ->
            normalLogRecord.elapsedMillis

        Manual manualInput ->
            manualInput.offsetSecond * 1000


setLogRecords : List LogRecord -> Log -> Log
setLogRecords records log =
    { log | records = records }


setLogInput : Maybe String -> Log -> Log
setLogInput strMaybe log =
    { log | input = strMaybe }


getElapsedSecond : Stopwatch -> Int
getElapsedSecond sw =
    (\s -> s // 1000) << List.sum <| List.map logRecordToElapsedMillis sw.log.records


defaultStopwatch : StopwatchId -> Stopwatch
defaultStopwatch id =
    { id = id
    , name = ""
    , log =
        { records = []
        , input = Nothing
        }
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
    | UpdateStartTime Time.Posix
    | ShowInputLog StopwatchId
    | InputTime StopwatchId String
    | ApplyManualInput StopwatchId


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
            let
                nextModel =
                    updateStopwatchByCond (\sw -> sw.id == stopwatchId) (\sw -> { sw | log = setLogRecords [] sw.log }) model
            in
            if Maybe.map .stopwatchId model.workingState == Just stopwatchId then
                ( nextModel
                , Task.perform UpdateStartTime getCurrentTime
                )

            else
                ( nextModel
                , Cmd.none
                )

        UpdateStartTime time ->
            let
                updateStartTime workingState =
                    { workingState | startTime = time }
            in
            ( { model | workingState = Maybe.map updateStartTime model.workingState }
            , Cmd.none
            )

        ShowInputLog stopwatchId ->
            let
                nextStopwatch =
                    case getStopwatchById stopwatchId model of
                        Just sw ->
                            let
                                log =
                                    sw.log
                            in
                            Just { sw | log = { log | input = Just "" } }

                        Nothing ->
                            Nothing

                nextModel =
                    nextStopwatch
                        |> Maybe.map (\sw -> updateStopwatchByCond (\s -> s.id == stopwatchId) (always sw) model)
                        |> Maybe.withDefault model
            in
            ( nextModel
            , Cmd.none
            )

        InputTime stopwatchId str ->
            let
                targetStopwatchMaybe =
                    getStopwatchById stopwatchId model

                nextModel =
                    targetStopwatchMaybe
                        |> Maybe.map
                            (\stopwatch ->
                                updateStopwatchByCond
                                    (\sw -> sw.id == stopwatchId)
                                    (\sw -> { sw | log = setLogInput (Just str) sw.log })
                                    model
                            )
                        |> Maybe.withDefault model
            in
            ( nextModel
            , Cmd.none
            )

        ApplyManualInput stopwatchId ->
            let
                _ =
                    Debug.log <| "ApplyManualInput" ++ String.fromInt stopwatchId

                targetStopwatchMaybe =
                    getStopwatchById stopwatchId model

                offsetSecMaybe =
                    targetStopwatchMaybe
                        |> Maybe.andThen (.log >> .input)
                        |> Maybe.andThen String.toInt

                nextModel =
                    offsetSecMaybe
                        |> Maybe.map (\offset -> ManualInput offset)
                        |> Maybe.map Manual
                        |> Maybe.map
                            (\manualInput ->
                                updateStopwatchByCond
                                    (\sw -> sw.id == stopwatchId)
                                    (\sw ->
                                        let
                                            log =
                                                setLogRecords (manualInput :: sw.log.records) sw.log
                                        in
                                        { sw | log = log }
                                    )
                                    model
                            )
                        |> Maybe.withDefault model
                        |> (\m ->
                                updateStopwatchByCond
                                    (\sw -> sw.id == stopwatchId)
                                    (\sw -> { sw | log = setLogInput Nothing sw.log })
                                    m
                           )
            in
            ( nextModel, Cmd.none )


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
            Maybe.withDefault [] (Maybe.map (List.singleton << Normal) log)

        newModel =
            updateStopwatchByCond (\sw -> sw.id == stopwatchId)
                (\sw ->
                    { sw | log = setLogRecords (logList ++ sw.log.records) sw.log }
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
    Browser.Events.onAnimationFrame Tick



-- COMMAND


getCurrentTime : Task Never Time.Posix
getCurrentTime =
    Time.now



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
                                [ Input.button (normalButtonSize ++ [ Background.color buttonColor, centerX, Border.rounded 5 ])
                                    buttonProp
                                , Input.button (normalButtonSize ++ [ Background.color (rgb255 244 235 66), centerX, Border.rounded 5 ])
                                    { onPress = Just (ResetTime stopwatch.id)
                                    , label = centerEL [] (text "reset")
                                    }
                                ]
                            , column [ centerX ]
                                -- log row
                                ([ row [ spacing 20 ]
                                    [ el [ Font.size 30, Font.bold ] (text "Logs")
                                    , Input.button
                                        [ width <| px 25
                                        , height <| px 25
                                        , Background.color <| rgb255 64 255 64
                                        , Border.rounded 12
                                        ]
                                        { label =
                                            el
                                                [ Font.bold
                                                , centerX
                                                , centerY
                                                , Font.color <| rgb255 255 255 255
                                                ]
                                            <|
                                                text "+"
                                        , onPress = Just <| ShowInputLog stopwatch.id
                                        }
                                    ]
                                 ]
                                    ++ (toList <| manualInputLogView stopwatch)
                                    ++ List.map
                                        (\log ->
                                            centerEL []
                                                (text <| logRecordToString log)
                                        )
                                        stopwatch.log.records
                                )
                            ]
                    )
                    model.stopwatches
                    ++ [ el [ alignTop, padding 10 ]
                            -- add button
                            (Input.button (normalButtonSize ++ [ Background.color (rgb255 22 167 237), centerX, Border.rounded 5 ])
                                { onPress = Just AddStopwatch
                                , label = centerEL [] (text "add")
                                }
                            )
                       ]
                )
        ]
    }



-- VIEW Components


manualInputLogView : Stopwatch -> Maybe (Element Msg)
manualInputLogView stopwatch =
    stopwatch.log.input
        |> Maybe.map
            (\inputStr ->
                Input.text [ Events.onLoseFocus <| ApplyManualInput stopwatch.id, Input.focusedOnLoad ]
                    { label = Input.labelHidden ""
                    , onChange = InputTime stopwatch.id
                    , placeholder = Nothing
                    , text = inputStr
                    }
            )


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

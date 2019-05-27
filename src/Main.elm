import Browser
import Html exposing (Html)
--import Html.Attributes exposing (..)
--import Html.Events exposing (..)
import Task
import Time
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import List.Extra exposing (..)

-- MAIN

main = Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias StopwatchId = Int
type alias Stopwatch =
  { id   : StopwatchId
  , name : String
  , time : Int
  }

defaultStopwatch : StopwatchId -> Stopwatch
defaultStopwatch id =
  { id   = id
  , name = ""
  , time = 0
  }

type alias Model =
  { stopwatches : List Stopwatch
  , workingStopwatchId : Maybe StopwatchId
  , currentStopwatchId : Int
  }

isWorking : Model -> Stopwatch -> Bool
isWorking model stopwatch =
  case model.workingStopwatchId of
    Just id -> stopwatch.id == id
    Nothing -> False

updateStopwatchByCond : (Stopwatch -> Bool) -> (Stopwatch -> Stopwatch) -> Model -> Model
updateStopwatchByCond cond f model =
  { model | stopwatches = List.map (\sw -> if cond sw then f sw else sw) model.stopwatches }

getStopwatchById : StopwatchId -> Model -> Maybe Stopwatch
getStopwatchById stopwatchId model =
  find (\sw -> stopwatchId == sw.id) model.stopwatches

type alias Hour   = Int
type alias Minute = Int
type alias Second = Int

init : () -> (Model, Cmd Msg)
init _ =
  ( { stopwatches = []
    , workingStopwatchId = Nothing
    , currentStopwatchId = 1
    }
  , Cmd.none)


-- UPDATE

type Msg
  = DoNothing
  | Tick
  | AddStopwatch
  | RemoveStopwatch StopwatchId
  | SwitchState StopwatchId
  | UpdateName StopwatchId String
  | ResetTime StopwatchId
  | InputTime String
  | ApplyTime

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DoNothing ->
      ( model
      , Cmd.none
      )

    Tick ->
      let
        targetStopwatchId = Maybe.withDefault -1 model.workingStopwatchId
      in
        ( updateStopwatchByCond (\sw -> sw.id == targetStopwatchId) (\sw -> { sw | time = sw.time + 1 }) model
        , Cmd.none
        )

    AddStopwatch ->
      ( { model | stopwatches = (model.stopwatches ++ [ defaultStopwatch model.currentStopwatchId])
        , currentStopwatchId = model.currentStopwatchId + 1
        }
      , Cmd.none
      )
    RemoveStopwatch stopwatchId ->
      let
        nextWorkingStopwatchId = if (Maybe.withDefault -1 model.workingStopwatchId) == stopwatchId then Nothing else model.workingStopwatchId
        nextStopwatches = List.filter (\sw -> sw.id /= stopwatchId) model.stopwatches
      in
        ( { model | workingStopwatchId = nextWorkingStopwatchId, stopwatches = nextStopwatches }
        , Cmd.none
        )

    SwitchState stopwatchId ->
      let
        nextWorkingStopwatchId =
          case model.workingStopwatchId of
            Just workingStopwatchId ->
              if stopwatchId == workingStopwatchId then Nothing else Just stopwatchId
            Nothing ->
              Just stopwatchId
      in
        ( { model | workingStopwatchId = nextWorkingStopwatchId }
        , Cmd.none )

    UpdateName stopwatchId name ->
      ( updateStopwatchByCond (\sw -> sw.id == stopwatchId) (\sw -> { sw | name = name }) model
      , Cmd.none
      )

    ResetTime stopwatchId ->
      ( updateStopwatchByCond (\sw -> sw.id == stopwatchId) (\sw -> { sw | time = 0 }) model
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



strToTime : String -> Maybe Int
strToTime str =
  let
    hour   = String.left  2 str
    minute = String.left  4 str |> String.right 2
    second = String.right 2 str
  in
    Maybe.map3 hmsToTime
      (String.toInt hour)
      (String.toInt minute)
      (String.toInt second)


hmsToTime : Hour -> Minute -> Second -> Int
hmsToTime h m s =
  h * 60 * 60 +
  m * 60 +
  s


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 (always Tick)

-- VIEW

view : Model -> Html Msg
view model =
  layout [] <|
    row [ spacing 30 ]
      (
        ( List.map (\stopwatch ->
          let
            hour   = to2digit <| String.fromInt (stopwatch.time // (60 * 60))
            minute = to2digit <| String.fromInt (modBy 60 (stopwatch.time // 60))
            second = to2digit <| String.fromInt (modBy 60 stopwatch.time)
            buttonText  = if isWorking model stopwatch then "■stop" else "▶start"
            buttonColor = if isWorking model stopwatch then stopBGColor else startBGColor
          in
            column [ centerX, spacing 15]
            [ row [ spacing 20 ]  -- title row
              [ Input.text []
                { onChange = UpdateName stopwatch.id
                , text = stopwatch.name
                , placeholder = Just (Input.placeholder [] (text "title"))
                , label = Input.labelHidden "msg"
                }
                ,
                Input.button [ Font.color (rgb255 255 0 0) ]
                { onPress = Just (RemoveStopwatch stopwatch.id)
                , label = text "✖"
                }
              ]
              ,
              row [ centerX ]  -- stopwatch row
              [ centerEL [ Font.size 55 ]
                (text (hour ++ ":" ++ minute ++ ":" ++ second))
              ]
              ,
              row [ centerX, spacing 10] -- button row
              [ Input.button (normalButtonSize ++ [ Background.color buttonColor, centerX ])
                { onPress = Just (SwitchState stopwatch.id)
                , label = centerEL [] (text buttonText)
                }
                ,
                Input.button (normalButtonSize ++ [ Background.color (rgb255 244 235 66), centerX])
                { onPress = Just (ResetTime stopwatch.id)
                , label = centerEL [] (text "reset")
                }
              ]
            ]
          ) model.stopwatches
        )
        ++
        [ el []
          ( Input.button (normalButtonSize ++ [ Background.color (rgb255 22 167 237), centerX ])
            { onPress = Just AddStopwatch
            , label = centerEL [] (text "add")
            }
          )
        ]
      )

to2digit : String -> String
to2digit s = if String.length s >= 2 then s else "0" ++ s

toTimeFormat : String -> String
toTimeFormat time =
  let
    hour   = String.left  2 time
    minute = String.left  4 time |> String.right 2
    second = String.right 2 time
  in
    hour ++ ":" ++ minute ++ ":" ++ second


-- UTIL
normalButtonSize = [ width (px 100), height (px 50) ]

centerEL : List (Attribute msg) -> Element msg -> Element msg
centerEL l e = el ([centerX, centerY] ++ l) e

-- DATA

startBGColor = rgb255 66 137 244
stopBGColor  = rgb255 244 31 31

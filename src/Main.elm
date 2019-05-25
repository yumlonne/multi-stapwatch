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

-- MAIN

main = Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { name : String
  , time : Int
  , isWorking : Bool
  , inputTime : String
  }

type alias Hour   = Int
type alias Minute = Int
type alias Second = Int

init : () -> (Model, Cmd Msg)
init _ =
  ( { name = ""
    , time = 0
    , isWorking = False
    , inputTime = ""
    }
  , Cmd.none
  )


-- UPDATE

type Msg
  = DoNothing
  | Tick
  | SwitchWatchState
  | UpdateName String
  | ResetTime
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
      if model.isWorking then
        ( { model | time = model.time + 1 }
        , Cmd.none
        )
      else
      ( model
      , Cmd.none
      )

    SwitchWatchState ->
      ( { model | isWorking = not model.isWorking }
      , Cmd.none
      )

    UpdateName name ->
      ( { model | name = name }
      , Cmd.none
      )

    ResetTime ->
      ( { model | time = 0 }
      , Cmd.none
      )

    InputTime time ->
      let
        timeLength = String.length time
      in
        if timeLength > 6 then  -- "hhmmdd"
          ( model
          , Cmd.none
          )
        else
          ( { model | inputTime = String.left 6 time }
          , Cmd.none
          )

    ApplyTime ->
      case strToTime model.inputTime of
        Just time ->
          ( { model | time = time }
          , Cmd.none
          )
        Nothing ->
          ( model
          , Cmd.none
          )



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
  let
    hour   = to2digit <| String.fromInt (model.time // (60 * 60))
    minute = to2digit <| String.fromInt (modBy 60 (model.time // 60))
    second = to2digit <| String.fromInt (modBy 60 model.time)
    buttonText = if model.isWorking then "■stop" else "▶start"
    buttonColor = if model.isWorking then stopBGColor else startBGColor
  in
    layout [] <|
      column [ centerX, spacing 40]
        [ row [ spacing 20 ]  -- title row
          [ Input.text []
            { onChange = UpdateName
            , text = model.name
            , placeholder = Just (Input.placeholder [] (text "Title"))
            , label = Input.labelHidden "msg"
            }
            ,
            Input.button [ Font.color (rgb255 255 0 0) ]
            { onPress = Nothing -- TODO
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
            { onPress = Just SwitchWatchState
            , label = centerEL [] (text buttonText)
            }
            ,
            Input.button (normalButtonSize ++ [ Background.color (rgb255 244 235 66), centerX])
            { onPress = Just ResetTime
            , label = centerEL [] (text "reset")
            }
          ]
        ]
--        [ input [ type_ "", placeholder "title", onInput UpdateName, style "font-size" "100px" ] []
--        , h1 [] [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]
--        , input [ type_ "text", placeholder "hh:mm:ss", onInput InputTime ] [ text (toTimeFormat model.inputTime) ]
--        , button [ onClick ApplyTime ] [ text "apply" ]
--        , button [ onClick SwitchWatchState ] [ text buttonText ]
--        , button [ onClick ResetTime ] [ text "reset" ]
--        ]

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

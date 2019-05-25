import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task
import Time

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
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( { name = ""
    , time = 0
    , isWorking = False
    }
  , Cmd.none
  )


-- UPDATE

type Msg
  = Tick
  | SwitchWatchState
  | UpdateName String
  | ResetTime

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
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
    buttonText = if model.isWorking then "stop" else "start"
  in
    div []
      [ input [ type_ "", placeholder "title", onInput UpdateName, style "font-size" "100px" ] []
      , h1 [] [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]
      , button [ onClick SwitchWatchState ] [ text buttonText ]
      , button [ onClick ResetTime ] [ text "reset" ]
      ]

to2digit : String -> String
to2digit s = if String.length s >= 2 then s else "0" ++ s

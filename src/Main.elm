module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Browser
import Time
import Time.Extra as Time
import Task

main : Program () Model Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Millis = Int

updated_time : Millis -> Time.Posix -> Time.Posix -> Millis
updated_time offset start cur_time =
  offset + Time.diff Time.Millisecond Time.utc start cur_time

type TimerState
  = Stopped  Millis
  | Starting Millis
  | Running  Millis Time.Posix
  | Stopping Millis Time.Posix

type alias Model =
  { state : TimerState
  , now   : Time.Posix
  }

init : () -> (Model, Cmd Msg)
init flags =
  ( Model (Stopped 0) (Time.millisToPosix 0), Cmd.none )


-- UPDATE

type Msg
  = Start
  | Stop
  | Reset
  | Tick Time.Posix

display_time : Model -> Millis
display_time model =
  case model.state of
    Stopped interval ->
      interval
    Starting interval ->
      interval
    Running interval start ->
      updated_time interval start model.now
    Stopping interval start ->
      updated_time interval start model.now

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Start ->
      case model.state of
        Stopped interval ->
          ( Model (Starting interval) model.now
          , Task.perform Tick Time.now
          )

        _ ->
          ( model, Cmd.none )

    Stop ->
      case model.state of
        Running interval start ->
          ( Model (Stopping interval start) model.now
          , Task.perform Tick Time.now
          )

        _ ->
          ( model, Cmd.none )

    Reset ->
      ( Model (Stopped 0) model.now, Cmd.none )

    Tick cur_time ->
      case model.state of
        Starting interval ->
          ( Model (Running interval cur_time) cur_time, Cmd.none )

        Stopping interval start ->
          ( Model (Stopped (updated_time interval start cur_time)) cur_time, Cmd.none )

        _ ->
          ( Model model.state cur_time, Cmd.none )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick


-- VIEW

get_timer : Model -> Html Msg
get_timer model =
  div []
    [ text (String.fromInt (display_time model))
    , button [ onClick Start ] [ text "Start" ]
    , button [ onClick Stop ] [text "Stop" ]
    , button [ onClick Reset ] [text "Reset" ]
    ]

get_html : Model -> Browser.Document Msg
get_html model =
  { title = "Elm Timecard"
  , body = [ get_timer model ]
  }

view : Model -> Browser.Document Msg
view model =
  get_html model

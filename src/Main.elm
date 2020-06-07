module Main exposing (..)

import Html exposing (..)
import Browser
import Time
import Time.Extra as Time

main : Program () Model Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type TimerState
  = Stopped  Time.Interval
  | Starting Time.Interval
  | Running  Time.Interval Time.Posix
  | Stopping Time.Interval Time.Posix

type alias Model = TimerState

init : () -> (Model, Cmd Msg)
init flags =
  (Stopped 0, Cmd.none)


-- UPDATE

type Msg
  = Start
  | Stop
  | Reset

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Start ->
      case model of
        Stopped interval ->
          Starting interval
        Starting interval ->
          Starting interval
        Running interval start ->
          Running interval start
        Stopping interval start ->
          Stopping interval start
    Stop ->
      case model of
        Stopped interval ->
          Stopped interval
        Starting interval ->
          Starting interval
        Running interval start ->
          Stopping interval start
        Stopping interval start ->
          Stopping interval start
    Reset ->
      Stopped 0

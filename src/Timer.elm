module Timer exposing (..)


import Time
import Time.Extra as Time
import Task
import Html exposing (..)
import Html.Events exposing (..)

-- MODEL

type alias Millis = Int

type Timer
  = Stopped Millis
  | Starting Millis
  | Running  Millis Time.Posix
  | Stopping Millis Time.Posix

updated_time : Millis -> Time.Posix -> Time.Posix -> Millis
updated_time offset start cur_time =
  offset + Time.diff Time.Millisecond Time.utc start cur_time

display_time : Time.Posix -> Timer -> Millis
display_time cur_time timer =
  case timer of
    Stopped interval ->
      interval
    Starting interval ->
      interval
    Running interval start ->
      updated_time interval start cur_time
    Stopping interval start ->
      updated_time interval start cur_time


-- UPDATE

type Msg
  = Start
  | Stop
  | Reset
  | ImmediateUpdate Time.Posix

update_state : Msg -> Timer -> (Timer, Cmd Msg)
update_state msg timer =
  case msg of
      Start ->
        case timer of
          Stopped interval ->
            ( Starting interval
            , Task.perform ImmediateUpdate Time.now
            )

          _ ->
            ( timer, Cmd.none )

      Stop ->
        case timer of
          Running interval start ->
            ( Stopping interval start
            , Task.perform ImmediateUpdate Time.now
            )

          _ ->
            ( timer, Cmd.none )

      Reset ->
        ( Stopped 0, Cmd.none )

      ImmediateUpdate cur_time ->
        time_set cur_time timer

time_set : Time.Posix -> Timer -> (Timer, Cmd Msg)
time_set cur_time timer =
    case timer of
      Starting interval ->
        (Running interval cur_time, Cmd.none )

      Stopping interval start ->
        ( Stopped (updated_time interval start cur_time), Cmd.none )

      _ ->
        ( timer, Cmd.none )


-- VIEW

get_timer_html : Time.Posix -> Timer -> Html Msg
get_timer_html cur_time timer =
  div []
    [ text (String.fromInt (display_time cur_time timer))
    , button [ onClick Start ] [ text "Start" ]
    , button [ onClick Stop ] [text "Stop" ]
    , button [ onClick Reset ] [text "Reset" ]
    ]

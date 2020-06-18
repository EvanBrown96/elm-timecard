module Timer exposing (Millis, Timer, newTimer, displayTime, isRunning, Msg(..), updateState)


import Time
import Time.Extra as Time
import Task


-- MODEL

type alias Millis = Int

type Timer =
  Timer TimerState

newTimer : Timer
newTimer = Timer <| Stopped 0

type TimerState =
    Stopped Millis
  | Starting Millis
  | Running  Millis Time.Posix
  | Stopping Millis Time.Posix


-- UPDATE

type Msg =
    Start
  | Stop
  | Reset
  | Update Time.Posix

updateState : Msg -> Timer -> (Timer, Cmd Msg)
updateState msg timer =
  case msg of
      Start ->
        case timer of

          Timer (Stopped interval) ->
            ( Timer (Starting interval)
            , Task.perform Update Time.now
            )

          _ ->
            ( timer, Cmd.none )

      Stop ->
        case timer of
          Timer (Running interval start) ->
            ( Timer (Stopping interval start)
            , Task.perform Update Time.now
            )

          _ ->
            ( timer, Cmd.none )

      Reset ->
        ( Timer (Stopped 0), Cmd.none )

      Update cur_time ->
        timeSet cur_time timer

timeSet : Time.Posix -> Timer -> (Timer, Cmd Msg)
timeSet cur_time timer =
    case timer of
      Timer state ->
        case state of
          Starting interval ->
            ( Timer (Running interval cur_time), Cmd.none )

          Stopping interval start ->
            ( Timer (Stopped (updatedTime cur_time start interval))
            , Cmd.none
            )

          _ ->
            ( timer, Cmd.none )


-- HELPERS

isRunning : Timer -> Bool
isRunning timer =
  case timer of
    Timer state ->
      case state of
        Stopped _ ->
          False
        Starting _ ->
          False
        _ ->
          True

updatedTime : Time.Posix -> Time.Posix -> Millis -> Millis
updatedTime cur_time start offset =
  offset + Time.diff Time.Millisecond Time.utc start cur_time

displayTime : Time.Posix -> Timer -> String
displayTime cur_time timer =
  millisToString <|
    case timer of
      Timer state ->
        case state of
          Stopped interval ->
            interval
          Starting interval ->
            interval
          Running interval start ->
            updatedTime cur_time start interval
          Stopping interval start ->
            updatedTime cur_time start interval

millisToString : Millis -> String
millisToString millis =
  let
    hours   = millis // 3600000
    minutes = (modBy 3600000 millis) // 60000
    seconds = (modBy 60000 millis) // 1000
    centis  = (modBy 1000 millis) // 10
    padZero = String.padLeft 2 '0'
  in
    String.fromInt hours
    ++ ":" ++ (String.fromInt minutes |> padZero)
    ++ ":" ++ (String.fromInt seconds |> padZero)
    ++ "." ++ (String.fromInt centis  |> padZero)

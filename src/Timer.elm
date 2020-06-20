module Timer exposing (Millis, Timer, newTimer, displayTime, isRunning, Msg(..), updateState, forwardToIndex)


import Time
import Time.Extra as Time
import Task


-- MODEL

type alias Millis = Int

type alias Timer =
  { name  : String
  , state : TimerState
  }

newTimer : String ->Timer
newTimer name =
  Timer name <| Stopped 0

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
        case timer.state  of
          Stopped interval ->
            ( { timer | state = Starting interval }
            , Task.perform Update Time.now
            )

          _ ->
            ( timer, Cmd.none )

      Stop ->
        case timer.state of
          Running interval start ->
            ( { timer | state = Stopping interval start }
            , Task.perform Update Time.now
            )

          _ ->
            ( timer, Cmd.none )

      Reset ->
        ( newTimer timer.name, Cmd.none )

      Update cur_time ->
        timeSet cur_time timer

timeSet : Time.Posix -> Timer -> (Timer, Cmd Msg)
timeSet cur_time timer =
    case timer.state of
        Starting interval ->
          ( { timer | state = Running interval cur_time }, Cmd.none )

        Stopping interval start ->
          ( { timer | state = Stopped (updatedTime cur_time start interval) }
          , Cmd.none
          )

        _ ->
          ( timer, Cmd.none )


-- HELPERS

isRunning : Timer -> Bool
isRunning timer =
  case timer.state of
    Stopped _ ->
      False
    Stopping _ _ ->
      False
    _ ->
      True

updatedTime : Time.Posix -> Time.Posix -> Millis -> Millis
updatedTime cur_time start offset =
  offset + Time.diff Time.Millisecond Time.utc start cur_time

displayTime : Time.Posix -> Timer -> String
displayTime cur_time timer =
  millisToString <|
    case timer.state of
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

forwardToIndex : (Int -> Msg -> parent) -> List Timer -> Int -> Msg -> (List Timer, Cmd parent)
forwardToIndex parentConstructor timers index msg =
  let
    start = List.take index timers
    end   = List.drop index timers
  in
    case end of
      x::xs ->
        let
          updated  = updateState msg x
        in
          ( start ++ Tuple.first updated :: xs
          , Cmd.map (parentConstructor index) (Tuple.second updated)
          )

      _ ->
        ( timers, Cmd.none )

module TimerGroup exposing (..)


import Timer exposing (Timer)
import Time


type TimerGroup =
  TimerGroup Timer (List Timer)

newTimerGroup : String -> TimerGroup
newTimerGroup name =
  TimerGroup (Timer.newTimer name) []

type Msg =
    Reset
  | UpdateAll Time.Posix
  | Start Int
  | Stop Int
  | Update Int Time.Posix

cmdHandler : Int -> Timer.Msg -> Msg
cmdHandler index msg =
  case msg of
    Timer.Start ->
      Start index
    Timer.Stop ->
      Stop index
    Timer.Reset ->
      Reset
    Timer.Update time ->
      Update index time



updateState : Msg -> TimerGroup -> (TimerGroup, Cmd Msg)
updateState msg timer_group =
  case timer_group of
    TimerGroup main children ->
      case msg of
        Reset ->
          ( TimerGroup (Timer.newTimer main.name) (List.map (.name >> Timer.newTimer) children)
          , Cmd.none
          )

        Start index ->
          let
            ( updated_main, _ ) = Timer.updateState Timer.Start main
          in
            Timer.forwardToIndex cmdHandler children index Timer.Start
            |> Tuple.mapFirst (TimerGroup updated_main)

        Stop index ->
          let
            ( updated_children, cmd ) = Timer.forwardToIndex cmdHandler children index Timer.Stop
            all_stopped               = List.filter Timer.isRunning updated_children
                                        |> List.isEmpty
            ( updated_main, _ )       = if all_stopped
                                        then Timer.updateState Timer.Stop main
                                        else ( main, Cmd.none )
          in
            ( TimerGroup updated_main updated_children, cmd )

        Update index cur_time ->
          let
            ( updated_main, _ ) = Timer.updateState (Timer.Update cur_time) main
          in
            Timer.forwardToIndex cmdHandler children index (Timer.Update cur_time)
            |> Tuple.mapFirst (TimerGroup updated_main)

        UpdateAll cur_time ->
          let
            ( updated_main, _ ) = Timer.updateState (Timer.Update cur_time) main
          in
            List.map (Timer.updateState (Timer.Update cur_time)) children
            |> List.unzip
            |> Tuple.mapBoth
                 (TimerGroup updated_main)
                 (\l -> List.indexedMap (\i -> Cmd.map (cmdHandler i)) l |> Cmd.batch )

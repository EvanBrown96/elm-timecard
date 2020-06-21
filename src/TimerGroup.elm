module TimerGroup exposing (..)


import Timer exposing (Timer)
import Time


type TimerGroup =
  TimerGroup Bool Timer (List Timer)

newTimerGroup : Bool -> String -> TimerGroup
newTimerGroup exclusive name =
  TimerGroup exclusive (Timer.newTimer name) []

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
    TimerGroup exclusive main children ->
      case msg of
        Reset ->
          ( TimerGroup exclusive (Timer.newTimer main.name) (List.map (.name >> Timer.newTimer) children)
          , Cmd.none
          )

        Start index ->
          let
            ( updated_main, _ ) = Timer.updateState Timer.Start main
          in
            case exclusive of
              False ->
                Timer.forwardToIndex cmdHandler children index Timer.Start
                |> Tuple.mapFirst (TimerGroup exclusive updated_main)

              True ->
                let
                  start = List.take index children
                  end   = List.drop index children
                in
                  case end of
                    x::xs ->
                      List.map (Timer.updateState Timer.Stop) start
                      ++ Timer.updateState Timer.Start x
                      :: List.map (Timer.updateState Timer.Stop) xs
                      |> List.unzip
                      |> Tuple.mapBoth
                           (TimerGroup exclusive updated_main)
                           (\l -> List.indexedMap (\i -> Cmd.map (cmdHandler i)) l |> Cmd.batch )

                    _ -> ( timer_group, Cmd.none )

        Stop index ->
          let
            ( updated_children, cmd ) = Timer.forwardToIndex cmdHandler children index Timer.Stop
            all_stopped               = List.all (not << Timer.isRunning) updated_children
            ( updated_main, _ )       = if all_stopped
                                        then Timer.updateState Timer.Stop main
                                        else ( main, Cmd.none )
          in
            ( TimerGroup exclusive updated_main updated_children, cmd )

        Update index cur_time ->
          let
            ( updated_main, _ ) = Timer.updateState (Timer.Update cur_time) main
          in
            Timer.forwardToIndex cmdHandler children index (Timer.Update cur_time)
            |> Tuple.mapFirst (TimerGroup exclusive updated_main)

        UpdateAll cur_time ->
          let
            ( updated_main, _ ) = Timer.updateState (Timer.Update cur_time) main
          in
            List.map (Timer.updateState (Timer.Update cur_time)) children
            |> List.unzip
            |> Tuple.mapBoth
                 (TimerGroup exclusive updated_main)
                 (\l -> List.indexedMap (\i -> Cmd.map (cmdHandler i)) l |> Cmd.batch )

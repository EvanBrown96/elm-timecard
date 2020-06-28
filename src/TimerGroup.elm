module TimerGroup exposing (TimerGroup, newTimerGroup, Msg, updateState, view)


import Timer exposing (Timer)
import Time
import Element exposing (..)
import CommonElements exposing (timerSegment)
import Element.Input as Input


-- MODEL

type TimerGroup =
  TimerGroup Bool Timer (List Timer)

newTimerGroup : Bool -> String -> TimerGroup
newTimerGroup exclusive name =
  TimerGroup exclusive (Timer.newTimer name) []


-- UPDATE

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

timerGroupForward : List Timer -> Int -> Timer.Msg -> (List Timer, Cmd Msg)
timerGroupForward =
  Timer.forwardToIndex cmdHandler

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
                timerGroupForward children index Timer.Start
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
            ( updated_children, cmd ) = timerGroupForward children index Timer.Stop
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
            timerGroupForward children index (Timer.Update cur_time)
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


-- VIEW

view : Time.Posix -> TimerGroup -> Element Msg
view cur_time timer_group =
  case timer_group of
    TimerGroup exclusive main children ->
        column [] <|
          timerSegment
            none
            (el [ centerY ] <| text main.name)
            (el [ centerY ] <| text <| Timer.displayTime cur_time main)
            (Input.button [ centerX, centerY ]
               { onPress = Just Reset, label = text "Reset" })
          :: List.indexedMap
              (\i t -> timerSegment
                (Input.button [ centerX, centerY ] <|
                   if Timer.isRunning t then
                     { onPress = Just (Stop i), label = text "Stop" }
                   else
                     { onPress = Just (Start i), label = text "Start" })
                (el [ centerY ] <| text t.name)
                (el [ centerY ] <| text <| Timer.displayTime cur_time t)
                none
              ) children

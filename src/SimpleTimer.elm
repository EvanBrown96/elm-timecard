module SimpleTimer exposing (..)


import Time
import Timer exposing (Timer)
import Element exposing (..)
import Element.Background as BG
import Element.Input as Input
import CommonElements exposing (timerSegment)


-- MODEL

type alias SimpleTimer =
  Timer


-- UPDATE

type alias Msg =
  Timer.Msg


-- VIEW

view : Time.Posix -> SimpleTimer -> Element Msg
view cur_time timer =
  timerSegment
    (Input.button [ centerX, centerY ] <|
       if Timer.isRunning timer then
         { onPress = Just Timer.Stop, label = text "Stop" }
       else
         { onPress = Just Timer.Start, label = text "Start" })
    (el [ centerY ] <| text timer.name)
    (el [ centerY ] <| text <| Timer.displayTime cur_time timer)
    (Input.button [ centerX, centerY ]
       { onPress = Just Timer.Reset, label = text "Reset" })

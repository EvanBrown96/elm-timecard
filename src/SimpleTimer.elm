module SimpleTimer exposing (..)


import Time
import Timer exposing (Timer)
-- import Html exposing (..)
-- import Html.Events exposing (..)
-- import Html.Attributes exposing (..)
-- import Bootstrap.Card as Card
-- import Bootstrap.Card.Block as Block
-- import Bootstrap.Utilities.Size as Size
-- import Bootstrap.Grid as Grid
-- import Bootstrap.Grid.Row as Row
-- import Bootstrap.Grid.Col as Col
-- import Bootstrap.Utilities.Spacing as Spacing
-- import HtmlElements exposing (..)
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
    (Input.button [] <|
       if Timer.isRunning timer then
         { onPress = Just Timer.Stop, label = text "Stop" }
       else
         { onPress = Just Timer.Start, label = text "Start" })
    (text timer.name)
    (text <| Timer.displayTime cur_time timer)
    (Input.button [] { onPress = Just Timer.Reset, label = text "Reset" })

module SimpleTimer exposing (..)


import Time
import Timer
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Utilities.Size as Size


-- MODEL

type alias SimpleTimer =
  { name  : String
  , timer : Timer.Timer
  }

-- UPDATE

updateState : Timer.Msg -> SimpleTimer -> (SimpleTimer, Cmd Timer.Msg)
updateState msg timer =
  Timer.updateState msg timer.timer
  |> Tuple.mapFirst (\t -> { timer | timer = t })


-- VIEW

getHtml : Time.Posix -> SimpleTimer -> Html Timer.Msg
getHtml cur_time timer =
  Card.group
    [ Card.config [ Card.secondary ]
      |> Card.block []
          [ Block.custom <|
              div []
                [ if Timer.isRunning timer.timer then
                    button [ onClick Timer.Stop ] [ text "Stop" ]
                  else
                      button [ onClick Timer.Start ] [ text "Start" ]
                ]
          ]
    , Card.config [ Card.light ]
      |> Card.block [] [ Block.titleH4 [] [ text timer.name ] ]
    , Card.config [ Card.light ]
      |> Card.block [] [ Block.text [] [ text <| Timer.displayTime cur_time timer.timer ] ]
    , Card.config [ Card.secondary ]
      |> Card.block []
          [ Block.custom <|
              div [] [ button [ onClick Timer.Reset ] [ text "Reset" ] ]
          ]
    ]

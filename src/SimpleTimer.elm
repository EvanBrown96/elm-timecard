module SimpleTimer exposing (..)


import Time
import Timer exposing (Timer)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Utilities.Size as Size


-- MODEL

type alias SimpleTimer =
  Timer

-- UPDATE

type alias Msg =
  Timer.Msg


-- VIEW

getHtml : Time.Posix -> SimpleTimer -> Html Msg
getHtml cur_time timer =
  Card.group
    [ Card.config [ Card.secondary ]
      |> Card.block []
          [ Block.custom <|
              div []
                [ if Timer.isRunning timer then
                    button [ onClick Timer.Stop ] [ text "Stop" ]
                  else
                      button [ onClick Timer.Start ] [ text "Start" ]
                ]
          ]
    , Card.config [ Card.light ]
      |> Card.block [] [ Block.titleH4 [] [ text timer.name ] ]
    , Card.config [ Card.light ]
      |> Card.block [] [ Block.text [] [ text <| Timer.displayTime cur_time timer ] ]
    , Card.config [ Card.secondary ]
      |> Card.block []
          [ Block.custom <|
              div [] [ button [ onClick Timer.Reset ] [ text "Reset" ] ]
          ]
    ]

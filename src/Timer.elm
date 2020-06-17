module Timer exposing (..)


import Time
import Time.Extra as Time
import Task
import Html exposing (..)
import Html.Events exposing (..)
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block


-- MODEL

type alias Millis = Int

type alias Timer =
  { name  : String
  , state : TimerState
  }

type TimerState =
    Stopped Millis
  | Starting Millis
  | Running  Millis Time.Posix
  | Stopping Millis Time.Posix

updated_time : Time.Posix -> Time.Posix -> Millis -> Millis
updated_time cur_time start offset =
  offset + Time.diff Time.Millisecond Time.utc start cur_time

display_time : Time.Posix -> Timer -> String
display_time cur_time timer =
  millisToString <|
    case timer.state of
      Stopped interval ->
        interval
      Starting interval ->
        interval
      Running interval start ->
        updated_time cur_time start interval
      Stopping interval start ->
        updated_time cur_time start interval

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

-- UPDATE

type Msg =
    Start
  | Stop
  | Reset
  | ImmediateUpdate Time.Posix

update_state : Msg -> Timer -> (Timer, Cmd Msg)
update_state msg timer =
  case msg of
      Start ->
        case timer.state of
          Stopped interval ->
            ( { timer | state = Starting interval }
            , Task.perform ImmediateUpdate Time.now
            )

          _ ->
            ( timer, Cmd.none )

      Stop ->
        case timer.state of
          Running interval start ->
            ( { timer | state = Stopping interval start }
            , Task.perform ImmediateUpdate Time.now
            )

          _ ->
            ( timer, Cmd.none )

      Reset ->
        ( { timer | state = Stopped 0 }, Cmd.none )

      ImmediateUpdate cur_time ->
        time_set cur_time timer

time_set : Time.Posix -> Timer -> (Timer, Cmd Msg)
time_set cur_time timer =
    case timer.state of
      Starting interval ->
        ( { timer | state = Running interval cur_time }, Cmd.none )

      Stopping interval start ->
        ( { timer | state = Stopped (updated_time cur_time start interval) }
        , Cmd.none
        )

      _ ->
        ( timer, Cmd.none )


-- VIEW

get_timer_html : Time.Posix -> Timer -> Html Msg
get_timer_html cur_time timer =
  Card.group
    [ Card.config [ Card.secondary ]
      |> Card.block []
          [ Block.custom <|
              div []
                [ case timer.state of
                    Stopped _ ->
                      button [ onClick Start ] [ text "Start" ]
                    Starting _ ->
                      button [ onClick Start ] [ text "Start" ]
                    Running _ _ ->
                      button [ onClick Stop ] [ text "Stop" ]
                    Stopping _ _ ->
                      button [ onClick Stop ] [ text "Stop" ]
                ]
          ]
    , Card.config [ Card.light ]
      |> Card.block [] [ Block.titleH4 [] [ text timer.name ] ]
    , Card.config [ Card.light ]
      |> Card.block [] [ Block.text [] [ text <| display_time cur_time timer ] ]
    , Card.config [ Card.secondary ]
      |> Card.block []
          [ Block.custom <|
              div [] [ button [ onClick Reset ] [ text "Reset" ] ]
          ]
    ]

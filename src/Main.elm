module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Browser
import Time
import Time.Extra as Time
import Task
import Timer

main : Program () Model Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model =
  { now    : Time.Posix
  , timers : List Timer.Timer}

init : () -> (Model, Cmd Msg)
init flags =
  ( Model (Time.millisToPosix 0) [], Cmd.none )

-- UPDATE

type Msg
  = TimerCommand Int Timer.Msg
  | Tick Time.Posix


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    TimerCommand index timer_msg ->
      let start = List.take index model.timers
          end   = List.drop index model.timers
      in
        case end of
          [] ->
            (model, Cmd.none)

          x::xs ->
            let updated = Timer.update_state timer_msg x
            in
              ( (Model model.now (start ++ Tuple.first updated :: xs))
              , Cmd.map (TimerCommand index) (Tuple.second updated)
              )

    Tick cur_time ->
      let updated = List.map (Timer.time_set cur_time) model.timers
      in
        ( (Model cur_time (List.map Tuple.first updated))
        , Cmd.batch (List.indexedMap (\i c -> Cmd.map (TimerCommand i) (Tuple.second c)) updated)
        )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick


-- VIEW

-- get_timer : Model -> Html Msg
-- get_timer model =
--   div []
--     [ text (String.fromInt (display_time model))
--     , button [ onClick Start ] [ text "Start" ]
--     , button [ onClick Stop ] [text "Stop" ]
--     , button [ onClick Reset ] [text "Reset" ]
--     ]

get_html : Model -> Browser.Document Msg
get_html model =
  { title = "Elm Timecard"
  , body = [] -- get_timer model ]
  }

view : Model -> Browser.Document Msg
view model =
  get_html model

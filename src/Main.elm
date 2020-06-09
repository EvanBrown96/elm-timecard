module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
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
  { now          : Time.Posix
  , addTimerText : String
  , timers       : List Timer.Timer
  }

init : () -> (Model, Cmd Msg)
init flags =
  ( Model (Time.millisToPosix 0) "" [], Cmd.none )

-- UPDATE

type Msg
  = AddTimer
  | UpdateTimerText String
  | TimerCommand Int Timer.Msg
  | Tick Time.Posix


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddTimer ->
      ( Model model.now "" (model.timers ++ [ Timer.Timer model.addTimerText (Timer.Stopped 0) ])
      , Cmd.none
      )

    UpdateTimerText text ->
      ( { model | addTimerText = text }, Cmd.none )

    TimerCommand index timer_msg ->
      let start = List.take index model.timers
          end   = List.drop index model.timers
      in
        case end of
          [] ->
            (model, Cmd.none)

          x::xs ->
            let updated  = Timer.update_state timer_msg x
                cur_time =
                  case timer_msg of
                    Timer.ImmediateUpdate new_time ->
                      new_time
                    _ -> model.now
            in
              ( (Model cur_time model.addTimerText (start ++ Tuple.first updated :: xs))
              , Cmd.map (TimerCommand index) (Tuple.second updated)
              )

    Tick cur_time ->
      let updated = List.map (Timer.time_set cur_time) model.timers
      in
        ( (Model cur_time model.addTimerText (List.map Tuple.first updated))
        , Cmd.batch (List.indexedMap (\i c -> Cmd.map (TimerCommand i) (Tuple.second c)) updated)
        )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 50 Tick


-- VIEW

view : Model -> Browser.Document Msg
view model =
  { title = "Elm Timecard"
  , body =
    ( div [] [ input [ placeholder "Timer Name", value model.addTimerText, onInput UpdateTimerText ] [], button [ onClick AddTimer ] [ text "Add Timer" ]]) ::
    List.indexedMap (\i h -> Html.map (TimerCommand i) (Timer.get_timer_html model.now h)) model.timers
  }

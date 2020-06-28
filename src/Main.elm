module Main exposing (..)


import Browser
import Time
import Time.Extra as Time
import Task
import Timer exposing (Timer)
import SimpleTimer exposing (SimpleTimer)
import TimerGroup exposing (TimerGroup)
import GeneralizedTimer exposing (GeneralizedTimer)
import Bootstrap.Modal as Modal
import Element exposing (..)
import Element.Input as Input

main : Program () Model Msg
main =
  Browser.document
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model =
  { now             : Time.Posix
  , addModalVisible : Modal.Visibility
  , addTimerText    : String
  , timers          : List GeneralizedTimer
  }

init : () -> (Model, Cmd Msg)
init flags =
  ( Model (Time.millisToPosix 0) Modal.hidden "" []
  , Cmd.none
  )


-- UPDATE

type Msg =
    AddTimer
  | ShowAddModal
  | HideAddModal
  | UpdateTimerText String
  | TimerCommand Int GeneralizedTimer.Msg
  | Tick Time.Posix

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddTimer ->
      ( { model | addModalVisible = Modal.hidden
                , addTimerText    = ""
                , timers          = model.timers ++ [ GeneralizedTimer.SimpleTimer <| Timer.newTimer model.addTimerText ]
        }
      , Cmd.none
      )

    UpdateTimerText text ->
      ( { model | addTimerText = text }, Cmd.none )

    TimerCommand index timer_msg ->
      let
        cur_time =
          case timer_msg of
            Timer.Update new_time ->
              new_time
            _ ->
              model.now
      in
        Timer.forwardToIndex TimerCommand model.timers index timer_msg
        |> Tuple.mapFirst (\timers -> { model | now = cur_time, timers = timers })

    Tick cur_time ->
      let
        (updated_timers, cmds) = List.map (Timer.updateState (Timer.Update cur_time)) model.timers
                                 |> List.unzip
      in
        ( { model | now    = cur_time
                  , timers = updated_timers
          }
        , cmds
            |> List.indexedMap (\i -> Cmd.map (TimerCommand i))
            |> Cmd.batch
        )

    ShowAddModal ->
      ( { model | addModalVisible = Modal.shown }, Cmd.none )

    HideAddModal ->
      ( { model | addModalVisible = Modal.hidden
                , addTimerText = ""
        }
      , Cmd.none )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 50 Tick


-- VIEW

view : Model -> Browser.Document Msg
view model =
  { title = "Elm Timecard"
  , body = List.singleton <| layout [] <|
      column [ centerX, width (px 800) ] <| buttonsRow model :: List.indexedMap
        (\i t -> SimpleTimer.view model.now t |> Element.map (TimerCommand i)) model.timers
  }

buttonsRow : Model -> Element Msg
buttonsRow model =
  row []
    [ Input.text [] { onChange = UpdateTimerText
                    , text = model.addTimerText
                    , placeholder = Just <| Input.placeholder [] (text "Timer Name")
                    , label = Input.labelHidden "Timer Name"
                    }
    , Input.button [] { onPress = Just AddTimer
                      , label = text "Add"
                      }
    ]

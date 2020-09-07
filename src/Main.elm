module Main exposing (..)


import Browser
import Time
import Time.Extra as Time
import Task
import Timer exposing (Timer)
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
  , timers          : List Timer
  }

init : () -> (Model, Cmd Msg)
init flags =
  ( Model (Time.millisToPosix 0) Modal.hidden "" [ Timer.testInitial ]
  , Cmd.none
  )


-- UPDATE

type Msg =
    AddTimer
  | ShowAddModal
  | HideAddModal
  | UpdateTimerText String
  | TimerCommand Int Timer.Msg
  | Tick Time.Posix

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddTimer ->
      ( { model | addModalVisible = Modal.hidden
                , addTimerText    = ""
                , timers          = model.timers ++ [ Timer.newTimer model.addTimerText ]
        }
      , Cmd.none
      )

    UpdateTimerText text ->
      ( { model | addTimerText = text }, Cmd.none )

    TimerCommand index timer_msg ->
      let
        cur_time =
          case timer_msg of
            Timer.Update new_time _ ->
              new_time
            _ ->
              model.now
      in
        forwardToIndex timer_msg model.timers index
        |> Tuple.mapFirst (\timers -> { model | now = cur_time, timers = timers })

    Tick cur_time ->
      let
        (updated_timers, cmds) = List.map (Timer.updateState (Timer.fullUpdate cur_time)) model.timers
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

forwardToIndex : Timer.Msg -> List Timer -> Int -> ( List Timer, Cmd Msg )
forwardToIndex msg timers index =
  let
    start = List.take index timers
    end = List.drop index timers
  in
    case end of
      x::xs ->
        let
          ( updated, cmd ) = Timer.updateState msg x
        in
          ( start ++ updated :: xs
          , Cmd.map (TimerCommand index) cmd
          )
      _ ->
        ( timers, Cmd.none )


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
        (\i t -> Timer.view model.now t |> Element.map (TimerCommand i)) model.timers
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

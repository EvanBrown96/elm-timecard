module Main exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Browser
import Time
import Time.Extra as Time
import Task
import Timer
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Modal as Modal


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
  , timers          : List Timer.Timer
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
  | TimerCommand Int Timer.Msg
  | Tick Time.Posix

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddTimer ->
      ( { model | addModalVisible = Modal.hidden
                , addTimerText    = ""
                , timers          = model.timers ++ [ Timer.Timer model.addTimerText (Timer.Stopped 0) ]
        }
      , Cmd.none
      )

    UpdateTimerText text ->
      ( { model | addTimerText = text }, Cmd.none )

    TimerCommand index timer_msg ->
      let
        start = List.take index model.timers
        end   = List.drop index model.timers
      in
        case end of
          [] ->
            (model, Cmd.none)

          x::xs ->
            let
              updated  = Timer.update_state timer_msg x
              cur_time =
                case timer_msg of
                  Timer.ImmediateUpdate new_time ->
                    new_time
                  _ ->
                    model.now
            in
              ( { model | now    = cur_time
                        , timers = (start ++ Tuple.first updated :: xs)
                }
              , Cmd.map (TimerCommand index) (Tuple.second updated)
              )

    Tick cur_time ->
      let
        updated = List.map (Timer.time_set cur_time) model.timers
      in
        ( { model | now    = cur_time
                  , timers = (List.map Tuple.first updated)
          }
        , updated
            |> List.indexedMap (\i c -> Cmd.map (TimerCommand i) (Tuple.second c))
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
  , body =
      [ CDN.stylesheet
      , addModal model
      , Grid.container [] <|
          buttonsRow :: List.indexedMap (timerToRow model.now) model.timers
      ]
  }

addModal : Model -> Html Msg
addModal model =
  Modal.config HideAddModal
  |> Modal.hideOnBackdropClick True
  |> Modal.header [] [ text "Add New Timer" ]
  |> Modal.body [] [ input [ placeholder "Timer Name", value model.addTimerText, onInput UpdateTimerText ] [] ]
  |> Modal.footer [] [ button [ onClick AddTimer ] [ text "Add" ] ]
  |> Modal.view model.addModalVisible

buttonsRow : Html Msg
buttonsRow =
  Grid.row [ Row.attrs [ Spacing.m2 ] ]
    [ Grid.col []
        [ button [ onClick ShowAddModal ] [ text "Add Timer" ] ]
    ]

timerToRow : Time.Posix -> Int -> Timer.Timer -> Html Msg
timerToRow now index timer =
  Grid.row [ Row.attrs [ Spacing.m2 ] ]
    [ Grid.col []
        [ Html.map (TimerCommand index) (Timer.get_timer_html now timer) ]
    ]

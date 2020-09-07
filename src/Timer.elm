module Timer exposing (Millis, Timer, newTimer, displayTime, isRunning, Msg(..), updateState, view, fullStart, fullStop, fullUpdate, testInitial)


import Time
import Time.Extra as Time
import Task
import Set
import Helpers exposing (..)
import Element exposing (..)
import Element.Input as Input
import CommonElements exposing (..)



testInitial =
  Timer <| TimerSpec "Collins" (Stopped 0) <|
    Just <| GroupType True
      [ TimerSpec "Work Package" (Stopped 0) Nothing
      , TimerSpec "General Time" (Stopped 0) Nothing
      , TimerSpec "Training" (Stopped 0) Nothing
      , TimerSpec "Intern Work" (Stopped 0) Nothing
      ]



-- MODEL

type alias Millis = Int

type Timer =
  Timer TimerSpec

type alias TimerSpec =
  { name  : String
  , state : TimerState
  , groupType : Maybe GroupType
  }

type GroupType =
  GroupType Bool (List TimerSpec)

newTimer : String -> Timer
newTimer name =
  Timer <| TimerSpec name (Stopped 0) Nothing

type TimerState =
    Stopped Millis
  | Starting Millis
  | Running  Millis Time.Posix
  | Stopping Millis Time.Posix


-- UPDATE

type Msg =
    Start ChildIndex
  | Stop ChildIndex
  | Reset
  | Update Time.Posix ChildIndex
  | AddChild String ChildIndex
  | Remove Int ChildIndex

fullStart : Msg
fullStart =
  Start Final

fullStop : Msg
fullStop =
  Stop Final

fullUpdate : Time.Posix -> Msg
fullUpdate posixTime =
  Update posixTime Final

type ChildIndex =
    Final
  | Child Int ChildIndex

encloseMsg : Int -> Msg -> Msg
encloseMsg index msg =
  case msg of
    Start childIndex ->
      Start (Child index childIndex)

    Stop childIndex ->
      Stop (Child index childIndex)

    Reset ->
      Reset

    Update posixTime childIndex ->
      Update posixTime (Child index childIndex)

    AddChild name childIndex ->
      AddChild name (Child index childIndex)

    Remove removeIndex childIndex ->
      Remove removeIndex (Child index childIndex)

updateState : Msg -> Timer -> ( Timer, Cmd Msg )
updateState msg timer =
  case timer of
    Timer timerSpec ->
      updateStateBase msg timerSpec
      |> Tuple.mapFirst Timer

updateStateBase : Msg -> TimerSpec -> ( TimerSpec, Cmd Msg )
updateStateBase msg timerSpec =
  case timerSpec.groupType of
    Nothing ->
      simpleUpdateState msg timerSpec

    Just group ->
      groupUpdateState msg timerSpec group

simpleUpdateState : Msg -> TimerSpec -> ( TimerSpec, Cmd Msg )
simpleUpdateState msg timerSpec =
  let
    default = ( timerSpec, Cmd.none )
  in
    case msg of
      Start childIndex ->
        case childIndex of
          Final ->
            startTimerSpec timerSpec

          _ ->
            default

      Stop childIndex ->
        case childIndex of
          Final ->
            stopTimerSpec timerSpec

          _ ->
            default

      Reset ->
        resetTimerSpec timerSpec

      Update posixTime childIndex  ->
        case childIndex of
          Final ->
            updateTimerSpec posixTime timerSpec

          _ ->
            default

      AddChild name childIndex ->
        case childIndex of
          Final ->
            ( { timerSpec | groupType = Just <| GroupType False <| List.singleton <| TimerSpec name (Stopped 0) Nothing }
            , Cmd.none
            )

          _ ->
            default

      Remove _ _ ->
        default

groupUpdateState : Msg -> TimerSpec -> GroupType -> ( TimerSpec, Cmd Msg )
groupUpdateState msg timerSpec groupType =
  case groupType of
    GroupType exclusive children ->
      let
        default = ( ( timerSpec, Cmd.none ), ( children, [] ) )
        ( ( updatedMain, mainCmd ), ( updatedChildren, childCmds ) ) =
          case msg of
            Reset ->
              ( resetTimerSpec timerSpec
              , List.map resetTimerSpec children |> List.unzip
              )

            Start childIndex ->
              case childIndex of
                Final ->
                  default

                Child index nextChildIndex ->
                  let
                    ( tempChildren, tempCmds ) =
                      case exclusive of
                        False ->
                          forwardToIndex Start children ( index, nextChildIndex )
                        True ->
                          let
                            start = List.take index children
                            end   = List.drop index children
                          in
                            case end of
                              x::xs ->
                                List.map (updateStateBase fullStop) start
                                ++ updateStateBase (Start nextChildIndex) x
                                :: List.map (updateStateBase fullStop) xs
                                |> List.unzip
                                |> Tuple.mapSecond (List.map (Cmd.map (encloseMsg index)))

                              _ ->
                                ( children, [] )
                  in
                    if List.any (Timer >> isRunning) tempChildren
                    then ( startTimerSpec timerSpec, ( tempChildren, tempCmds ) )
                    else default

            Stop childIndex ->
              case childIndex of
                Final ->
                  ( stopTimerSpec timerSpec
                  , List.map (updateStateBase fullStop) children |> List.unzip
                  )

                Child index nextChildIndex ->
                  let
                    ( tempChildren, tempCmds ) = forwardToIndex Stop children ( index, nextChildIndex )
                  in
                    if not <| List.any (Timer >> isRunning) tempChildren
                    then ( stopTimerSpec timerSpec, ( tempChildren, tempCmds ) )
                    else default

            Update posixTime childIndex ->
              case childIndex of
                Final ->
                  ( updateTimerSpec posixTime timerSpec
                  , List.map (updateStateBase (fullUpdate posixTime)) children |> List.unzip
                  )

                Child index nextChildIndex ->
                  ( ( timerSpec, Cmd.none )
                  , forwardToIndex (Update posixTime) children ( index, nextChildIndex )
                  )

            AddChild name childIndex ->
              case childIndex of
                Final ->
                  ( ( timerSpec, Cmd.none )
                  , ( children ++ List.singleton (TimerSpec name (Stopped 0) Nothing), [] )
                  )

                Child index nextChildIndex ->
                  ( ( timerSpec, Cmd.none)
                  , forwardToIndex (AddChild name) children ( index, nextChildIndex )
                  )

            Remove removeIndex childIndex ->
              case childIndex of
                Final ->
                  let
                    start = List.take removeIndex children
                    end   = List.drop (removeIndex + 1) children
                    tempChildren = start ++ end
                  in
                    ( if not <| List.any (Timer >> isRunning) tempChildren
                      then stopTimerSpec timerSpec else ( timerSpec, Cmd.none )
                    , ( tempChildren, [] )
                    )

                Child index nextChildIndex ->
                  let
                    ( tempChildren, tempCmds ) = forwardToIndex (Remove removeIndex) children ( index, nextChildIndex )
                  in
                    ( if not <| List.any (Timer >> isRunning) tempChildren
                      then stopTimerSpec timerSpec else (timerSpec, Cmd.none )
                    , ( tempChildren, tempCmds )
                    )

      in
        ( { updatedMain | groupType = Just <| GroupType exclusive updatedChildren }
        , Cmd.batch <| mainCmd :: childCmds
        )

forwardToIndex : (ChildIndex -> Msg) -> List TimerSpec -> ( Int, ChildIndex ) -> ( List TimerSpec, List (Cmd Msg) )
forwardToIndex msgConstr timers ( index, nextChildIndex ) =
  let
    start = List.take index timers
    end = List.drop index timers
  in
    case end of
      x::xs ->
        let
          ( updated, cmd ) = updateStateBase (msgConstr nextChildIndex) x
        in
          ( start ++ updated :: xs
          , Cmd.map (encloseMsg index) cmd |> List.singleton
          )
      _ ->
        ( timers, [] )

updateTimerSpec : Time.Posix -> TimerSpec -> ( TimerSpec, Cmd Msg )
updateTimerSpec curTime timerSpec =
  case timerSpec.state of
    Starting interval ->
      ( { timerSpec | state = Running interval curTime }
      , Cmd.none
      )

    Stopping interval start ->
      ( { timerSpec | state = Stopped (updatedTime curTime start interval) }
      , Cmd.none
      )

    _ ->
      ( timerSpec, Cmd.none )

startTimerSpec : TimerSpec -> ( TimerSpec, Cmd Msg )
startTimerSpec timerSpec =
  case timerSpec.state of
    Stopped interval ->
      ( { timerSpec | state = Starting interval }
      , Task.perform (\t -> Update t Final) Time.now
      )

    _ ->
      ( timerSpec, Cmd.none )

stopTimerSpec : TimerSpec -> ( TimerSpec, Cmd Msg )
stopTimerSpec timerSpec =
  case timerSpec.state of
    Running interval start ->
      ( { timerSpec | state = Stopping interval start }
      , Task.perform (\t -> Update t Final) Time.now
      )

    _ ->
      ( timerSpec, Cmd.none )

resetTimerSpec : TimerSpec -> ( TimerSpec, Cmd Msg )
resetTimerSpec timerSpec =
    ( { timerSpec | state = Stopped 0 }, Cmd.none )

-- addChildToTimerSpec : String -> TimerSpec ->  (TimerSpec, Cmd Msg )
-- addChildToTimerSpec string timerSpec =



-- HELPERS

isRunning : Timer -> Bool
isRunning timer =
  case timer of
    Timer timerSpec ->
      case timerSpec.state of
        Stopped _ ->
          False
        Stopping _ _ ->
          False
        _ ->
          True

updatedTime : Time.Posix -> Time.Posix -> Millis -> Millis
updatedTime cur_time start offset =
  offset + Time.diff Time.Millisecond Time.utc start cur_time

displayTime : Time.Posix -> Timer -> String
displayTime cur_time timer =
  millisToString <|
    case timer of
      Timer timerSpec ->
        case timerSpec.state of
          Stopped interval ->
            interval
          Starting interval ->
            interval
          Running interval start ->
            updatedTime cur_time start interval
          Stopping interval start ->
            updatedTime cur_time start interval

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


-- VIEW

view : Time.Posix -> Timer -> Element Msg
view =
  viewGeneral Final

viewGeneral : ChildIndex -> Time.Posix -> Timer -> Element Msg
viewGeneral childIndex posixTime timer =
  case timer of
    Timer timerSpec ->
      let
        stopButtonRec = { onPress = Just <| Stop childIndex, label = text "Stop" }
        startButtonRec = { onPress = Just <| Start childIndex, label = text "Start" }
        nameText = el [ centerY ] <| text timerSpec.name
        timeText = el [ centerY, alignRight ] <| text <| displayTime posixTime timer
        resetButton =
          case childIndex of
            Final ->
              Input.button [ centerX, centerY ]
                { onPress = Just Reset, label = text "Reset" }
            _ ->
              none
      in
        case timerSpec.groupType of
          Nothing ->
            timerSegment
              (Input.button [ centerX, centerY ] <|
                if isRunning timer then stopButtonRec else startButtonRec)
              nameText
              timeText
              resetButton

          Just (GroupType _ children) ->
            column [ width fill ]
              (timerSegment
                (if isRunning timer then
                  Input.button [ centerX, centerY ] stopButtonRec
                else none)
                nameText
                timeText
                resetButton
              :: List.indexedMap (\i t -> row [ width fill ] [ CommonElements.offsetBox, viewGeneral (Child i childIndex) posixTime (Timer t) ]) children)

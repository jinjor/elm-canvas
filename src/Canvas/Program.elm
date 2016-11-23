module Canvas.Program exposing (..)

import Task
import Html exposing (Html)

import Canvas.Basics exposing (..)
import Canvas.Element exposing (..)
import Canvas.Ports as Ports exposing (..)


type alias Model model msg =
  { userModel : model
  , element : Element msg
  , zSorted : List (AbsolutePositionWithElementChain msg)
  }


type Msg msg
  = NoOp
  | UserMsg msg
  | MouseDown OriginalMouseEvent
  | MouseUp OriginalMouseEvent
  | Click OriginalMouseEvent
  | DoubleClick OriginalMouseEvent



type alias Config model msg =
  { init : (model, Cmd msg)
  , update : msg -> model -> (model, Cmd msg)
  , view : model -> Html msg
  , canvasId : String
  , canvasView : model -> Element msg
  , subscriptions : model -> Sub msg
  }


init
   : Ports (Msg msg)
  -> Config model msg
  -> (Model model msg, Cmd (Msg msg))
init ports config =
  let
    (initUserModel, initUserCmd) =
      config.init

    element =
      config.canvasView initUserModel

    formattedNodes =
      formatNode initialContext element

    output =
      Ports.toOutput config.canvasId formattedNodes
  in
    { userModel = initUserModel
    , element = element
    , zSorted = sortByZIndex element
    }
    ! [ initUserCmd |> Cmd.map UserMsg
      , ports.output output |> Cmd.map (always NoOp)
      ]


update
   : Ports (Msg msg)
  -> Config model msg
  -> Msg msg
  -> Model model msg
  -> (Model model msg, Cmd (Msg msg))
update ports config msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)

    UserMsg msg ->
      let
        (userModel, userCmd) =
          config.update msg model.userModel

        element =
          config.canvasView userModel

        formattedNodes =
          formatNode initialContext element

        output =
          Ports.toOutput config.canvasId formattedNodes
      in
        { model
          | userModel = userModel
          , element = element
          , zSorted = sortByZIndex element
        }
        ! [ userCmd |> Cmd.map UserMsg
          , ports.output output |> Cmd.map (always NoOp)
          ]

    MouseDown e ->
      ( model
      , collectMouseDownEvents e model.zSorted e.offset |> toUserMsgCmd
      )

    MouseUp e ->
      ( model
      , collectMouseUpEvents e model.zSorted e.offset |> toUserMsgCmd
      )

    Click e ->
      ( model
      , collectClickEvents e model.zSorted e.offset |> toUserMsgCmd
      )

    DoubleClick e ->
      ( model
      , collectDoubleClickEvents e model.zSorted e.offset |> toUserMsgCmd
      )


toUserMsgCmd : List msg -> Cmd (Msg msg)
toUserMsgCmd messages =
  messages
    |> List.map (Task.succeed >> Task.perform UserMsg)
    |> Cmd.batch


subscriptions : Ports (Msg msg) -> Sub (Msg msg)
subscriptions ports =
  ports.input <|
    Ports.decodeEvent
      { mousedown = MouseDown
      , mouseup = MouseUp
      , click = Click
      , dblclick = DoubleClick
      }

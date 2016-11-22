module Canvas.Program exposing (..)


import Color exposing (Color)
import Task
import Mouse
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Html exposing (Html)

import Canvas.Model exposing (..)
import Canvas.Position as Position exposing (..)



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


type alias Ports msg =
  { output : Output -> Cmd Output
  , input : (Input -> Msg msg) -> Sub (Msg msg)
  }


type alias Input =
  { type_ : String
  , data : E.Value
  }


type alias Output =
  { canvasId : String
  , elements : List E.Value
  }


type alias Config model msg =
  { init : (model, Cmd msg)
  , update : msg -> model -> (model, Cmd msg)
  , view : model -> Html msg
  , canvasId : String
  , canvasView : model -> Element msg
  , subscriptions : model -> Sub msg
  }


init
   : Ports msg
  -> Config model msg
  -> (Model model msg, Cmd (Msg msg))
init ports config =
  let
    (initUserModel, initUserCmd) =
      config.init

    element =
      config.canvasView initUserModel

    output =
      toOutput config.canvasId element
  in
    { userModel = initUserModel
    , element = element
    , zSorted = sortByZIndex element
    }
    ! [ initUserCmd |> Cmd.map UserMsg
      , ports.output output |> Cmd.map (always NoOp)
      ]


update
   : Ports msg
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

        output =
          toOutput config.canvasId element
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


subscriptions : Ports msg -> Sub (Msg msg)
subscriptions ports =
  ports.input fromInput


fromInput : Input -> Msg msg
fromInput input =
  if input.type_ == "mousedown" then
    case D.decodeValue mouseEvent input.data of
      Ok e -> MouseDown e
      _ -> Debug.crash "cannot decode"
  else if input.type_ == "mouseup" then
    case D.decodeValue mouseEvent input.data of
      Ok e -> MouseUp e
      _ -> Debug.crash "cannot decode"
  else if input.type_ == "click" then
    case D.decodeValue mouseEvent input.data of
      Ok e -> Click e
      _ -> Debug.crash "cannot decode"
  else if input.type_ == "dblclick" then
    case D.decodeValue mouseEvent input.data of
      Ok e -> DoubleClick e
      _ -> Debug.crash "cannot decode"
  else
    Debug.crash ("undefined input type: " ++ input.type_)



type alias OriginalMouseEvent =
  { page : Position
  , offset : Position
  }


mouseEvent : Decoder OriginalMouseEvent
mouseEvent =
  D.map4
    (\offsetX offsetY pageX pageY ->
      { offset = { x = offsetX, y = offsetY }
      , page = { x = pageX, y = pageY }
      }
    )
    ( D.field "offsetX" D.int )
    ( D.field "offsetY" D.int )
    ( D.field "pageX" D.int )
    ( D.field "pageY" D.int )


toOutput : String -> Element msg -> Output
toOutput canvasId element =
  { canvasId = canvasId
  , elements = toOutputHelp initialContext element
  }


type alias Context =
  { position : Position
  }


initialContext : Context
initialContext =
  { position = Position.zero
  }


toOutputHelp : Context -> Element msg -> List E.Value
toOutputHelp context element =
  case element of
    Element options children ->
      let
        position =
          positionFrom context.position options

        size =
          sizeOf options

        newContext =
          { context | position = position <+> Maybe.withDefault Position.zero options.padding }

        value =
          E.object
            [ ( "type", E.string "element" )
            , ( "position", E.object [ ("x", E.int position.x), ("y", E.int position.y) ] )
            , ( "size", E.object [ ("width", E.int size.width), ("height", E.int size.height) ] )
            , ( "color", E.string (options.color |> Maybe.map fromColor |> Maybe.withDefault "#000") )
            , ( "border", encodeBorder options.border )
            , ( "shadow", encodeShadow options.shadow )
            , ( "backgroundColor", E.string (options.backgroundColor |> Maybe.map fromColor |> Maybe.withDefault "transparent") )
            ]
      in
        value :: List.concatMap (toOutputHelp newContext) children

    Text content ->
      let
        position =
          context.position

        value =
          E.object
            [ ( "type", E.string "text" )
            , ( "position", E.object [ ("x", E.int position.x), ("y", E.int position.y) ] )
            , ( "color", E.string "#000" )
            , ( "content", E.string content )
            ]
      in
        [ value ]


encodeBorder : Maybe Border -> E.Value
encodeBorder border =
  border
    |> Maybe.map (\border -> E.object [ ("width", E.int border.width), ("color", E.string (fromColor border.color)) ])
    |> Maybe.withDefault E.null


encodeShadow : Maybe Shadow -> E.Value
encodeShadow shadow =
  shadow
    |> Maybe.map (\shadow ->
        E.object
          [ ("blur", E.int shadow.blur)
          , ("offsetX", E.int shadow.offsetX)
          , ("offsetX", E.int shadow.offsetX)
          , ("color", E.string (fromColor shadow.color))
          ]
      )
    |> Maybe.withDefault E.null


positionFrom : Position -> Options msg -> Position
positionFrom from options =
  from <+> (options.position |> Maybe.withDefault Position.zero)


sizeOf : Options msg -> Size
sizeOf options =
  options.size |> Maybe.withDefault { width = 0, height = 0 }


collectMouseDownEvents : OriginalMouseEvent -> List (AbsolutePositionWithElementChain msg) -> Position -> List msg
collectMouseDownEvents =
  collectMouseEvents (\e ->
    case e of
      MouseDownE toMsg ->
        Just toMsg

      _ ->
        Nothing
  )


collectMouseUpEvents : OriginalMouseEvent -> List (AbsolutePositionWithElementChain msg) -> Position -> List msg
collectMouseUpEvents =
  collectMouseEvents (\e ->
    case e of
      MouseUpE toMsg ->
        Just toMsg

      _ ->
        Nothing
  )


collectClickEvents : OriginalMouseEvent -> List (AbsolutePositionWithElementChain msg) -> Position -> List msg
collectClickEvents =
  collectMouseEvents (\e ->
    case e of
      ClickE toMsg ->
        Just toMsg

      _ ->
        Nothing
  )


collectDoubleClickEvents : OriginalMouseEvent -> List (AbsolutePositionWithElementChain msg) -> Position -> List msg
collectDoubleClickEvents =
  collectMouseEvents (\e ->
    case e of
      DoubleClickE toMsg ->
        Just toMsg

      _ ->
        Nothing
  )


collectMouseEvents : (Event msg -> Maybe (MouseEvent -> msg)) -> OriginalMouseEvent -> List (AbsolutePositionWithElementChain msg) -> Position -> List msg
collectMouseEvents f originalEvent =
  collectEvents (\offset e ->
    f e
      |> Maybe.map
        (\toMsg ->
          toMsg
            { page = originalEvent.page
            , canvas = originalEvent.offset
            , offset = offset
            }
        )
  )


collectEvents : (Position -> Event msg -> Maybe msg) -> List (AbsolutePositionWithElementChain msg) -> Position -> List msg
collectEvents toMsg zSorted position =
  findMouseEventTarget (toMsg Position.zero >> (/=) Nothing) position zSorted
    |> Maybe.map (\(target, parents) -> collectEventsHelp toMsg position target parents)
    |> Maybe.withDefault []


collectEventsHelp : (Position -> Event msg -> Maybe msg) -> Position -> AbsolutePositionWithElement msg -> List (AbsolutePositionWithElement msg) -> List msg
collectEventsHelp toMsg position (absPosition, element) parents =
  case element of
    Element options _ ->
      let
        targetEvents =
          options.events
            |> List.filterMap (toMsg (position <-> absPosition))
      in
        case parents of
          [] ->
            targetEvents

          x :: xs ->
            targetEvents ++ collectEventsHelp toMsg position x xs

    _ ->
      Debug.crash "text cannot be a target or its parents"


findMouseEventTarget
   : (Event msg -> Bool)
  -> Position
  -> List (AbsolutePositionWithElementChain msg)
  -> Maybe (AbsolutePositionWithElementChain msg)
findMouseEventTarget isTarget position chainList =
  chainList
    |> find (\((absPosition, element), _) ->
      case element of
        Element options _ ->
          let
            size =
              sizeOf options
          in
            position.x >= absPosition.x &&
            position.x <= absPosition.x + size.width &&
            position.y >= absPosition.y &&
            position.y <= absPosition.y + size.height

        _ ->
          False
      )


find : (a -> Bool) -> List a -> Maybe a
find ok list =
  case list of
    [] -> Nothing

    x :: xs ->
      if ok x then Just x else find ok xs


type alias AbsolutePositionWithElementChain msg
  = (AbsolutePositionWithElement msg, List (AbsolutePositionWithElement msg))


type alias AbsolutePositionWithElement msg =
  (Position, Element msg)


sortByZIndex : Element msg -> List (AbsolutePositionWithElementChain msg)
sortByZIndex element =
  sortByZIndexHelp Position.zero [] element []


sortByZIndexHelp
   : Position
  -> List (AbsolutePositionWithElement msg)
  -> Element msg
  -> List (AbsolutePositionWithElementChain msg)
  -> List (AbsolutePositionWithElementChain msg)
sortByZIndexHelp from parents element prev =
  case element of
    Element options children ->
      let
        absPosition =
          positionFrom from options

        paddedAbsPosition =
          absPosition <+> Maybe.withDefault Position.zero options.padding
      in
        List.foldr
          (sortByZIndexHelp paddedAbsPosition ((absPosition, element) :: parents))
          prev
          children
        ++ [ ((absPosition, element), parents) ]

    _ ->
      prev


fromColor : Color -> String
fromColor color =
  let
    rgb =
      Color.toRgb color
  in
    "rgba(" ++ toString rgb.red ++
        "," ++ toString rgb.green ++
        "," ++ toString rgb.blue ++
        "," ++ toString rgb.alpha ++
        ")"

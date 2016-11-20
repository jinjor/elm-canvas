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
  }



type Msg msg
  = NoOp
  | UserMsg msg
  | MouseClick Position



type alias Ports msg =
  { output : Output -> Cmd Output
  , input : (Input -> msg) -> Sub msg
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
        }
        ! [ userCmd |> Cmd.map UserMsg
          , ports.output output |> Cmd.map (always NoOp)
          ]

    MouseClick page ->
      let
        events =
          handleClickEvents model.element page

        cmd =
          events
            |> List.map (Task.succeed >> Task.perform UserMsg)
            |> Cmd.batch
      in
        ( model, cmd )


subscriptions : Sub (Msg msg)
subscriptions =
  Sub.batch
    [ Mouse.clicks MouseClick
    ]


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


handleClickEvents : Element msg -> Position -> List msg
handleClickEvents element position =
  let
    sorted =
      sortByZIndex element
  in
    findMouseEventTarget position sorted
      |> Maybe.map (\(_, element, parents) ->
          collectEvents element parents
        )
      |> Maybe.withDefault []


collectEvents : Element msg -> List (Element msg) -> List msg
collectEvents element parents =
  case element of
    Element options _ ->
      let
        targetEvents =
          options.events
            |> List.map (\(Click msg) -> msg)
      in
        case parents of
          [] ->
            targetEvents

          x :: xs ->
            targetEvents ++ collectEvents x xs

    _ ->
      Debug.crash "text cannot be a target or its parents"


findMouseEventTarget
   : Position
  -> List (Position, Element msg, List (Element msg))
  -> Maybe (Position, Element msg, List (Element msg))
findMouseEventTarget position elements =
  elements
    |> find (\(absPosition, e, parents) ->
      case e of
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


sortByZIndex : Element msg -> List (Position, Element msg, List (Element msg))
sortByZIndex element =
  sortByZIndexHelp Position.zero [] element []


sortByZIndexHelp
   : Position
  -> List (Element msg)
  -> Element msg
  -> List (Position, Element msg, List (Element msg))
  -> List (Position, Element msg, List (Element msg))
sortByZIndexHelp from parents element prev =
  case element of
    Element options children ->
      let
        absPosition =
          positionFrom from options

        paddedAbsPosition =
          absPosition <+> Maybe.withDefault { x = 0, y = 0 } options.padding
      in
        List.foldr
          (sortByZIndexHelp paddedAbsPosition (element :: parents))
          prev
          children
        ++ [ (absPosition, element, parents) ]

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

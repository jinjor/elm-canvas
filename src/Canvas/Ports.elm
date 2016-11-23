module Canvas.Ports exposing (..)

import Color exposing (Color)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E

import Canvas.Basics exposing (..)


type alias Ports msg =
  { output : Output -> Cmd Output
  , input : (Input -> msg) -> Sub msg
  }


type alias Input =
  { type_ : String
  , data : Json
  }


decodeEvent :
  { mousedown : OriginalMouseEvent -> a
  , mouseup : OriginalMouseEvent -> a
  , click : OriginalMouseEvent -> a
  , dblclick : OriginalMouseEvent -> a
  } -> Input -> a
decodeEvent options input =
  if input.type_ == "mousedown" then
    case D.decodeValue decodeMouseEvent input.data of
      Ok e -> options.mousedown e
      _ -> Debug.crash "cannot decode"
  else if input.type_ == "mouseup" then
    case D.decodeValue decodeMouseEvent input.data of
      Ok e -> options.mouseup e
      _ -> Debug.crash "cannot decode"
  else if input.type_ == "click" then
    case D.decodeValue decodeMouseEvent input.data of
      Ok e -> options.click e
      _ -> Debug.crash "cannot decode"
  else if input.type_ == "dblclick" then
    case D.decodeValue decodeMouseEvent input.data of
      Ok e -> options.dblclick e
      _ -> Debug.crash "cannot decode"
  else
    Debug.crash ("undefined input type: " ++ input.type_)



decodeMouseEvent : Decoder OriginalMouseEvent
decodeMouseEvent =
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


---


type alias Output =
  { canvasId : String
  , elements : List Json
  }


toOutput : String -> List FormattedNode -> Output
toOutput canvasId elements =
  { canvasId = canvasId
  , elements = List.map encodeNode elements
  }


encodeNode : FormattedNode -> Json
encodeNode node =
  case node of
    ElementF position size border shadow backgroundColor ->
      encodeElement position size border shadow backgroundColor

    TextF position color fontWeight fontFamily fontSize content ->
      encodeText position color fontWeight fontFamily fontSize content


encodeElement : Position -> Size -> Maybe Border -> Maybe Shadow -> Color -> Json
encodeElement position size border shadow backgroundColor =
  E.object
    [ ( "type", E.string "element" )
    , ( "position", encodePosition position )
    , ( "size", encodeSize size )
    , ( "border", encodeBorder border )
    , ( "shadow", encodeShadow shadow )
    , ( "backgroundColor", encodeColor backgroundColor )
    ]


encodeText : Position -> Color -> String -> String -> Int -> String -> Json
encodeText position color fontWeight fontFamily fontSize content =
  E.object
    [ ( "type", E.string "text" )
    , ( "position", encodePosition position )
    , ( "color", encodeColor color )
    , ( "fontWeight", E.string fontWeight )
    , ( "fontFamily", E.string fontFamily )
    , ( "fontSize", E.int fontSize )
    , ( "content", E.string content )
    ]


encodeBorder : Maybe Border -> Json
encodeBorder border =
  border
    |> Maybe.map (\border -> E.object [ ("width", E.int border.width), ("color", encodeColor border.color) ])
    |> Maybe.withDefault E.null


encodeShadow : Maybe Shadow -> Json
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


encodeColor : Color -> Json
encodeColor color =
  E.string (fromColor color)


encodePosition : Position -> Json
encodePosition position =
  E.object [ ("x", E.int position.x), ("y", E.int position.y) ]


encodeSize : Size -> Json
encodeSize size =
  E.object [ ("width", E.int size.width), ("height", E.int size.height) ]

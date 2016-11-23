module Canvas exposing (..)

import Color exposing (Color)
import Html exposing (Html)


import Canvas.Element as Element exposing (..)
import Canvas.Properties as Properties exposing (..)
import Canvas.Event as Event exposing (..)
import Canvas.Program as Program exposing (..)
import Canvas.Ports as Ports


-- PROGRAM


type alias Ports msg
  = Ports.Ports (Msg msg)


type alias Input
  = Ports.Input


type alias Output
  = Ports.Output


type alias Msg msg
  = Program.Msg msg


type alias Model model msg
  = Program.Model model msg



program :
  Ports msg ->
  Config model msg ->
  Program Never (Model model msg) (Msg msg)
program ports options =
  let
    init =
      Program.init ports options

    update =
      Program.update ports options

    view model =
      options.view model.userModel |> Html.map UserMsg

    subscriptions model =
      Sub.batch
        [ options.subscriptions model.userModel |> Sub.map UserMsg
        , Program.subscriptions ports
        ]
  in
    Html.program
      { init = init
      , update = update
      , view = view
      , subscriptions = subscriptions
      }


-- ELEMENTS


type alias Element msg =
  Element.Element msg


text : String -> Element msg
text = Text


element : List (Attribute msg) -> List (Element msg) -> Element msg
element attrs children =
  Element (fromAttributes attrs) children



-- ATTRIBUTES


position : Int -> Int -> Attribute msg
position x y = PositionA { x = x, y = y }


size : Int -> Int -> Attribute msg
size width height =
  SizeA { width = width, height = height }


border : Int -> Color -> Attribute msg
border width color =
  BorderA { width = width, color = color }


shadow : Int -> Int -> Int -> Color -> Attribute msg
shadow blur offsetX offsetY color =
  ShadowA { blur = blur, offsetX = offsetX, offsetY = offsetY, color = color }


color : Color -> Attribute msg
color = ColorA


backgroundColor : Color -> Attribute msg
backgroundColor = BackgroundColorA


padding : Int -> Int -> Attribute msg
padding x y = PaddingA { x = x, y = y }



-- EVENTS


onMouseDown : (MouseEvent -> msg) -> Attribute msg
onMouseDown =
  EventA << MouseDownE


onMouseUp : (MouseEvent -> msg) -> Attribute msg
onMouseUp =
  EventA << MouseUpE


onClick : (MouseEvent -> msg) -> Attribute msg
onClick =
  EventA << ClickE


onDoubleClick : (MouseEvent -> msg) -> Attribute msg
onDoubleClick =
  EventA << DoubleClickE

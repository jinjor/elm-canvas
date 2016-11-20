module Canvas exposing (..)

import Color exposing (Color)
import Html exposing (Html)

import Canvas.Model as Model exposing (..)
import Canvas.Program as Program exposing (..)


-- PROGRAM


type alias Input
  = Program.Input


type alias Output
  = Program.Output


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
        , Program.subscriptions
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
  Model.Element msg


text : String -> Element msg
text = Text


element : List (Attribute msg) -> List (Element msg) -> Element msg
element attrs children =
  Element (makeOptions attrs) children



-- ATTRIBUTES


type alias Attribute msg =
  Model.Attribute msg


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


onClick : msg -> Attribute msg
onClick = EventA << Click

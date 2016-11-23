port module DragAndDrop exposing (..)

import Color
import Html exposing (Html)
import Html.Attributes as A
import Canvas exposing (..)
import Mouse exposing (Position)


port input : (Canvas.Input -> msg) -> Sub msg

port output : Canvas.Output -> Cmd msg


main : Program Never (Canvas.Model Model Msg) (Canvas.Msg Msg)
main =
  Canvas.program
    { input = input
    , output = output
    }
    { init = init
    , update = update
    , view = view
    , canvasId = "canvas"
    , canvasView = canvasView
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
    { position : Position
    , drag : Maybe Drag
    }


type alias Drag =
    { start : Position
    , current : Position
    }


init : ( Model, Cmd Msg )
init =
  ( Model (Position 200 200) Nothing, Cmd.none )



-- UPDATE


type Msg
    = DragStart Position
    | DragAt Position
    | DragEnd Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( updateHelp msg model, Cmd.none )


updateHelp : Msg -> Model -> Model
updateHelp msg ({position, drag} as model) =
  case msg of
    DragStart xy ->
      Model position (Just (Drag xy xy))

    DragAt xy ->
      Model position (Maybe.map (\{start} -> Drag start xy) drag)

    DragEnd _ ->
      Model (getPosition model) Nothing



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.drag of
    Nothing ->
      Sub.none

    Just _ ->
      Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]



-- VIEW


canvasView : Model -> Element Msg
canvasView model =
  let
    realPosition =
      getPosition model
  in
    element
      [ onMouseDown (.page >> DragStart)
      , backgroundColor (Color.rgb 60 141 47)
      , size 100 100
      , position realPosition.x realPosition.y
      , color Color.white
      , fontSize 16
      , fontFamily "'Source Sans Pro', 'Trebuchet MS', 'Lucida Grande', 'Bitstream Vera Sans', 'Helvetica Neue', sans-serif"
      ]
      [ text "Drag Me!" ]


getPosition : Model -> Position
getPosition {position, drag} =
  case drag of
    Nothing ->
      position

    Just {start,current} ->
      Position
        (position.x + current.x - start.x)
        (position.y + current.y - start.y)


view : Model -> Html Msg
view model =
  Html.canvas
    [ A.id "canvas"
    , A.width 800
    , A.height 600
    , A.style [("background-color", "#ddd"), ("margin-left", "50px"), ("margin-top", "50px")]
    ] []

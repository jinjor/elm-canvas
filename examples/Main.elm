port module Main exposing (..)

import Color
import Time
import Html exposing (Html)
import Html.Attributes as A
import Canvas exposing (..)


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


type alias Model =
  { count : Int
  }


init : (Model, Cmd Msg)
init =
  { count = 0 } ! []


type Msg =
  Increment | Decrement


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Increment ->
      { model | count = model.count + 1 } ! []

    Decrement ->
      { model | count = model.count - 1 } ! []



subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



canvasView : Model -> Element Msg
canvasView model =
  element
    [ size 660 460
    , position 80 80
    , backgroundColor Color.lightBrown
    ]
    [ element
        [ padding 5 5
        , size 30 30
        , position 325 130
        , backgroundColor Color.green
        , onClick Increment
        , onClick Increment
        ]
        [ text "Increment" ]
    , element
        [ padding 10 10
        , size 120 80
        , position 25 30
        , backgroundColor Color.red
        , onMouseDown Decrement
        ]
        [ text "Decrement"
        , element
            [ size 60 60
            , position 40 40
            , backgroundColor Color.lightPurple
            , border 3 (Color.rgb 30 50 230)
            , onClick Decrement
            ]
            []
        ]
    , element
        [ size 120 80
        , position (205 + model.count * 30) 300
        , backgroundColor Color.blue
        , shadow 10 5 5 (Color.rgb 70 0 0)
        ]
        []
    ]


view : Model -> Html Msg
view model =
  Html.canvas
    [ A.id "canvas"
    , A.width 800
    , A.height 600
    , A.style [("background-color", "#ddd"), ("margin-left", "50px"), ("margin-top", "50px")]
    ] []

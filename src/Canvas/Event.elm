module Canvas.Event exposing (..)

import Canvas.Basics exposing (..)


type alias MouseEvent =
  { page : Position
  , canvas : Position
  , offset : Position
  }


type Event msg
  = MouseDownE (MouseEvent -> msg)
  | MouseUpE (MouseEvent -> msg)
  | ClickE (MouseEvent -> msg)
  | DoubleClickE (MouseEvent -> msg)

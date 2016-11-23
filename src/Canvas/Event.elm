module Canvas.Event exposing (..)

import Canvas.Basics exposing (..)


type alias MouseEvent =
  { page : Position
  , canvas : Position
  , offset : Position
  }


type Event msg
  = MouseDownE (MouseEvent -> Bool) (MouseEvent -> msg)
  | MouseUpE (MouseEvent -> Bool) (MouseEvent -> msg)
  | ClickE (MouseEvent -> Bool) (MouseEvent -> msg)
  | DoubleClickE (MouseEvent -> Bool) (MouseEvent -> msg)



defaultPropagation : MouseEvent -> Bool
defaultPropagation =
  always False


setStopPropagation : (MouseEvent -> Bool) -> Event msg -> Event msg
setStopPropagation f e =
  case e of
    MouseDownE _ tagger -> MouseDownE f tagger
    MouseUpE _ tagger -> MouseUpE f tagger
    ClickE _ tagger -> ClickE f tagger
    DoubleClickE _ tagger -> DoubleClickE f tagger

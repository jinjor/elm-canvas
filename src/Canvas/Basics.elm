module Canvas.Basics exposing (..)


import Color exposing (Color)
import Json.Encode


type alias Json =
  Json.Encode.Value


type alias Position =
  { x : Int
  , y : Int
  }


(<+>) : Position -> Position -> Position
(<+>) a b =
  { x = a.x + b.x, y = a.y + b.y }


(<->) : Position -> Position -> Position
(<->) a b =
  { x = a.x - b.x, y = a.y - b.y }


zeroPosition : Position
zeroPosition =
  { x = 0, y = 0 }


type alias Size =
  { width : Int
  , height : Int
  }


transparent : Color
transparent =
  Color.rgba 0 0 0 0


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


find : (a -> Bool) -> List a -> Maybe a
find ok list =
  case list of
    [] -> Nothing

    x :: xs ->
      if ok x then Just x else find ok xs


----


type alias Border =
  { width : Int
  , color : Color
  }


type alias Shadow =
  { blur : Int
  , offsetX : Int
  , offsetY : Int
  , color : Color
  }


type alias OriginalMouseEvent =
  { page : Position
  , offset : Position
  }


type FormattedNode
  = ElementF Position Size (Maybe Border) (Maybe Shadow) Color
  | TextF Position Color String String Int String

module Canvas.Position exposing (..)


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


zero : Position
zero =
  { x = 0, y = 0 }

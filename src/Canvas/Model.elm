module Canvas.Model exposing (..)


import Color exposing (..)

import Canvas.Position as Position exposing (..)


type alias Size =
  { width : Int
  , height : Int
  }


type Element msg
  = Element (Options msg) (List (Element msg))
  | Text String


type Attribute msg
  = PositionA Position
  | SizeA Size
  | BorderA Color
  | ColorA Color
  | BackgroundColorA Color
  | PaddingA Position
  | EventA (Event msg)


type alias Options msg =
  { position : Maybe Position
  , size : Maybe Size
  , border : Maybe Color
  , color : Maybe Color
  , backgroundColor : Maybe Color
  , padding : Maybe Position
  , events : List (Event msg)
  }


type Event msg =
  Click msg



----


defaultOptions : Options msg
defaultOptions =
  { position = Nothing
  , size = Nothing
  , border = Nothing
  , color = Nothing
  , backgroundColor = Nothing
  , padding = Nothing
  , events = []
  }


makeOptions : List (Attribute msg) -> Options msg
makeOptions attrs =
  attrs
    |> List.foldl
      (\attr op ->
        case attr of
          PositionA posision ->
            { op | position = Just posision }

          SizeA size ->
            { op | size = Just size }

          BorderA border ->
            { op | border = Just border }

          ColorA color ->
            { op | color = Just color }

          BackgroundColorA backgroundColor ->
            { op | backgroundColor = Just backgroundColor }

          PaddingA padding ->
            { op | padding = Just padding }

          EventA event ->
            { op | events = event :: op.events } -- TODO dedup?
      )
      defaultOptions

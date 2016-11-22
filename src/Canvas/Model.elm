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
  | BorderA Border
  | ShadowA Shadow
  | ColorA Color
  | BackgroundColorA Color
  | PaddingA Position
  | EventA (Event msg)


type alias Options msg =
  { position : Maybe Position
  , size : Maybe Size
  , border : Maybe Border
  , shadow : Maybe Shadow
  , color : Maybe Color
  , backgroundColor : Maybe Color
  , padding : Maybe Position
  , events : List (Event msg)
  }


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


----


defaultOptions : Options msg
defaultOptions =
  { position = Nothing
  , size = Nothing
  , border = Nothing
  , shadow = Nothing
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

          ShadowA shadow ->
            { op | shadow = Just shadow }

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

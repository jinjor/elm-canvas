module Canvas.Properties exposing (..)

import Color exposing (..)

import Canvas.Basics as Basics exposing (..)
import Canvas.Event as Event exposing (..)


type alias Properties msg =
  { position : Maybe Position
  , size : Maybe Size
  , border : Maybe Border
  , shadow : Maybe Shadow
  , color : Maybe Color
  , backgroundColor : Maybe Color
  , padding : Maybe Position
  , events : List (Event msg)
  }


default : Properties msg
default =
  { position = Nothing
  , size = Nothing
  , border = Nothing
  , shadow = Nothing
  , color = Nothing
  , backgroundColor = Nothing
  , padding = Nothing
  , events = []
  }


type Attribute msg
  = PositionA Position
  | SizeA Size
  | BorderA Border
  | ShadowA Shadow
  | ColorA Color
  | BackgroundColorA Color
  | PaddingA Position
  | EventA (Event msg)


fromAttributes : List (Attribute msg) -> Properties msg
fromAttributes attrs =
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
            { op | events = event :: op.events }
      )
      default

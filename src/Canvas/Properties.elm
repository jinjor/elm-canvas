module Canvas.Properties exposing (..)

import Color exposing (..)

import Canvas.Basics as Basics exposing (..)
import Canvas.Event as Event exposing (..)


type alias Properties msg =
  { position : Maybe Position
  , size : Maybe Size
  , border : Maybe Border
  , shadow : Maybe Shadow
  , backgroundColor : Maybe Color
  , padding : Maybe Position
  , color : Maybe Color
  , fontWeight : Maybe String
  , fontFamily : Maybe String
  , fontSize : Maybe Int
  , events : List (Event msg)
  }


default : Properties msg
default =
  { position = Nothing
  , size = Nothing
  , border = Nothing
  , shadow = Nothing
  , backgroundColor = Nothing
  , padding = Nothing
  , color = Nothing
  , fontWeight = Nothing
  , fontFamily = Nothing
  , fontSize = Nothing
  , events = []
  }


type Attribute msg
  = PositionA Position
  | SizeA Size
  | BorderA Border
  | ShadowA Shadow
  | BackgroundColorA Color
  | PaddingA Position
  | ColorA Color
  | FontWeightA String
  | FontFamilyA String
  | FontSizeA Int
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

          BackgroundColorA backgroundColor ->
            { op | backgroundColor = Just backgroundColor }

          PaddingA padding ->
            { op | padding = Just padding }

          ColorA color ->
            { op | color = Just color }

          FontWeightA fontWeight ->
            { op | fontWeight = Just fontWeight }

          FontFamilyA fontFamily ->
            { op | fontFamily = Just fontFamily }

          FontSizeA fontSize ->
            { op | fontSize = Just fontSize }

          EventA event ->
            { op | events = event :: op.events }
      )
      default

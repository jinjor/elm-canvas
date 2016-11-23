module Canvas.Element exposing (..)

import Color exposing (..)

import Canvas.Basics as Basics exposing (..)
import Canvas.Event as Event exposing (..)
import Canvas.Properties as Properties exposing (..)


type Element msg
  = Element (Properties msg) (List (Element msg))
  | Text String


type alias AbsolutePositionWithElementChain msg
  = (AbsolutePositionWithElement msg, List (AbsolutePositionWithElement msg))


type alias AbsolutePositionWithElement msg =
  (Position, Element msg)


sortByZIndex : Element msg -> List (AbsolutePositionWithElementChain msg)
sortByZIndex element =
  sortByZIndexHelp zeroPosition [] element []


sortByZIndexHelp
   : Position
  -> List (AbsolutePositionWithElement msg)
  -> Element msg
  -> List (AbsolutePositionWithElementChain msg)
  -> List (AbsolutePositionWithElementChain msg)
sortByZIndexHelp from parents element prev =
  case element of
    Element properties children ->
      let
        absPosition =
          from <+> (properties.position |> Maybe.withDefault zeroPosition)

        paddedAbsPosition =
          absPosition <+> Maybe.withDefault zeroPosition properties.padding
      in
        List.foldr
          (sortByZIndexHelp paddedAbsPosition ((absPosition, element) :: parents))
          prev
          children
        ++ [ ((absPosition, element), parents) ]

    _ ->
      prev


type alias Context =
  { position : Position
  , color : Color
  }


initialContext : Context
initialContext =
  { position = zeroPosition
  , color = Color.black
  }


formatNode : Context -> Element msg -> List FormattedNode
formatNode context element =
  case element of
    Element properties children ->
      let
        position =
          context.position <+> (properties.position |> Maybe.withDefault zeroPosition)

        size =
          properties.size |> Maybe.withDefault { width = 0, height = 0 }

        backgroundColor =
          properties.backgroundColor |> Maybe.withDefault transparent

        padding =
          Maybe.withDefault zeroPosition properties.padding

        value =
          ElementF position size properties.border properties.shadow backgroundColor

        newContext =
          { context
            | position = position <+> padding
            , color = Maybe.withDefault context.color properties.color
          }
      in
        value :: List.concatMap (formatNode newContext) children

    Text content ->
      let
        position =
          context.position

        value =
          TextF position context.color content
      in
        [ value ]


collectMouseDownEvents
   : OriginalMouseEvent
  -> List (AbsolutePositionWithElementChain msg)
  -> Position
  -> List msg
collectMouseDownEvents =
  collectMouseEvents (\e ->
    case e of
      MouseDownE toMsg ->
        Just toMsg

      _ ->
        Nothing
  )


collectMouseUpEvents
   : OriginalMouseEvent
  -> List (AbsolutePositionWithElementChain msg)
  -> Position
  -> List msg
collectMouseUpEvents =
  collectMouseEvents (\e ->
    case e of
      MouseUpE toMsg ->
        Just toMsg

      _ ->
        Nothing
  )


collectClickEvents
   : OriginalMouseEvent
  -> List (AbsolutePositionWithElementChain msg)
  -> Position
  -> List msg
collectClickEvents =
  collectMouseEvents (\e ->
    case e of
      ClickE toMsg ->
        Just toMsg

      _ ->
        Nothing
  )


collectDoubleClickEvents
   : OriginalMouseEvent
  -> List (AbsolutePositionWithElementChain msg)
  -> Position
  -> List msg
collectDoubleClickEvents =
  collectMouseEvents (\e ->
    case e of
      DoubleClickE toMsg ->
        Just toMsg

      _ ->
        Nothing
  )


collectMouseEvents
   : (Event msg -> Maybe (MouseEvent -> msg))
  -> OriginalMouseEvent
  -> List (AbsolutePositionWithElementChain msg)
  -> Position
  -> List msg
collectMouseEvents f originalEvent zSorted position =
  let
    isTarget e =
      f e /= Nothing

    toMsg offset e =
      f e
        |> Maybe.map
          (\toMsg ->
            toMsg
              { page = originalEvent.page
              , canvas = originalEvent.offset
              , offset = offset
              }
          )
  in
    findMouseEventTarget isTarget position zSorted
      |> Maybe.map (\(target, parents) -> collectEventsHelp toMsg position target parents)
      |> Maybe.withDefault []


collectEventsHelp
   : (Position -> Event msg -> Maybe msg)
  -> Position
  -> AbsolutePositionWithElement msg
  -> List (AbsolutePositionWithElement msg)
  -> List msg
collectEventsHelp toMsg position (absPosition, element) parents =
  case element of
    Element properties _ ->
      let
        targetEvents =
          properties.events
            |> List.filterMap (toMsg (position <-> absPosition))
      in
        case parents of
          [] ->
            targetEvents

          x :: xs ->
            targetEvents ++ collectEventsHelp toMsg position x xs

    _ ->
      Debug.crash "text cannot be a target or its parents"


findMouseEventTarget
   : (Event msg -> Bool)
  -> Position
  -> List (AbsolutePositionWithElementChain msg)
  -> Maybe (AbsolutePositionWithElementChain msg)
findMouseEventTarget isTarget position chainList =
  chainList
    |> find (\((absPosition, element), _) ->
      case element of
        Element properties _ ->
          let
            size =
              properties.size |> Maybe.withDefault { width = 0, height = 0 }
          in
            position.x >= absPosition.x &&
            position.x <= absPosition.x + size.width &&
            position.y >= absPosition.y &&
            position.y <= absPosition.y + size.height

        _ ->
          False
      )

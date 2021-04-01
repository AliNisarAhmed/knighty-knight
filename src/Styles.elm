module Styles exposing (..)

import Element as E exposing (Attribute, Color, Element)
import Element.Background as B
import Element.Border as Border
import Element.Events as Ev
import Element.Font as F
import RankNFiles exposing (Move(..))


boardWidth : number
boardWidth =
    850


squareWidth : number
squareWidth =
    100


knightWidth : number
knightWidth =
    70


queenWidth : number
queenWidth =
    90


legalMoveCircleWidth : number
legalMoveCircleWidth =
    20


squareDarkColor : Color
squareDarkColor =
    E.rgb255 232 235 239


squareLight : Color
squareLight =
    E.rgb255 125 135 150


warnColor : Color
warnColor =
    E.rgba255 181 20 18 0.8


bgColor : Color
bgColor =
    E.rgb255 70 130 180


legalMoveCircleColor : Color
legalMoveCircleColor =
    E.rgba255 0 0 0 0.6



---- STYLES ----


debug =
    E.explain Debug.todo


layout : List (Attribute msg)
layout =
    [ E.centerX
    , E.centerY
    , B.color bgColor
    ]


square : Color -> List (Attribute msg)
square color =
    [ E.width <| E.px squareWidth
    , E.height <| E.px squareWidth
    , Border.width 1
    , Border.solid
    , B.color color
    , E.centerY
    , E.centerX
    , E.focused []
    ]


knight : List (Attribute msg)
knight =
    [ E.centerX
    , E.centerY
    , E.width <| E.px knightWidth
    , E.height <| E.px knightWidth
    , E.pointer
    ]


queen : List (Attribute msg)
queen =
    [ E.centerX
    , E.centerY
    , E.width <| E.px queenWidth
    , E.height <| E.px queenWidth
    ]


legalMoveCircle : List (Attribute msg)
legalMoveCircle =
    [ B.color legalMoveCircleColor
    , E.width <| E.px legalMoveCircleWidth
    , E.height <| E.px legalMoveCircleWidth
    , E.centerX
    , E.centerY
    , Border.rounded 10
    ]


legalMoveSquare : Element msg -> List (Attribute msg)
legalMoveSquare e =
    [ E.width E.fill, E.height E.fill, E.centerX, E.centerY, E.inFront e ]

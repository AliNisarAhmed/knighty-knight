module Styles exposing (..)

import Element as E exposing (Attribute, Color, Element)
import Element.Background as B
import Element.Border as Border
import Element.Font as Font
import RankNFiles exposing (Move(..))


boardWidth : number
boardWidth =
    850


squareWidth : number
squareWidth =
    90


knightWidth : number
knightWidth =
    50


selectedKnightWidth : number
selectedKnightWidth =
    60


queenWidth : number
queenWidth =
    80


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


legalMoveCircleColorLight : Color
legalMoveCircleColorLight =
    E.rgba255 0 0 0 0.23


targetColor : Color
targetColor =
    E.rgba 0 1 0 0.6


knightColor : Color
knightColor =
    E.rgb255 82 27 59



---- STYLES ----
-- debug =
-- E.explain Debug.todo


center : List (Attribute msg)
center =
    [ E.centerY, E.centerX ]


layout : List (Attribute msg)
layout =
    [ E.centerX
    , E.centerY
    , B.color bgColor
    , E.width <| E.fill
    , E.height E.fill
    ]


content : List (Attribute msg)
content =
    [ E.centerX
    , E.centerY
    , E.width <| (E.fill |> E.maximum 1400 |> E.minimum 400)
    , E.height E.fill
    , E.paddingXY 20 20
    ]


mainContent : List (Attribute msg)
mainContent =
    [ E.width <| E.fillPortion 3
    , E.height E.fill
    ]


heading : List (Attribute msg)
heading =
    [ Font.color knightColor
    , Font.size 60
    , Font.bold
    , E.width E.fill
    ]


boardColumn : List (Attribute msg)
boardColumn =
    [ E.width (E.fillPortion 1)
    , E.alignRight
    ]


square : Color -> List (Attribute msg)
square color =
    [ E.width <| E.px squareWidth
    , E.height <| E.px squareWidth
    , B.color color
    , E.centerY
    , E.centerX
    , E.focused []
    ]


targetSquare : List (Attribute msg)
targetSquare =
    [ E.width <| E.px squareWidth
    , E.height <| E.px squareWidth
    , B.color targetColor
    , E.centerY
    , E.centerX
    , E.focused []
    , B.color targetColor
    ]


knight : List (Attribute msg)
knight =
    [ E.centerX
    , E.centerY
    , E.width <| E.px knightWidth
    , E.height <| E.px knightWidth
    , E.pointer
    ]


selectedKnight : List (Attribute msg)
selectedKnight =
    [ E.centerX
    , E.centerY
    , E.width <| E.px selectedKnightWidth
    , E.height <| E.px selectedKnightWidth
    , E.pointer
    , Border.shadow
        { offset = ( 0, 0 )
        , size = 0.1
        , blur = 40
        , color = legalMoveCircleColor
        }
    , B.color legalMoveCircleColorLight
    , Border.rounded 20
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


rankLabelText : List (Attribute msg)
rankLabelText =
    [ Font.color <| E.rgb 1 1 1
    , E.centerX
    , E.centerY
    , E.paddingXY 5 0
    , E.width (E.px 20)
    ]


blankRankLabel : List (Attribute msg)
blankRankLabel =
    [ E.paddingXY 5 0
    , E.width (E.px 20)
    ]


fileLabelText : List (Attribute msg)
fileLabelText =
    [ Font.color <| E.rgb 1 1 1
    , E.centerY
    , E.centerX
    , E.paddingXY 0 7
    , E.width (E.px squareWidth)
    ]

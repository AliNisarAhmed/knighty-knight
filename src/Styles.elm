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


legalMoveCircleColorLight : Color
legalMoveCircleColorLight =
    E.rgba255 0 0 0 0.23


targetColor : Color
targetColor =
    E.rgba 0 1 0 0.6


knightColor : Color
knightColor =
    E.rgb255 82 27 59


knightLightGold : Color
knightLightGold =
    E.rgb255 255 207 112


knightShadow : Color
knightShadow =
    E.rgba255 0 0 0 0.6


queenColor : Color
queenColor =
    E.rgb255 128 77 118


white : Color
white =
    E.rgb 1 1 1


black : Color
black =
    E.rgb 0 0 0


red : Color
red =
    E.rgba255 150 11 11 0.8


underAttackRed : Color
underAttackRed =
    red


navyBlue : Color
navyBlue =
    E.rgb255 0 0 128


lightRed : Color
lightRed =
    E.rgba255 255 134 134 0.8



---- STYLES ----


debug =
    E.explain Debug.todo


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
    , Font.family
        [ Font.typeface "Hind Siluguri"
        , Font.sansSerif
        ]
    ]


content : List (Attribute msg)
content =
    [ E.centerX
    , E.centerY
    , E.width <| (E.fill |> E.maximum 1500 |> E.minimum 400)
    , E.height <| (E.px <| squareWidth * 8)
    , E.spaceEvenly
    ]


mainContent : List (Attribute msg)
mainContent =
    [ E.width <| E.fillPortion 3
    , E.height E.fill
    , E.paddingEach { top = 0, left = 40, right = 40, bottom = 30 }
    ]


stats =
    [ Border.width 2
    , Border.color navyBlue
    , E.centerY
    , E.centerX
    , B.color knightColor
    , Border.rounded 20
    , E.paddingXY 20 40
    ]


button : List (Attribute msg)
button =
    [ E.paddingXY 120 20
    , B.color <| E.rgb255 53 117 35
    , E.centerX
    , Border.rounded 20
    , Font.size 24
    , Font.letterSpacing 1.5
    , Border.shadow { offset = ( 0, 1 ), size = 1, blur = 2, color = knightShadow }
    ]


startButton : List (Attribute msg)
startButton =
    button
        ++ [ Font.color <| E.rgba 1 1 1 0.8
           , B.color <| E.rgb255 53 117 35
           , E.alignBottom
           , E.centerX
           ]


resetButton : List (Attribute msg)
resetButton =
    button
        ++ [ Font.color white
           , B.color red
           , Border.color red
           , Border.width 1
           , E.alignBottom
           ]


heading : List (Attribute msg)
heading =
    [ Font.color knightColor
    , Font.size 60
    , Font.bold
    , E.centerX
    , Font.letterSpacing 1.6
    ]


text : List (Attribute msg)
text =
    [ E.spacing 10
    , E.width <| (E.fill |> E.maximum 500)
    , E.centerY
    , E.centerX
    , E.paddingXY 0 20
    , Font.justify
    , Font.color (E.rgba 1 1 1 0.8)
    , B.color (E.rgba255 0 0 128 0.8)
    , E.paddingEach { top = 40, left = 30, bottom = 40, right = 30 }
    , Border.rounded 20
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
        , color = knightShadow
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
    [ B.color legalMoveCircleColorLight
    , E.width <| E.px legalMoveCircleWidth
    , E.height <| E.px legalMoveCircleWidth
    , E.centerX
    , E.centerY
    , Border.rounded 10
    ]


legalMoveSquare : Element msg -> List (Attribute msg)
legalMoveSquare e =
    [ E.width E.fill, E.height E.fill, E.centerX, E.centerY, E.inFront e ]


attackedByQueen : List (Attribute msg)
attackedByQueen =
    [ E.centerX
    , E.centerY
    , Font.color underAttackRed
    ]


x1 : List (Attribute msg)
x1 =
    [ E.rotate (degrees 45)
    , E.width (E.px legalMoveCircleWidth)
    , E.height (E.px 2)
    , B.color underAttackRed
    , E.inFront <| E.el x2 E.none
    ]


x2 : List (Attribute msg)
x2 =
    [ E.rotate (degrees 90)
    , E.width (E.px legalMoveCircleWidth)
    , E.height (E.px 2)
    , B.color underAttackRed
    ]


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


targetSquareName : List (Attribute msg)
targetSquareName =
    [ Font.size 100
    , Font.color white
    , E.centerX
    , E.centerY
    , E.paddingEach { top = 0, bottom = 20, left = 0, right = 0 }
    ]


knightStartingSquareText =
    [ Font.size 32
    , Font.color knightLightGold
    ]


targetSquareText =
    [ Font.size 32
    , Font.color targetColor
    ]


queenSquareText =
    [ Font.size 32
    , Font.color queenColor
    ]


totalMovesText =
    [ E.centerX
    , Font.color targetColor
    , Font.size 20
    , E.paddingXY 0 5
    ]


wrongMovesText =
    [ E.centerX
    , E.centerY
    , Font.color lightRed
    , Font.size 20
    , E.paddingXY 0 5
    ]


totalMovesNumber =
    [ Font.size 40
    , Font.color white
    , E.paddingXY 0 20
    , E.centerX
    ]


wrongMovesNumber =
    [ Font.size 40
    , Font.color white
    , E.paddingXY 0 20
    , E.centerX
    ]


timer =
    [ Font.size 60
    , Font.color white
    , E.centerX
    ]

module Styles exposing (..)

import Element as E exposing (Attribute, Color, Device, DeviceClass(..), Element)
import Element.Background as B
import Element.Border as Border
import Element.Font as Font
import RankNFiles exposing (File(..), Move(..), Rank(..), fileToInt, rankToInt)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P


animatedEl : Animation -> List (Attribute msg) -> Element msg -> Element msg
animatedEl =
    animatedUi E.el


animatedUi =
    Animated.ui { behindContent = E.behindContent, htmlAttribute = E.htmlAttribute, html = E.html }


animation : ( Float, Float ) -> ( Float, Float ) -> Animation
animation ( oldX, oldY ) ( newX, newY ) =
    Animation.steps
        { startAt = [ P.x oldX, P.y oldY, P.property "z-index" "2" ]
        , options = [ Animation.count 1 ]
        }
        [ Animation.step 170 <| [ P.x newX, P.y newY, P.property "z-index" "2" ] ]


knightPosition : File -> Rank -> ( Float, Float )
knightPosition file rank =
    ( fileToInt file * toFloat desktopSquareWidth + toFloat knightWidth / 2
    , (toFloat (8 - rankToInt rank) * toFloat desktopSquareWidth) + 65
    )


scales =
    E.modular 10 1.5


boardWidth : number
boardWidth =
    850


squareWidth : number
squareWidth =
    90


desktopSquareWidth : Int
desktopSquareWidth =
    round <| squareWidth / 1.25


tabletSquareWidth : Int
tabletSquareWidth =
    round <| squareWidth / 1.5


knightWidth : number
knightWidth =
    50


selectedKnightWidth : number
selectedKnightWidth =
    60


queenWidth : number
queenWidth =
    80


desktopQueenWidth : number
desktopQueenWidth =
    60


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


goldenRod : Color
goldenRod =
    E.rgb255 218 165 32



---- STYLES ----
-- debug =
--     E.explain Debug.todo


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


content : Device -> List (Attribute msg)
content { class, orientation } =
    case class of
        BigDesktop ->
            [ E.centerX
            , E.centerY
            , E.width <| (E.fill |> E.maximum 1500 |> E.minimum 400)
            , E.height <| (E.px <| squareWidth * 8)
            , E.spaceEvenly
            ]

        Desktop ->
            [ E.centerX
            , E.centerY
            , E.width <| (E.fill |> E.maximum 1500 |> E.minimum 1200)
            , E.height <| (E.px <| squareWidth * 8)
            ]

        _ ->
            [ E.centerX
            , E.centerY
            , E.width <| E.fill
            , E.height <| E.fill
            , E.spaceEvenly
            , E.padding 10
            ]


mainContent : Device -> List (Attribute msg)
mainContent { class } =
    case class of
        BigDesktop ->
            [ E.width <| E.fillPortion 1
            , E.height E.fill
            , E.paddingEach { top = 0, left = 40, right = 40, bottom = 30 }
            ]

        Desktop ->
            [ E.width <| E.fillPortion 1
            , E.height (E.px <| 8 * desktopSquareWidth)
            , E.paddingEach { top = 0, left = 20, right = 20, bottom = 0 }
            ]

        Tablet ->
            [ E.paddingEach { top = 0, left = 10, right = 10, bottom = 0 }
            , E.width <| (E.fill |> E.maximum 1000)
            , E.centerX
            , E.spaceEvenly
            ]

        _ ->
            [ E.paddingEach { top = 0, left = 10, right = 10, bottom = 0 }
            ]


stats : Device -> List (Attribute msg)
stats { class } =
    case class of
        BigDesktop ->
            [ Border.width 2
            , Border.color navyBlue
            , E.centerY
            , E.centerX
            , B.color knightColor
            , Border.rounded 20
            , E.paddingXY 20 40
            ]

        _ ->
            [ Border.width 2
            , Border.color navyBlue
            , E.centerY
            , E.centerX
            , B.color knightColor
            , Border.rounded 20
            , E.paddingXY 10 20
            ]


finishedStats =
    [ E.paddingEach { top = 40, left = 40, right = 40, bottom = 30 }
    , B.color goldenRod
    , E.centerY
    , E.centerX
    , Border.rounded 20
    , Font.color navyBlue
    ]


statList =
    [ E.paddingXY 30 0
    , E.spacingXY 0 20
    ]


button : Device -> List (Attribute msg)
button { class } =
    case class of
        BigDesktop ->
            [ E.paddingXY 120 20
            , B.color <| E.rgb255 53 117 35
            , E.centerX
            , Border.rounded 20
            , Font.size 24
            , Font.letterSpacing 1.5
            , Border.shadow { offset = ( 0, 1 ), size = 1, blur = 2, color = knightShadow }
            ]

        Desktop ->
            [ E.paddingXY 120 20
            , B.color <| E.rgb255 53 117 35
            , E.centerX
            , Border.rounded 20
            , Font.size 24
            , Font.letterSpacing 1.5
            , Border.shadow { offset = ( 0, 1 ), size = 1, blur = 2, color = knightShadow }
            ]

        _ ->
            [ E.paddingXY 60 20
            , B.color <| E.rgb255 53 117 35
            , E.centerX
            , Border.rounded 5
            , Font.size 18
            , Font.letterSpacing 1.5
            , Border.shadow { offset = ( 0, 1 ), size = 1, blur = 2, color = knightShadow }
            , E.height E.fill
            , E.alignRight
            ]


startButton : Device -> List (Attribute msg)
startButton device =
    button device
        ++ [ Font.color <| E.rgba 1 1 1 0.8
           , B.color <| E.rgb255 53 117 35
           , E.alignBottom
           , E.centerX
           ]


resetButton : Device -> List (Attribute msg)
resetButton device =
    button device
        ++ [ Font.color white
           , B.color red
           , Border.color red
           , Border.width 1
           , E.alignBottom
           ]


heading : Device -> List (Attribute msg)
heading { class } =
    let
        fontSize =
            case class of
                BigDesktop ->
                    60

                _ ->
                    40
    in
    [ Font.color knightColor
    , Font.size fontSize
    , Font.bold
    , E.centerX
    , Font.letterSpacing 1.6
    ]


text : Device -> List (Attribute msg)
text { class } =
    case class of
        BigDesktop ->
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

        Tablet ->
            [ E.spacing 10
            , E.width (E.fill |> E.maximum 600)
            , E.centerY
            , E.centerX
            , Font.justify
            , Font.size 14
            , Font.color (E.rgba 1 1 1 0.8)
            , B.color (E.rgba255 0 0 128 0.8)
            , E.paddingEach { top = 10, left = 10, bottom = 10, right = 10 }
            , Border.rounded 5
            ]

        _ ->
            [ E.spacing 10
            , E.width <| (E.fill |> E.maximum 500)
            , E.centerY
            , E.centerX
            , E.paddingXY 0 0
            , Font.justify
            , Font.size 18
            , Font.color (E.rgba 1 1 1 0.8)
            , B.color (E.rgba255 0 0 128 0.8)
            , E.paddingEach { top = 40, left = 30, bottom = 40, right = 30 }
            , Border.rounded 20
            ]


boardColumn : Device -> List (Attribute msg)
boardColumn { class } =
    case class of
        BigDesktop ->
            [ E.width (E.fillPortion 1)
            , E.alignRight
            ]

        Desktop ->
            [ E.width (E.fillPortion 1)
            , E.alignRight
            , E.paddingEach { top = 0, left = 0, right = 20, bottom = 0 }
            ]

        _ ->
            [ E.centerX
            , E.paddingEach { top = 20, left = 0, right = 20, bottom = 0 }
            ]


square : Color -> Device -> List (Attribute msg)
square color { class } =
    case class of
        BigDesktop ->
            [ E.width <| E.px squareWidth
            , E.height <| E.px squareWidth
            , B.color color
            , E.centerY
            , E.centerX
            , E.focused []
            ]

        Desktop ->
            [ E.width <| E.px <| desktopSquareWidth
            , E.height <| E.px <| desktopSquareWidth
            , B.color color
            , E.centerY
            , E.centerX
            , E.focused []
            ]

        Tablet ->
            [ E.width <| E.px <| tabletSquareWidth
            , E.height <| E.px <| tabletSquareWidth
            , B.color color
            , E.centerY
            , E.centerX
            , E.focused []
            ]

        _ ->
            [ E.width <| E.px <| round <| squareWidth / 1.25
            , E.height <| E.px <| round <| squareWidth / 1.25
            , B.color color
            , E.centerY
            , E.centerX
            , E.focused []
            ]


targetSquare : Device -> List (Attribute msg)
targetSquare { class } =
    case class of
        BigDesktop ->
            [ E.width <| E.px squareWidth
            , E.height <| E.px squareWidth
            , B.color targetColor
            , E.centerY
            , E.centerX
            , E.focused []
            , B.color targetColor
            ]

        Desktop ->
            [ E.width <| E.px desktopSquareWidth
            , E.height <| E.px desktopSquareWidth
            , B.color targetColor
            , E.centerY
            , E.centerX
            , E.focused []
            , B.color targetColor
            ]

        _ ->
            [ E.width <| E.px tabletSquareWidth
            , E.height <| E.px tabletSquareWidth
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


queen : Device -> List (Attribute msg)
queen { class } =
    case class of
        BigDesktop ->
            [ E.centerX
            , E.centerY
            , E.width <| E.px queenWidth
            , E.height <| E.px queenWidth
            ]

        _ ->
            [ E.centerX
            , E.centerY
            , E.width <| E.px desktopQueenWidth
            , E.height <| E.px desktopQueenWidth
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


fileLabelText : Device -> List (Attribute msg)
fileLabelText { class } =
    case class of
        BigDesktop ->
            [ Font.color <| E.rgb 1 1 1
            , E.centerY
            , E.centerX
            , E.paddingXY 0 7
            , E.width (E.px squareWidth)
            ]

        Desktop ->
            [ Font.color <| E.rgb 1 1 1
            , E.centerY
            , E.centerX
            , E.paddingXY 0 7
            , E.width (E.px desktopSquareWidth)
            ]

        _ ->
            [ Font.color <| E.rgb 1 1 1
            , E.centerY
            , E.centerX
            , E.paddingXY 0 7
            , E.width (E.px tabletSquareWidth)
            ]


targetSquareName : Device -> List (Attribute msg)
targetSquareName { class } =
    case class of
        BigDesktop ->
            [ Font.size 100
            , Font.color white
            , E.centerX
            , E.centerY
            , E.paddingEach { top = 0, bottom = 20, left = 0, right = 0 }
            ]

        _ ->
            [ Font.size 80
            , Font.color white
            , E.centerX
            , E.centerY
            , E.paddingEach { top = 20, bottom = 0, left = 0, right = 0 }
            ]


knightStartingSquareText : Device -> List (Attribute msg)
knightStartingSquareText { class } =
    case class of
        Tablet ->
            [ Font.size 24
            , Font.color knightLightGold
            ]

        _ ->
            [ Font.size 32
            , Font.color knightLightGold
            ]


targetSquareText : Device -> List (Attribute msg)
targetSquareText { class } =
    case class of
        Tablet ->
            [ Font.size 24
            , Font.color targetColor
            ]

        _ ->
            [ Font.size 32
            , Font.color targetColor
            ]


queenSquareText : Device -> List (Attribute msg)
queenSquareText { class } =
    case class of
        Tablet ->
            [ Font.size 24
            , Font.color queenColor
            ]

        _ ->
            [ Font.size 32
            , Font.color queenColor
            ]


totalMovesText =
    [ E.centerX
    , Font.color targetColor
    , Font.size 20
    , E.paddingEach { top = 5, left = 0, right = 0, bottom = 5 }
    ]


wrongMovesText =
    [ E.centerX
    , E.centerY
    , Font.color lightRed
    , Font.size 20
    , E.paddingEach { top = 5, left = 0, right = 0, bottom = 5 }
    ]


totalMovesNumber =
    [ Font.size 40
    , Font.color white
    , E.paddingEach { top = 40, left = 0, right = 0, bottom = 5 }
    , E.centerX
    ]


wrongMovesNumber =
    [ Font.size 40
    , Font.color white
    , E.paddingEach { top = 40, left = 0, right = 0, bottom = 5 }
    , E.centerX
    ]


timer : Device -> List (Attribute msg)
timer { class } =
    case class of
        BigDesktop ->
            [ Font.size 60
            , Font.color white
            , E.centerX
            ]

        _ ->
            [ Font.size 50
            , Font.color white
            , E.centerX
            ]


congrats =
    [ Font.size 30
    , Font.color navyBlue
    , E.paddingEach { top = 0, bottom = 30, left = 0, right = 0 }
    ]


took =
    [ E.paddingEach { top = 0, bottom = 30, left = 0, right = 0 } ]

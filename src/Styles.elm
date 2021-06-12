module Styles exposing (..)

import Colors as Colors
import Config as Cfg
import Element as E exposing (Attribute, Color, Device, DeviceClass(..), Element)
import Element.Background as B
import Element.Border as Border
import Element.Font as Font
import RankNFiles exposing (File(..), Move(..), Rank(..), fileToInt, rankToInt)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P



--- Animations


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


knightPosition : File -> Rank -> Device -> ( Float, Float )
knightPosition file rank device =
    case device.class of
        Desktop ->
            ( fileToInt file * toFloat Cfg.desktopSquareWidth + toFloat Cfg.knightWidth / 2
            , (toFloat (8 - rankToInt rank) * toFloat Cfg.desktopSquareWidth) + (toFloat <| getKnightSize device + 5)
            )

        Tablet ->
            ( fileToInt file * toFloat Cfg.tabletSquareWidth + toFloat Cfg.knightWidth / 2
            , (toFloat (8 - rankToInt rank) * toFloat Cfg.tabletSquareWidth) + (toFloat <| getKnightSize device + 5)
            )

        _ ->
            ( fileToInt file * toFloat Cfg.desktopSquareWidth + toFloat Cfg.knightWidth / 2
            , (toFloat (8 - rankToInt rank) * toFloat Cfg.desktopSquareWidth) + (toFloat <| getKnightSize device + 5)
            )



----
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
    , B.color Colors.bgColor
    , E.width <| E.fill
    , E.height E.fill
    , Font.family
        [ Font.typeface "Hind Siluguri"
        , Font.sansSerif
        ]
    ]


contentPortrait : Device -> List (Attribute msg)
contentPortrait { class, orientation } =
    case class of
        _ ->
            [ E.centerX
            , E.centerY
            , E.width <| E.fill
            , E.height <| E.fill
            , E.spaceEvenly
            , E.padding 10
            ]


content : Device -> List (Attribute msg)
content { class, orientation } =
    case class of
        BigDesktop ->
            [ E.centerX
            , E.centerY
            , E.width <| (E.fill |> E.maximum 1500 |> E.minimum 400)
            , E.height <| (E.px <| Cfg.squareWidth * 8)
            , E.spaceEvenly
            ]

        Desktop ->
            [ E.centerX
            , E.centerY
            , E.width <| (E.fill |> E.maximum 1500 |> E.minimum 1200)
            , E.height <| (E.px <| Cfg.squareWidth * 8)
            ]

        _ ->
            [ E.centerX
            , E.centerY
            , E.width <| E.fill
            , E.height <| E.fill
            , E.spaceEvenly
            , E.padding 10
            ]


mainContentLandscape : Device -> List (Attribute msg)
mainContentLandscape { class } =
    case class of
        BigDesktop ->
            [ E.width <| E.fillPortion 1
            , E.height E.fill
            , E.paddingEach { top = 0, left = 40, right = 40, bottom = 30 }
            ]

        Desktop ->
            [ E.width <| E.fillPortion 1
            , E.height (E.px <| 8 * Cfg.desktopSquareWidth)
            , E.paddingEach { top = 0, left = 20, right = 20, bottom = 0 }
            ]

        _ ->
            [ E.width <| E.fillPortion 1
            , E.height (E.px <| 8 * Cfg.desktopSquareWidth)
            , E.paddingEach { top = 0, left = 20, right = 20, bottom = 0 }
            ]



-- Tablet ->
--     [ E.paddingEach { top = 0, left = 10, right = 10, bottom = 0 }
--     , E.width <| (E.fill |> E.maximum 1000)
--     , E.centerX
--     , E.spaceEvenly
--     ]
-- _ ->
--     [ E.paddingEach { top = 0, left = 10, right = 10, bottom = 0 }
--     ]


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
            , E.height (E.px <| 8 * Cfg.desktopSquareWidth)
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
            , Border.color Colors.navyBlue
            , E.centerY
            , E.centerX
            , B.color Colors.knightColor
            , Border.rounded 20
            , E.paddingXY 20 40
            ]

        _ ->
            [ Border.width 2
            , Border.color Colors.navyBlue
            , E.centerY
            , E.centerX
            , B.color Colors.knightColor
            , Border.rounded 20
            , E.paddingXY 10 20
            ]


finishedStats =
    [ E.paddingEach { top = 40, left = 40, right = 40, bottom = 30 }
    , B.color Colors.goldenRod
    , E.centerY
    , E.centerX
    , Border.rounded 20
    , Font.color Colors.navyBlue
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
            , Border.shadow { offset = ( 0, 1 ), size = 1, blur = 2, color = Colors.knightShadow }
            ]

        Desktop ->
            [ E.paddingXY 120 20
            , B.color <| E.rgb255 53 117 35
            , E.centerX
            , Border.rounded 20
            , Font.size 24
            , Font.letterSpacing 1.5
            , Border.shadow { offset = ( 0, 1 ), size = 1, blur = 2, color = Colors.knightShadow }
            ]

        _ ->
            [ E.paddingXY 60 20
            , B.color <| E.rgb255 53 117 35
            , E.centerX
            , Border.rounded 5
            , Font.size 18
            , Font.letterSpacing 1.5
            , Border.shadow { offset = ( 0, 1 ), size = 1, blur = 2, color = Colors.knightShadow }

            -- , E.height E.fill
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
        ++ [ Font.color Colors.white
           , B.color Colors.red
           , Border.color Colors.red
           , Border.width 1
           , E.alignBottom
           , E.centerX
           ]


headingContainer : List (Attribute msg)
headingContainer =
    [ E.centerX, Font.center ]


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
    [ Font.color Colors.knightColor
    , Font.size fontSize
    , Font.bold
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
            [ E.width <| E.px Cfg.squareWidth
            , E.height <| E.px Cfg.squareWidth
            , B.color color
            , E.centerY
            , E.centerX
            , E.focused []
            ]

        Desktop ->
            [ E.width <| E.px <| Cfg.desktopSquareWidth
            , E.height <| E.px <| Cfg.desktopSquareWidth
            , B.color color
            , E.centerY
            , E.centerX
            , E.focused []
            ]

        Tablet ->
            [ E.width <| E.px <| Cfg.tabletSquareWidth
            , E.height <| E.px <| Cfg.tabletSquareWidth
            , B.color color
            , E.centerY
            , E.centerX
            , E.focused []
            ]

        _ ->
            [ E.width <| E.px <| round <| Cfg.squareWidth / 1.25
            , E.height <| E.px <| round <| Cfg.squareWidth / 1.25
            , B.color color
            , E.centerY
            , E.centerX
            , E.focused []
            ]


targetSquare : Device -> List (Attribute msg)
targetSquare { class } =
    case class of
        BigDesktop ->
            [ E.width <| E.px Cfg.squareWidth
            , E.height <| E.px Cfg.squareWidth
            , B.color Colors.targetColor
            , E.centerY
            , E.centerX
            , E.focused []
            , B.color Colors.targetColor
            ]

        Desktop ->
            [ E.width <| E.px Cfg.desktopSquareWidth
            , E.height <| E.px Cfg.desktopSquareWidth
            , B.color Colors.targetColor
            , E.centerY
            , E.centerX
            , E.focused []
            , B.color Colors.targetColor
            ]

        _ ->
            [ E.width <| E.px Cfg.tabletSquareWidth
            , E.height <| E.px Cfg.tabletSquareWidth
            , B.color Colors.targetColor
            , E.centerY
            , E.centerX
            , E.focused []
            , B.color Colors.targetColor
            ]


knight : List (Attribute msg)
knight =
    [ E.centerX
    , E.centerY
    , E.width <| E.px Cfg.knightWidth
    , E.height <| E.px Cfg.knightWidth
    ]


getKnightSize : Device -> Int
getKnightSize { class } =
    case class of
        Tablet ->
            round <| Cfg.selectedKnightWidth / 1.25

        _ ->
            Cfg.selectedKnightWidth


selectedKnight : Device -> List (Attribute msg)
selectedKnight device =
    [ E.centerX
    , E.centerY
    , E.width <| E.px <| getKnightSize device
    , E.height <| E.px <| getKnightSize device
    , E.pointer
    , Border.shadow
        { offset = ( 0, 0 )
        , size = 0.1
        , blur = 40
        , color = Colors.knightShadow
        }
    , B.color Colors.legalMoveCircleColorLight
    , Border.rounded 20
    ]


queen : Device -> List (Attribute msg)
queen { class } =
    case class of
        BigDesktop ->
            [ E.centerX
            , E.centerY
            , E.width <| E.px Cfg.queenWidth
            , E.height <| E.px Cfg.queenWidth
            ]

        _ ->
            [ E.centerX
            , E.centerY
            , E.width <| E.px Cfg.desktopQueenWidth
            , E.height <| E.px Cfg.desktopQueenWidth
            ]


legalMoveCircle : List (Attribute msg)
legalMoveCircle =
    [ B.color Colors.legalMoveCircleColorLight
    , E.width <| E.px Cfg.legalMoveCircleWidth
    , E.height <| E.px Cfg.legalMoveCircleWidth
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
    , Font.color Colors.underAttackRed
    ]


x1 : List (Attribute msg)
x1 =
    [ E.rotate (degrees 45)
    , E.width (E.px Cfg.legalMoveCircleWidth)
    , E.height (E.px 2)
    , B.color Colors.underAttackRed
    , E.inFront <| E.el x2 E.none
    ]


x2 : List (Attribute msg)
x2 =
    [ E.rotate (degrees 90)
    , E.width (E.px Cfg.legalMoveCircleWidth)
    , E.height (E.px 2)
    , B.color Colors.underAttackRed
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
            , E.width (E.px Cfg.squareWidth)
            ]

        Desktop ->
            [ Font.color <| E.rgb 1 1 1
            , E.centerY
            , E.centerX
            , E.paddingXY 0 7
            , E.width (E.px Cfg.desktopSquareWidth)
            ]

        _ ->
            [ Font.color <| E.rgb 1 1 1
            , E.centerY
            , E.centerX
            , E.paddingXY 0 7
            , E.width (E.px Cfg.tabletSquareWidth)
            ]


targetSquareName : Device -> List (Attribute msg)
targetSquareName { class } =
    case class of
        BigDesktop ->
            [ Font.size 100
            , Font.color Colors.white
            , E.centerX
            , E.centerY
            , E.paddingEach { top = 0, bottom = 20, left = 0, right = 0 }
            ]

        _ ->
            [ Font.size 80
            , Font.color Colors.white
            , E.centerX
            , E.centerY
            , E.paddingEach { top = 20, bottom = 0, left = 0, right = 0 }
            ]


knightStartingSquareText : Device -> List (Attribute msg)
knightStartingSquareText { class } =
    case class of
        Tablet ->
            [ Font.size 24
            , Font.color Colors.knightLightGold
            ]

        _ ->
            [ Font.size 32
            , Font.color Colors.knightLightGold
            ]


targetSquareText : Device -> List (Attribute msg)
targetSquareText { class } =
    case class of
        Tablet ->
            [ Font.size 24
            , Font.color Colors.targetColor
            ]

        _ ->
            [ Font.size 32
            , Font.color Colors.targetColor
            ]


queenSquareText : Device -> List (Attribute msg)
queenSquareText { class } =
    case class of
        Tablet ->
            [ Font.size 24
            , Font.color Colors.queenColor
            ]

        _ ->
            [ Font.size 32
            , Font.color Colors.queenColor
            ]


totalMovesText =
    [ E.centerX
    , Font.color Colors.targetColor
    , Font.size 20
    , E.paddingEach { top = 5, left = 0, right = 0, bottom = 5 }
    ]


wrongMovesText =
    [ E.centerX
    , E.centerY
    , Font.color Colors.lightRed
    , Font.size 20
    , E.paddingEach { top = 5, left = 0, right = 0, bottom = 5 }
    ]


totalMovesNumber =
    [ Font.size 40
    , Font.color Colors.white
    , E.paddingEach { top = 40, left = 0, right = 0, bottom = 5 }
    , E.centerX
    ]


wrongMovesNumber =
    [ Font.size 40
    , Font.color Colors.white
    , E.paddingEach { top = 40, left = 0, right = 0, bottom = 5 }
    , E.centerX
    ]


timer : Device -> List (Attribute msg)
timer { class } =
    case class of
        BigDesktop ->
            [ Font.size 60
            , Font.color Colors.white
            , E.centerX
            ]

        _ ->
            [ Font.size 50
            , Font.color Colors.white
            , E.centerX
            ]


congrats =
    [ Font.size 30
    , Font.color Colors.navyBlue
    , E.paddingEach { top = 0, bottom = 30, left = 0, right = 0 }
    ]


took =
    [ E.paddingEach { top = 0, bottom = 30, left = 0, right = 0 } ]

module Styles exposing (..)

import Element as E exposing (Attribute, Color, Element)
import Element.Background as B
import Element.Border as Border
import Element.Events as EE
import Element.Font as F


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
    E.rgb 232 235 239


squareLight : Color
squareLight =
    E.rgb 125 135 150


warnColor : Color
warnColor =
    E.rgba 181 20 18 0.8



---- STYLES ----


square : List (Attribute msg)
square =
    [ E.width <| E.px squareWidth
    , E.height <| E.px squareWidth
    , Border.width 1
    , Border.solid
    ]

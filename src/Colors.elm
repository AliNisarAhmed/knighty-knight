module Colors exposing (..)

import Element exposing (Color, rgb, rgb255, rgba, rgba255)


white : Color
white =
    rgb 1 1 1


squareDarkColor : Color
squareDarkColor =
    rgb255 232 235 239


squareLight : Color
squareLight =
    rgb255 125 135 150


warnColor : Color
warnColor =
    rgba255 181 20 18 0.8


bgColor : Color
bgColor =
    rgb255 70 130 180


legalMoveCircleColorLight : Color
legalMoveCircleColorLight =
    rgba255 0 0 0 0.23


targetColor : Color
targetColor =
    rgba 0 1 0 0.6


knightColor : Color
knightColor =
    rgb255 82 27 59


knightLightGold : Color
knightLightGold =
    rgb255 255 207 112


knightShadow : Color
knightShadow =
    rgba255 0 0 0 0.6


queenColor : Color
queenColor =
    rgb255 128 77 118


black : Color
black =
    rgb 0 0 0


red : Color
red =
    rgba255 150 11 11 0.8


underAttackRed : Color
underAttackRed =
    red


navyBlue : Color
navyBlue =
    rgb255 0 0 128


lightRed : Color
lightRed =
    rgba255 255 134 134 0.8


goldenRod : Color
goldenRod =
    rgb255 218 165 32

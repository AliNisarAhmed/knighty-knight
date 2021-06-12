module Config exposing (..)

import Element exposing (modular)


scales =
    modular 10 1.5


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


phoneSquareWidth : Int
phoneSquareWidth =
    48


knightWidth : number
knightWidth =
    50


knightWidthPhone : number
knightWidthPhone =
    40


selectedKnightWidth : number
selectedKnightWidth =
    60


queenWidth : number
queenWidth =
    80


desktopQueenWidth : number
desktopQueenWidth =
    60


tabletQueenWidth : number
tabletQueenWidth =
    55


phoneQueenWidth : number
phoneQueenWidth =
    45


legalMoveCircleWidth : number
legalMoveCircleWidth =
    20

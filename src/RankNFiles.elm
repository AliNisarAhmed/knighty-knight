module RankNFiles exposing (..)


type File
    = A
    | B
    | C
    | D
    | E
    | F
    | G
    | H


type Rank
    = One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight


files : List File
files =
    [ A, B, C, D, E, F, G, H ]


ranks : List Rank
ranks =
    [ Eight, Seven, Six, Five, Four, Three, Two, One ]


fileToString : File -> String
fileToString f =
    case f of
        A ->
            "a"

        B ->
            "b"

        C ->
            "c"

        D ->
            "d"

        E ->
            "e"

        F ->
            "f"

        G ->
            "g"

        H ->
            "h"


rankToInt : Rank -> Int
rankToInt f =
    case f of
        One ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8

module RankNFiles exposing (..)

import Array exposing (Array)
import List.Extra exposing (dropWhile)
import Maybe.Extra exposing (values)


type Move
    = Legal
    | Illegal


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


type NextTarget
    = NextTarget ( File, Rank ) (Array ( File, Rank ))
    | NotHit
    | Win


type alias LegalMoves =
    List ( File, Rank )


files : List File
files =
    [ A, B, C, D, E, F, G, H ]


ranks : List Rank
ranks =
    [ Eight, Seven, Six, Five, Four, Three, Two, One ]


fileToInt : File -> number
fileToInt f =
    case f of
        A ->
            0

        B ->
            1

        C ->
            2

        D ->
            3

        E ->
            4

        F ->
            5

        G ->
            6

        H ->
            7


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


rankToString : Rank -> String
rankToString =
    rankToInt >> String.fromInt


squareToString : ( File, Rank ) -> String
squareToString ( file, rank ) =
    fileToString file ++ rankToString rank


combineToLegalMove : Maybe File -> Maybe Rank -> Maybe ( File, Rank )
combineToLegalMove mf mr =
    case ( mf, mr ) of
        ( Just f, Just r ) ->
            Just ( f, r )

        _ ->
            Nothing


getLegalMoves : File -> Rank -> LegalMoves
getLegalMoves file rank =
    let
        shortLeft =
            prevFile file

        longLeft =
            nextToNext prevFile file

        shortRight =
            nextFile file

        longRight =
            nextToNext nextFile file

        moves =
            [ combineToLegalMove shortLeft <| nextToNext nextRank rank
            , combineToLegalMove shortLeft <| nextToNext prevRank rank
            , combineToLegalMove longLeft <| nextRank rank
            , combineToLegalMove longLeft <| prevRank rank
            , combineToLegalMove shortRight <| nextToNext nextRank rank
            , combineToLegalMove shortRight <| nextToNext prevRank rank
            , combineToLegalMove longRight <| nextRank rank
            , combineToLegalMove longRight <| prevRank rank
            ]
    in
    moves
        |> values


nextFile : File -> Maybe File
nextFile f =
    case f of
        A ->
            Just B

        B ->
            Just C

        C ->
            Just D

        D ->
            Just E

        E ->
            Just F

        F ->
            Just G

        G ->
            Just H

        H ->
            Nothing


nextToNext : (a -> Maybe a) -> a -> Maybe a
nextToNext f a =
    f a |> Maybe.andThen f


prevFile : File -> Maybe File
prevFile f =
    case f of
        H ->
            Just G

        G ->
            Just F

        F ->
            Just E

        E ->
            Just D

        D ->
            Just C

        C ->
            Just B

        B ->
            Just A

        A ->
            Nothing


nextRank : Rank -> Maybe Rank
nextRank r =
    case r of
        One ->
            Just Two

        Two ->
            Just Three

        Three ->
            Just Four

        Four ->
            Just Five

        Five ->
            Just Six

        Six ->
            Just Seven

        Seven ->
            Just Eight

        Eight ->
            Nothing


prevRank : Rank -> Maybe Rank
prevRank r =
    case r of
        Eight ->
            Just Seven

        Seven ->
            Just Six

        Six ->
            Just Five

        Five ->
            Just Four

        Four ->
            Just Three

        Three ->
            Just Two

        Two ->
            Just One

        One ->
            Nothing


getNextTarget : ( File, Rank ) -> Maybe ( File, Rank )
getNextTarget fr =
    validSequence
        |> dropWhile (\item -> item /= fr)
        |> List.drop 1
        |> List.head


getNextTarget2 : ( File, Rank ) -> Array ( File, Rank ) -> NextTarget
getNextTarget2 ( file, rank ) array =
    let
        current =
            Array.get 0 array

        next =
            Array.get 1 array

        arrayLength =
            Array.length array
    in
    case ( current, next ) of
        ( Just c, Just n ) ->
            if ( file, rank ) == c then
                NextTarget n (Array.slice 1 arrayLength array)

            else
                NotHit

        ( Just c, Nothing ) ->
            if ( file, rank ) == c then
                Win

            else
                NotHit

        _ ->
            NotHit


queenMoves : List ( File, Rank )
queenMoves =
    [ ( D, Eight )
    , ( D, Seven )
    , ( D, Six )
    , ( D, Five )
    , ( D, Four )
    , ( D, Three )
    , ( D, Two )
    , ( D, One )
    , ( E, Five )
    , ( F, Five )
    , ( G, Five )
    , ( H, Five )
    , ( C, Five )
    , ( B, Five )
    , ( A, Five )
    , ( E, Six )
    , ( F, Seven )
    , ( G, Eight )
    , ( A, Two )
    , ( B, Three )
    , ( C, Four )
    , ( A, Eight )
    , ( B, Seven )
    , ( C, Six )
    , ( E, Four )
    , ( F, Three )
    , ( G, Two )
    , ( H, One )
    ]


validSequence : List ( File, Rank )
validSequence =
    [ ( F, Eight )
    , ( E, Eight )
    , ( C, Eight )
    , ( B, Eight )
    , ( H, Seven )
    , ( G, Seven )
    , ( E, Seven )
    , ( C, Seven )
    , ( A, Seven )
    , ( H, Six )
    , ( G, Six )
    , ( F, Six )
    , ( B, Six )
    , ( A, Six )
    , ( H, Four )
    , ( G, Four )
    , ( F, Four )
    , ( B, Four )
    , ( A, Four )
    , ( H, Three )
    , ( G, Three )
    , ( C, Three )
    , ( A, Three )
    , ( H, Two )
    , ( F, Two )
    , ( E, Two )
    , ( C, Two )
    , ( B, Two )
    , ( G, One )
    , ( F, One )
    , ( E, One )
    , ( C, One )
    , ( B, One )
    , ( A, One )
    ]

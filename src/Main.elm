port module Main exposing (main)

import Array exposing (Array)
import Browser exposing (Document)
import Css exposing (..)
import Element as E exposing (Color, Element)
import Element.Input as Input
import RankNFiles exposing (..)
import Styles as St
import Time


knightFilePath =
    "assets/horse.svg"


queenFilePath =
    "assets/queen2.svg"


type GameState
    = NotStarted
    | Ready
    | Started
    | Finished


type alias Knight =
    { rank : Rank
    , file : File
    }


knightStartingPosition : Knight
knightStartingPosition =
    { rank = Eight
    , file = H
    }


init : () -> () -> () -> ( Model, Cmd Msg )
init _ _ _ =
    ( initModel, Cmd.none )


main : Program () Model Msg
main =
    Browser.document
        { init = always ( initModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-------------------------------------------------------
---------------------- UPDATE ---------------------------
-------------------------------------------------------


type alias Model =
    { knight : Knight
    , knightSelected : Maybe LegalMoves
    , currentTarget : ( File, Rank )
    , totalMoves : Int
    , wrongMoves : Int
    , gameState : GameState
    , timer : Maybe Int
    , validMoves : Array ( File, Rank )
    }


initModel : Model
initModel =
    { knight = knightStartingPosition
    , knightSelected = Nothing
    , currentTarget = ( F, Eight )
    , totalMoves = 0
    , wrongMoves = 0
    , gameState = NotStarted
    , timer = Nothing
    , validMoves = Array.fromList validSequence
    }


type Msg
    = ToggleKnightSelect File Rank
    | MoveKnight File Rank
    | Tick Time.Posix
    | StartPressed
    | ResetGame
    | NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.gameState of
        Started ->
            Time.every 1000 Tick

        _ ->
            Sub.none


port playSound : () -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartPressed ->
            ( { model
                | knightSelected = Just <| getLegalMoves H Eight
                , gameState = Ready
              }
            , Cmd.none
            )

        ResetGame ->
            ( initModel, Cmd.none )

        ToggleKnightSelect file rank ->
            case model.knightSelected of
                Nothing ->
                    ( { model | knightSelected = Just <| getLegalMoves file rank }, Cmd.none )

                Just _ ->
                    ( { model | knightSelected = Nothing }, Cmd.none )

        MoveKnight file rank ->
            let
                isAttackedByQueen =
                    List.member ( file, rank ) queenMoves
            in
            if isAttackedByQueen then
                ( { model | wrongMoves = model.wrongMoves + 1 }, Cmd.none )

            else
                let
                    nextTarget =
                        getNextTarget2 ( file, rank ) model.validMoves

                    newGameState =
                        case model.gameState of
                            NotStarted ->
                                Started

                            _ ->
                                Started
                in
                case nextTarget of
                    NextTarget newTarget remainingValidMoves ->
                        ( { model
                            | knight = { rank = rank, file = file }
                            , knightSelected = Just <| getLegalMoves file rank
                            , currentTarget = newTarget
                            , totalMoves = model.totalMoves + 1
                            , validMoves = remainingValidMoves
                            , gameState = newGameState
                          }
                        , playSound ()
                        )

                    NotHit ->
                        ( { model
                            | knight = { rank = rank, file = file }
                            , knightSelected = Just <| getLegalMoves file rank
                            , totalMoves = model.totalMoves + 1
                            , gameState = newGameState
                          }
                        , playSound ()
                        )

                    Win ->
                        ( { model
                            | gameState = Finished
                            , knight = { rank = rank, file = file }
                            , knightSelected = Nothing
                          }
                        , Cmd.none
                        )

        Tick _ ->
            let
                newTime =
                    case model.timer of
                        Nothing ->
                            Just 1

                        Just secs ->
                            Just <| secs + 1
            in
            ( { model | timer = newTime }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-------------------------------------------------------
---------------------- VIEW ---------------------------
-------------------------------------------------------


view : Model -> Document Msg
view model =
    { title = "Knighty Knight"
    , body =
        [ E.layout St.layout <|
            E.row St.content <|
                [ E.column St.mainContent <| mainContent model
                , E.column St.boardColumn <| board model
                ]
        ]
    }


mainContent : Model -> List (Element Msg)
mainContent { currentTarget, totalMoves, wrongMoves, timer, gameState } =
    let
        accuracy =
            (totalMoves - wrongMoves) * 100 // totalMoves
    in
    case gameState of
        NotStarted ->
            [ title
            , explanation
            , startButton
            ]

        Finished ->
            [ title
            , E.paragraph [] [ E.text "Congratulations, you did it..." ]
            , E.text <| "It took you: "
            , E.paragraph []
                [ displayTimer timer ]
            , E.paragraph []
                [ E.el [] <| E.text <| String.fromInt totalMoves
                , E.text " total moves, "
                ]
            , E.paragraph []
                [ E.text " with an accuracy of "
                , E.el [] <| E.text <| String.fromInt accuracy ++ "%"
                ]
            , restartButton
            ]

        _ ->
            [ title
            , E.el St.targetSquareName <| E.text <| squareToString currentTarget
            , E.column St.stats <|
                [ displayTimer timer
                , E.el St.wrongMovesNumber <| E.text <| String.fromInt wrongMoves
                , E.paragraph St.wrongMovesText <|
                    [ E.text <| "Wrong attempted moves"
                    ]
                , E.el St.totalMovesNumber <| E.text <| String.fromInt totalMoves
                , E.el St.totalMovesText <|
                    E.text <|
                        "Total moves"
                ]
            , resetButton
            ]


title : Element Msg
title =
    E.el [ E.width E.fill ] <| E.el St.heading <| E.text "A KNIGHT'S JOURNEY"


explanation : Element Msg
explanation =
    E.paragraph St.text <|
        [ E.text "Can you take the knight at "
        , E.el St.knightStartingSquareText <| E.text "h8"
        , E.text " square, visiting all the squares one by one (left to right, top to bottom), all the way to the "
        , E.el St.targetSquareText <| E.text "a1"
        , E.text " square, while avoiding all the squares attacked by the enemy "
        , E.el St.queenSquareText <| E.text "Queen"
        , E.text " stationed at d5?"
        ]


startButton : Element Msg
startButton =
    Input.button St.startButton { onPress = Just <| StartPressed, label = E.el [] <| E.text "START" }


resetButton : Element Msg
resetButton =
    Input.button St.resetButton { onPress = Just ResetGame, label = E.el [] <| E.text "RESET" }


restartButton : Element Msg
restartButton =
    Input.button St.startButton { onPress = Just ResetGame, label = E.el [] <| E.text "Play Again?" }


board : Model -> List (Element Msg)
board model =
    let
        drawnBox =
            case model.gameState of
                NotStarted ->
                    startingBox

                _ ->
                    box
    in
    List.map
        (\rank ->
            E.row [] <|
                rankLabel rank
                    :: List.map (\file -> drawnBox file rank model) files
        )
        ranks
        ++ fileLabelRow files


box : File -> Rank -> Model -> Element Msg
box file rank { knight, knightSelected, currentTarget, gameState } =
    let
        boxColor =
            getBoxColor file rank

        knightStyles =
            case knightSelected of
                Just _ ->
                    St.selectedKnight

                Nothing ->
                    St.knight

        knightImg =
            if file == knight.file && rank == knight.rank then
                E.image
                    knightStyles
                    { src = knightFilePath, description = "Knight" }

            else
                E.none

        move =
            case knightSelected of
                Nothing ->
                    Illegal

                Just legalMoves ->
                    if List.member ( file, rank ) legalMoves then
                        Legal

                    else
                        Illegal

        moveHandler =
            case move of
                Legal ->
                    Just <| MoveKnight file rank

                Illegal ->
                    Nothing

        queenImg =
            if file == D && rank == Five then
                E.image
                    St.queen
                    { src = queenFilePath, description = "Queen" }

            else
                E.none

        targetSquare =
            if ( file, rank ) == currentTarget && gameState /= Finished then
                E.el St.targetSquare E.none

            else
                E.none
    in
    case move of
        Legal ->
            Input.button
                (St.square boxColor)
                { onPress = moveHandler
                , label = E.row (St.legalMoveSquare legalMoveCircle) [ queenImg, targetSquare ]
                }

        Illegal ->
            E.row (St.square boxColor) <| [ knightImg, queenImg, targetSquare ]


startingBox : File -> Rank -> Model -> Element Msg
startingBox file rank { knight, knightSelected } =
    let
        isAttackedByQueen =
            if ( file, rank ) /= ( D, Five ) && List.member ( file, rank ) queenMoves then
                True

            else
                False

        attackedByQueenSquare =
            if isAttackedByQueen then
                E.row St.attackedByQueen <| [ E.el St.x1 E.none ]

            else
                E.none

        boxColor =
            getBoxColor file rank

        knightStyles =
            case knightSelected of
                Just _ ->
                    St.selectedKnight

                Nothing ->
                    St.knight

        knightImg =
            if file == knight.file && rank == knight.rank then
                E.image
                    knightStyles
                    { src = knightFilePath, description = "Knight" }

            else
                E.none

        queenImg =
            if file == D && rank == Five then
                E.image
                    St.queen
                    { src = queenFilePath, description = "Queen" }

            else
                E.none

        targetSquare =
            if ( file, rank ) == ( A, One ) then
                E.el St.targetSquare E.none

            else
                E.none
    in
    E.row (St.square boxColor) <| [ knightImg, queenImg, targetSquare, attackedByQueenSquare ]


legalMoveCircle : Element Msg
legalMoveCircle =
    E.el St.legalMoveCircle E.none


rankLabel : Rank -> Element Msg
rankLabel rank =
    E.el St.rankLabelText (E.text <| rankToString rank)


fileLabelRow : List File -> List (Element Msg)
fileLabelRow files =
    [ E.row [] <| E.el St.blankRankLabel E.none :: List.map fileLabel files ]


fileLabel : File -> Element Msg
fileLabel file =
    E.el St.fileLabelText (E.el St.center <| E.text <| fileToString file)


displayTimer : Maybe Int -> Element msg
displayTimer timer =
    case timer of
        Nothing ->
            E.el St.timer <| E.text <| showTime 0

        Just secs ->
            E.el St.timer <| E.text <| showTime secs


showTime : Int -> String
showTime secs =
    let
        seconds =
            modBy 60 secs

        minutes =
            secs // 60

        secondsString =
            seconds
                |> String.fromInt
                |> String.pad 2 '0'

        minutesString =
            minutes
                |> String.fromInt
                |> String.pad 2 '0'
    in
    minutesString ++ ":" ++ secondsString


getBoxColor : File -> Rank -> Color
getBoxColor f r =
    let
        blackBg =
            St.squareDarkColor

        whiteBg =
            St.squareLight
    in
    case ( f, modBy 2 <| rankToInt r ) of
        ( A, 0 ) ->
            blackBg

        ( A, 1 ) ->
            whiteBg

        ( B, 0 ) ->
            whiteBg

        ( B, 1 ) ->
            blackBg

        ( C, 0 ) ->
            blackBg

        ( C, 1 ) ->
            whiteBg

        ( D, 0 ) ->
            whiteBg

        ( D, 1 ) ->
            blackBg

        ( E, 0 ) ->
            blackBg

        ( E, 1 ) ->
            whiteBg

        ( F, 0 ) ->
            whiteBg

        ( F, 1 ) ->
            blackBg

        ( G, 0 ) ->
            blackBg

        ( G, 1 ) ->
            whiteBg

        ( H, 0 ) ->
            whiteBg

        ( H, 1 ) ->
            blackBg

        _ ->
            blackBg

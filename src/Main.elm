port module Main exposing (main)

import Array exposing (Array)
import Browser exposing (Document)
import Browser.Events exposing (onResize)
import Colors as Colors
import Css exposing (..)
import Element as E exposing (Color, Device, DeviceClass(..), Element, Orientation(..))
import Element.Input as Input
import Process
import RankNFiles exposing (..)
import Styles as St exposing (knightPosition)
import Task
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


knightStartingCoords : Knight
knightStartingCoords =
    { rank = Eight
    , file = H
    }


init : Flags -> ( Model, Cmd Msg )
init { height, width } =
    let
        device =
            E.classifyDevice { width = width, height = height }
    in
    ( { initModel | device = device }, Cmd.none )


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-------------------------------------------------------
---------------------- UPDATE ---------------------------
-------------------------------------------------------


type alias Flags =
    { width : Int
    , height : Int
    }


type alias Model =
    { knight : Knight
    , knightOldPosition : ( Float, Float )
    , knightNewPosition : ( Float, Float )
    , knightSelected : Maybe LegalMoves
    , currentTarget : ( File, Rank )
    , totalMoves : Int
    , wrongMoves : Int
    , gameState : GameState
    , timer : Maybe Int
    , validMoves : Array ( File, Rank )
    , wrongMoveSquare : Maybe ( File, Rank )
    , device : Device
    }


initModel : Model
initModel =
    let
        device =
            { class = BigDesktop, orientation = Landscape }

        knightStartingPosition =
            St.knightPosition knightStartingCoords.file knightStartingCoords.rank device
    in
    { knight = knightStartingCoords
    , knightOldPosition = knightStartingPosition
    , knightNewPosition = knightStartingPosition
    , knightSelected = Nothing
    , currentTarget = ( F, Eight )
    , totalMoves = 0
    , wrongMoves = 0
    , gameState = NotStarted
    , timer = Nothing
    , validMoves = Array.fromList validSequence
    , wrongMoveSquare = Nothing
    , device = device
    }


type Msg
    = ToggleKnightSelect File Rank
    | MoveKnight File Rank
    | Tick Time.Posix
    | StartPressed
    | ResetGame
    | HideWrongMove ()
    | GotNewWidth Int Int
    | NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.gameState of
        Started ->
            Sub.batch [ Time.every 1000 Tick, onResize GotNewWidth ]

        _ ->
            onResize GotNewWidth


sleep : Task.Task x ()
sleep =
    Process.sleep 200


port playSound : () -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotNewWidth w h ->
            let
                knightNewPosition =
                    St.knightPosition knightStartingCoords.file knightStartingCoords.rank model.device
            in
            ( { model
                | device = E.classifyDevice { width = w, height = h }
                , knightOldPosition = model.knightNewPosition
                , knightNewPosition = knightNewPosition
              }
            , Cmd.none
            )

        HideWrongMove _ ->
            ( { model | wrongMoveSquare = Nothing }, Cmd.none )

        StartPressed ->
            let
                knightPosition =
                    St.knightPosition knightStartingCoords.file knightStartingCoords.rank model.device
            in
            ( { model
                | knightSelected = Just <| getLegalMoves H Eight
                , knightOldPosition = knightPosition
                , knightNewPosition = knightPosition
                , gameState = Ready
              }
            , Cmd.none
            )

        ResetGame ->
            ( { initModel | device = model.device }, Cmd.none )

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
                ( { model | wrongMoves = model.wrongMoves + 1, wrongMoveSquare = Just ( file, rank ) }
                , Task.perform HideWrongMove sleep
                )

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

                    knightNewPosition =
                        St.knightPosition file rank model.device
                in
                case nextTarget of
                    NextTarget newTarget remainingValidMoves ->
                        ( { model
                            | knight = { rank = rank, file = file }
                            , knightOldPosition = model.knightNewPosition
                            , knightNewPosition = knightNewPosition
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
                            , knightOldPosition = model.knightNewPosition
                            , knightNewPosition = knightNewPosition
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
                            , knightOldPosition = model.knightNewPosition
                            , knightNewPosition = knightNewPosition
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
    let
        mainContentSection =
            case model.device.orientation of
                Portrait ->
                    E.row (St.mainContent model.device) <| mainContent model

                Landscape ->
                    E.column (St.mainContentLandscape model.device) <| mainContent model

        titleText =
            "Knighty Knight"
    in
    case model.device.orientation of
        Portrait ->
            { title = titleText
            , body =
                [ E.layout St.layout <|
                    E.column (St.contentPortrait model.device) <|
                        [ title model.device
                        , mainContentSection
                        , E.column (St.boardColumn model.device) <| board model
                        ]
                ]
            }

        Landscape ->
            { title = titleText
            , body =
                [ E.layout St.layout <|
                    E.row (St.content model.device) <|
                        [ mainContentSection
                        , E.column (St.boardColumn model.device) <| board model
                        ]
                ]
            }


mainContent : Model -> List (Element Msg)
mainContent { currentTarget, totalMoves, wrongMoves, timer, gameState, device } =
    let
        accuracy =
            (totalMoves - wrongMoves) * 100 // totalMoves

        titleElement =
            case device.orientation of
                Portrait ->
                    E.none

                _ ->
                    title device
    in
    case gameState of
        NotStarted ->
            [ titleElement
            , explanation device
            , startButton device
            ]

        Finished ->
            [ titleElement
            , E.column St.finishedStats <|
                [ E.paragraph St.congrats [ E.text "Congratulations, you did it!" ]
                , E.el St.took <| E.text "It took you: "
                , E.column St.statList <|
                    [ E.paragraph []
                        [ E.text "- ", displayFinalTimer timer, E.text "," ]
                    , E.paragraph []
                        [ E.text "- "
                        , E.el [] <| E.text <| String.fromInt totalMoves
                        , E.text " total moves, "
                        ]
                    , E.paragraph []
                        [ E.text "- "
                        , E.text " with an accuracy of "
                        , E.el [] <| E.text <| String.fromInt accuracy ++ "%"
                        ]
                    ]
                ]
            , restartButton device
            ]

        _ ->
            case device.class of
                Phone ->
                    [ E.column []
                        [ E.el (St.targetSquareName device) <| E.text <| squareToString currentTarget
                        , resetButton device
                        ]
                    , E.column (St.stats device) <|
                        [ displayTimer timer device
                        , E.el (St.wrongMovesNumber device) <| E.text <| String.fromInt wrongMoves
                        , E.paragraph (St.wrongMovesText device) <|
                            [ E.text <| "Wrong attempted moves"
                            ]
                        , E.el (St.totalMovesNumber device) <| E.text <| String.fromInt totalMoves
                        , E.el (St.totalMovesText device) <|
                            E.text <|
                                "Total moves"
                        ]
                    ]

                _ ->
                    [ titleElement
                    , E.el (St.targetSquareName device) <| E.text <| squareToString currentTarget
                    , E.column (St.stats device) <|
                        [ displayTimer timer device
                        , E.el (St.wrongMovesNumber device) <| E.text <| String.fromInt wrongMoves
                        , E.paragraph (St.wrongMovesText device) <|
                            [ E.text <| "Wrong attempted moves"
                            ]
                        , E.el (St.totalMovesNumber device) <| E.text <| String.fromInt totalMoves
                        , E.el (St.totalMovesText device) <|
                            E.text <|
                                "Total moves"
                        ]
                    , resetButton device
                    ]


title : Device -> Element Msg
title device =
    E.paragraph St.headingContainer <| [ E.el (St.heading device) <| E.text "A KNIGHT'S JOURNEY" ]


explanation : Device -> Element Msg
explanation device =
    case device.class of
        Phone ->
            E.textColumn (St.textColumn device)
                [ E.paragraph (St.text device)
                    [ E.text "Take the knight at "
                    , E.el (St.knightStartingSquareText device) <| E.text "h8"
                    , E.text " square, visit each target, avoid the enemy Queen at "
                    , E.el (St.queenSquareText device) <| E.text "d5"
                    , E.text "and reach the end of the board at "
                    , E.el (St.targetSquareText device) <| E.text "a1"
                    ]

                -- , E.paragraph (St.text device)
                --     [ E.text "visit all the squares one by one (left to right, top to bottom), all the way to the "
                --     , E.el (St.targetSquareText device) <| E.text "a1"
                --     , E.text " square"
                --     ]
                -- , E.paragraph (St.text device)
                --     [ E.text "while avoiding all the squares attacked by the enemy "
                --     , E.text "Queen"
                --     , E.text " stationed at "
                --     , E.el (St.queenSquareText device) <| E.text "d5"
                --     , E.text "?"
                --     ]
                ]

        _ ->
            E.textColumn (St.textColumn device)
                [ E.paragraph (St.text device)
                    [ E.text "Can you take the knight at "
                    , E.el (St.knightStartingSquareText device) <| E.text "h8"
                    , E.text " square"
                    ]
                , E.paragraph (St.text device)
                    [ E.text "visit all the squares one by one (left to right, top to bottom), all the way to the "
                    , E.el (St.targetSquareText device) <| E.text "a1"
                    , E.text " square"
                    ]
                , E.paragraph (St.text device)
                    [ E.text "while avoiding all the squares attacked by the enemy "
                    , E.text "Queen"
                    , E.text " stationed at "
                    , E.el (St.queenSquareText device) <| E.text "d5"
                    , E.text "?"
                    ]
                ]


startButton : Device -> Element Msg
startButton device =
    Input.button (St.startButton device) { onPress = Just <| StartPressed, label = E.el [] <| E.text "START" }


resetButton : Device -> Element Msg
resetButton device =
    Input.button (St.resetButton device) { onPress = Just ResetGame, label = E.el [] <| E.text "RESET" }


restartButton : Device -> Element Msg
restartButton device =
    Input.button (St.startButton device) { onPress = Just ResetGame, label = E.el [] <| E.text "Play Again?" }


board : Model -> List (Element Msg)
board model =
    let
        drawnBox =
            case model.gameState of
                NotStarted ->
                    startingBox

                _ ->
                    box

        knightStyles =
            case model.knightSelected of
                Just _ ->
                    St.selectedKnight model.device

                Nothing ->
                    St.knight model.device

        knightImg =
            case model.gameState of
                NotStarted ->
                    E.el [] E.none

                _ ->
                    St.animatedEl (St.animation model.knightOldPosition model.knightNewPosition)
                        []
                        (E.image
                            knightStyles
                            { src = knightFilePath, description = "Knight" }
                        )
    in
    knightImg
        :: List.map
            (\rank ->
                E.row (St.boardRow model.device) <|
                    rankLabel rank
                        :: List.map (\file -> drawnBox file rank model) files
            )
            ranks
        ++ fileLabelRow model.device files


box : File -> Rank -> Model -> Element Msg
box file rank { knightSelected, currentTarget, gameState, wrongMoveSquare, device } =
    let
        boxColor =
            getBoxColor file rank

        squareStyles =
            St.square boxColor device

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
                    (St.queen device)
                    { src = queenFilePath, description = "Queen" }

            else
                E.none

        targetSquare =
            if ( file, rank ) == currentTarget && gameState /= Finished then
                E.el (St.targetSquare device) E.none

            else
                E.none

        knightMoveIndicator =
            Input.button
                squareStyles
                { onPress = moveHandler
                , label = E.row (St.legalMoveSquare legalMoveCircle) [ queenImg, targetSquare ]
                }

        illegalMoveIndicator =
            E.row St.attackedByQueen <| [ E.el St.x1 E.none ]

        illegalMoveSquare =
            Input.button
                squareStyles
                { onPress = moveHandler
                , label = E.row (St.legalMoveSquare illegalMoveIndicator) [ queenImg, targetSquare ]
                }
    in
    case move of
        Legal ->
            case wrongMoveSquare of
                Just ( f, r ) ->
                    if ( f, r ) == ( file, rank ) then
                        illegalMoveSquare

                    else
                        knightMoveIndicator

                Nothing ->
                    knightMoveIndicator

        Illegal ->
            E.row squareStyles <| [ queenImg, targetSquare ]


startingBox : File -> Rank -> Model -> Element Msg
startingBox file rank { knight, knightSelected, device } =
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
                    St.selectedKnight device

                Nothing ->
                    St.knight device

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
                    (St.queen device)
                    { src = queenFilePath, description = "Queen" }

            else
                E.none

        targetSquare =
            if ( file, rank ) == ( A, One ) then
                E.el (St.targetSquare device) E.none

            else
                E.none
    in
    E.row (St.square boxColor device) <| [ knightImg, queenImg, targetSquare, attackedByQueenSquare ]


legalMoveCircle : Element Msg
legalMoveCircle =
    E.el St.legalMoveCircle E.none


rankLabel : Rank -> Element Msg
rankLabel rank =
    E.el St.rankLabelText (E.text <| rankToString rank)


fileLabelRow : Device -> List File -> List (Element Msg)
fileLabelRow device files =
    [ E.row (St.fileLabelRow device) <| E.el St.blankRankLabel E.none :: List.map (fileLabel device) files ]


fileLabel : Device -> File -> Element Msg
fileLabel device file =
    E.el (St.fileLabelText device) (E.el St.center <| E.text <| fileToString file)


displayTimer : Maybe Int -> Device -> Element msg
displayTimer timer device =
    case timer of
        Nothing ->
            E.el (St.timer device) <| E.text <| showTime 0

        Just secs ->
            E.el (St.timer device) <| E.text <| showTime secs


displayFinalTimer : Maybe Int -> Element Msg
displayFinalTimer timer =
    case timer of
        Nothing ->
            E.none

        Just secs ->
            let
                minutes =
                    getMinutes secs

                seconds =
                    getSeconds secs
            in
            if minutes == 0 then
                E.paragraph []
                    [ E.el [] <| E.text <| String.fromInt seconds
                    , E.text <| "seconds"
                    ]

            else if seconds == 0 then
                E.paragraph []
                    [ E.el [] <| E.text <| String.fromInt minutes
                    , E.text "minutes "
                    ]

            else if minutes == 1 then
                E.paragraph []
                    [ E.el [] <| E.text "1"
                    , E.text "minute "
                    , E.el [] <| E.text <| String.fromInt seconds
                    , E.text <| "seconds"
                    ]

            else
                E.paragraph []
                    [ E.el [] <| E.text <| String.fromInt minutes
                    , E.text "minutes "
                    , E.el [] <| E.text <| String.fromInt seconds
                    , E.text <| "seconds"
                    ]


getSeconds : Int -> Int
getSeconds =
    modBy 60


getMinutes : Int -> Int
getMinutes s =
    s // 60


showTime : Int -> String
showTime secs =
    let
        seconds =
            getSeconds secs

        minutes =
            getMinutes secs

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
            Colors.squareDarkColor

        whiteBg =
            Colors.squareLight
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

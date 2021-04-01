module Main exposing (main)

import Browser exposing (Document)
import Css exposing (..)
import Element as E exposing (Color, Element)
import Element.Events as Ev
import Element.Input as Input
import RankNFiles exposing (..)
import Styles as St


knightFilePath =
    "assets/horse.svg"


queenFilePath =
    "assets/queen2.svg"


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
        , subscriptions = always Sub.none
        }



-------------------------------------------------------
---------------------- UPDATE ---------------------------
-------------------------------------------------------


type alias Model =
    { knight : Knight
    , knightSelected : Maybe LegalMoves
    , currentTarget : ( File, Rank )
    }


initModel : Model
initModel =
    { knight = knightStartingPosition
    , knightSelected = Nothing
    , currentTarget = ( F, Eight )
    }


type Msg
    = ToggleKnightSelect File Rank
    | MoveKnight File Rank
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                ( model, Cmd.none )

            else
                let
                    newTarget =
                        if model.currentTarget == ( file, rank ) then
                            case getNextTarget ( file, rank ) of
                                Just nt ->
                                    nt

                                Nothing ->
                                    model.currentTarget

                        else
                            model.currentTarget
                in
                ( { model
                    | knight = { rank = rank, file = file }
                    , knightSelected = Just <| getLegalMoves file rank
                    , currentTarget = newTarget
                  }
                , Cmd.none
                )

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
            E.row St.layout <|
                [ board model ]
        ]
    }


board : Model -> Element Msg
board model =
    E.column [] <|
        List.map
            (\rank ->
                E.row [] <| List.map (\file -> box file rank model) files
            )
            ranks


box : File -> Rank -> Model -> Element Msg
box file rank { knight, knightSelected, currentTarget } =
    let
        boxColor =
            getBoxColor file rank

        knightClickEvent =
            [ Ev.onClick <| ToggleKnightSelect file rank ]

        knightStyles =
            case knightSelected of
                Just _ ->
                    St.selectedKnight

                Nothing ->
                    St.knight

        knightImg =
            if file == knight.file && rank == knight.rank then
                E.image
                    (knightStyles ++ knightClickEvent)
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
            if ( file, rank ) == currentTarget then
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


legalMoveCircle : Element Msg
legalMoveCircle =
    E.el St.legalMoveCircle E.none


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

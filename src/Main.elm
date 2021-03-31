module Main exposing (main)

import Browser exposing (Document)
import Css exposing (..)
import Html.Styled exposing (Html, div, h1, img, p, text, toUnstyled)
import Html.Styled.Attributes exposing (css, src)
import Html.Styled.Events exposing (onClick)
import RankNFiles exposing (..)


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
    }


initModel : Model
initModel =
    { knight = knightStartingPosition
    , knightSelected = Nothing
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
            ( { model
                | knight = { rank = rank, file = file }
                , knightSelected = Just <| getLegalMoves file rank
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )



-------------------------------------------------------
---------------------- VIEW ---------------------------
-------------------------------------------------------


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
    rgb 232 235 239


squareLight : Color
squareLight =
    rgb 125 135 150


view : Model -> Document Msg
view model =
    let
        mainStyles =
            css
                [ width (px boardWidth)
                , height (px boardWidth)
                , marginLeft auto
                , marginRight auto
                , displayFlex
                , flexWrap wrap
                , boxSizing borderBox
                ]
    in
    { title = "Knighty Knight"
    , body =
        [ toUnstyled <|
            div [ mainStyles ] <|
                board model
        ]
    }


board : Model -> List (Html Msg)
board model =
    List.map (\file -> div [ css [ marginTop (px 10) ] ] (List.map (\rank -> box rank file model) ranks)) files


box : Rank -> File -> Model -> Html Msg
box rank file { knight, knightSelected } =
    let
        centerContent =
            css [ displayFlex, alignItems center, justifyContent center ]

        boxColor =
            getBoxColor rank file

        queenStyles =
            css [ width (px squareWidth), height (px squareWidth) ]

        knightStyles =
            css
                [ width (px squareWidth)
                , height (px squareWidth)
                , cursor pointer
                ]

        knightImg =
            if rank == knight.rank && file == knight.file then
                div
                    [ centerContent
                    , knightStyles
                    ]
                    [ img
                        [ onClick <| ToggleKnightSelect file rank
                        , src "../assets/horse.svg"
                        , css [ width (px knightWidth), height (px knightWidth) ]
                        ]
                        []
                    ]

            else
                div [ css [ display none ] ] []

        queenImg =
            if rank == Five && file == D then
                div
                    [ centerContent
                    , queenStyles
                    ]
                    [ img
                        [ src "../assets/queen2.svg"
                        , css
                            [ width (px queenWidth)
                            , height (px queenWidth)
                            ]
                        ]
                        []
                    ]

            else
                div [ css [ display none ] ] []

        isLegalMove =
            case knightSelected of
                Nothing ->
                    False

                Just legalMoves ->
                    List.member ( file, rank ) legalMoves

        legalMoveIndicator =
            if isLegalMove then
                legalMoveCircle

            else
                emptyDiv

        knightMoveHandler =
            if isLegalMove then
                onClick <| MoveKnight file rank

            else
                onClick <| NoOp

        cursorStyle =
            if isLegalMove then
                cursor pointer

            else
                cursor default
    in
    div
        [ knightMoveHandler
        , css
            [ position relative
            , width (px squareWidth)
            , height (px squareWidth)
            , borderWidth (px 1)
            , borderStyle solid
            , boxColor
            , cursorStyle
            ]
        ]
        [ knightImg, queenImg, legalMoveIndicator ]


legalMoveCircle : Html Msg
legalMoveCircle =
    div
        [ css
            [ position absolute
            , top (px 0)
            , left (px 0)
            , right (px 0)
            , bottom (px 0)
            , margin auto
            , width (px legalMoveCircleWidth)
            , height (px legalMoveCircleWidth)
            , backgroundColor (rgba 0 0 0 0.4)
            , borderRadius (pc 1.0)
            , borderWidth (px 1)
            , borderColor (rgba 0 0 0 0.7)
            ]
        ]
        []


getBoxColor : Rank -> File -> Style
getBoxColor r f =
    let
        blackBg =
            backgroundColor squareDarkColor

        whiteBg =
            backgroundColor squareLight
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


emptyDiv : Html Msg
emptyDiv =
    div [ css [ display none ] ] []

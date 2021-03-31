module Main exposing (main)

import Browser exposing (Document)
import Css exposing (..)
import Element as E exposing (Element)
import Html.Styled exposing (Html, div, h1, img, p, text, toUnstyled)
import Html.Styled.Attributes exposing (css, src)
import Html.Styled.Events exposing (onClick)
import RankNFiles exposing (..)
import Styles as St


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


view : Model -> Document Msg
view model =
    let
        mainStyles =
            css
                [ width (px St.boardWidth)
                , height (px St.boardWidth)
                , marginLeft auto
                , marginRight auto
                , displayFlex
                , flexWrap wrap
                , boxSizing borderBox
                ]
    in
    { title = "Knighty Knight"
    , body =
        [ E.layout [] <|
            E.row [] <|
                [ board2 model ]
        ]
    }


board2 : Model -> Element Msg
board2 model =
    E.column [] <|
        List.map
            (\rank ->
                E.row [] <| List.map (\file -> box2 file rank model) files
            )
            ranks


box2 : File -> Rank -> Model -> Element Msg
box2 file rank { knight, knightSelected } =
    E.el St.square <| E.text <| fileToString file ++ (String.fromInt << rankToInt) rank


board : Model -> List (Html Msg)
board model =
    List.map (\file -> div [ css [ marginTop (px 10) ] ] (List.map (\rank -> box rank file model) ranks)) files


box : Rank -> File -> Model -> Html Msg
box rank file { knight, knightSelected } =
    let
        centerContent =
            css [ displayFlex, alignItems center, justifyContent center ]

        -- boxColor =
        --     getBoxColor rank file
        queenStyles =
            css [ width (px St.squareWidth), height (px St.squareWidth) ]

        knightStyles =
            css
                [ width (px St.squareWidth)
                , height (px St.squareWidth)
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
                        , css [ width (px St.knightWidth), height (px St.knightWidth) ]
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
                            [ width (px St.queenWidth)
                            , height (px St.queenWidth)
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

        isQueenMove =
            List.member ( file, rank ) queenMoves

        queenMoveSquare =
            if isQueenMove then
                redSquare

            else
                emptyDiv

        redSquare =
            div
                [ css
                    [ width (px St.squareWidth)
                    , height (px St.squareWidth)
                    , opacity (num 0.8)
                    ]
                ]
                []
    in
    div
        [ knightMoveHandler
        , css
            [ position relative
            , width (px St.squareWidth)
            , height (px St.squareWidth)
            , borderWidth (px 1)
            , borderStyle solid
            , borderColor (rgba 0 0 0 0.7)
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
            , width (px St.legalMoveCircleWidth)
            , height (px St.legalMoveCircleWidth)
            , backgroundColor (rgba 0 0 0 0.4)
            , borderRadius (pc 1.0)
            , borderWidth (px 1)
            , borderColor (rgba 0 0 0 0.7)
            ]
        ]
        []



-- getBoxColor : Rank -> File -> Style
-- getBoxColor r f =
--     let
--         blackBg =
--             backgroundColor St.squareDarkColor
--         whiteBg =
--             backgroundColor St.squareLight
--     in
--     case ( f, modBy 2 <| rankToInt r ) of
--         ( A, 0 ) ->
--             blackBg
--         ( A, 1 ) ->
--             whiteBg
--         ( B, 0 ) ->
--             whiteBg
--         ( B, 1 ) ->
--             blackBg
--         ( C, 0 ) ->
--             blackBg
--         ( C, 1 ) ->
--             whiteBg
--         ( D, 0 ) ->
--             whiteBg
--         ( D, 1 ) ->
--             blackBg
--         ( E, 0 ) ->
--             blackBg
--         ( E, 1 ) ->
--             whiteBg
--         ( F, 0 ) ->
--             whiteBg
--         ( F, 1 ) ->
--             blackBg
--         ( G, 0 ) ->
--             blackBg
--         ( G, 1 ) ->
--             whiteBg
--         ( H, 0 ) ->
--             whiteBg
--         ( H, 1 ) ->
--             blackBg
--         _ ->
--             blackBg


emptyDiv : Html Msg
emptyDiv =
    div [ css [ display none ] ] []

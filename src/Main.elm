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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleKnightSelect file rank ->
            case model.knightSelected of
                Nothing ->
                    ( { model | knightSelected = Just <| getLegalMoves file rank }, Cmd.none )

                Just _ ->
                    ( { model | knightSelected = Nothing }, Cmd.none )



-------------------------------------------------------
---------------------- VIEW ---------------------------
-------------------------------------------------------


view : Model -> Document Msg
view model =
    let
        mainStyles =
            css
                [ width (px 850)
                , height (px 850)
                , borderWidth (px 1)
                , borderStyle solid
                , marginLeft auto
                , marginRight auto
                , displayFlex
                , flexWrap wrap
                , boxSizing borderBox
                ]
    in
    { title = "Knight Knight"
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
        boxColor =
            getBoxColor rank file

        knightStyles =
            css
                [ width (px 100)
                , height (px 100)
                , displayFlex
                , alignItems center
                , justifyContent center
                ]

        knightImg =
            if rank == knight.rank && file == knight.file then
                div [ knightStyles ] [ img [ onClick <| ToggleKnightSelect file rank, src "../assets/horse.svg", css [ width (px 70), height (px 70) ] ] [] ]

            else
                div [ css [ display none ] ] []

        queenImg =
            if rank == Five && file == D then
                div [ knightStyles ] [ img [ src "../assets/queen2.svg", css [ width (px 90), height (px 90) ] ] [] ]

            else
                div [ css [ display none ] ] []

        legalMove =
            case knightSelected of
                Nothing ->
                    div [ css [ display none ] ] []

                Just legalMoves ->
                    if List.member ( file, rank ) legalMoves then
                        legalMoveCircle

                    else
                        div [ css [ display none ] ] []
    in
    div [ css [ width (px 100), height (px 100), borderWidth (px 1), borderStyle solid, boxColor ], knightStyles ]
        [ knightImg, queenImg, legalMove ]


legalMoveCircle : Html msg
legalMoveCircle =
    div
        [ css
            [ width (px 20), height (px 20), backgroundColor (rgba 0 0 0 0.4), borderRadius (pc 1.0), borderWidth (px 1), borderColor (rgba 0 0 0 0.7) ]
        ]
        []


getBoxColor : Rank -> File -> Style
getBoxColor r f =
    let
        blackBg =
            backgroundColor (rgb 232 235 239)

        whiteBg =
            backgroundColor (rgb 125 135 150)
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

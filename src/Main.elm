module Main exposing (main)

import Browser exposing (Document)
import Css exposing (..)
import Html.Styled exposing (Html, div, h1, img, p, text, toUnstyled)
import Html.Styled.Attributes exposing (css, src)


type alias Model =
    { knight : Knight }


initModel : Model
initModel =
    { knight = knightStartingPosition }


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


type alias Knight =
    { rank : Rank
    , file : File
    }


knightStartingPosition : Knight
knightStartingPosition =
    { rank = Eight
    , file = H
    }


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


type Msg
    = None


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


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
board { knight } =
    List.map (\file -> div [ css [ marginTop (px 10) ] ] (List.map (\rank -> box rank file knight) ranks)) files


box : Rank -> File -> Knight -> Html Msg
box rank file knight =
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
                div [ knightStyles ] [ img [ src "../assets/horse.svg", css [ width (px 90), height (px 90) ] ] [] ]

            else
                div [] []
    in
    div [ css [ width (px 100), height (px 100), borderWidth (px 1), borderStyle solid, boxColor ] ] [ knightImg ]


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

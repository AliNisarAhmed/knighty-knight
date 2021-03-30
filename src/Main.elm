module Main exposing (main)

import Browser exposing (Document)
import Css exposing (..)
import Html.Styled exposing (div, h1, text, toUnstyled)
import Html.Styled.Attributes exposing (css)


type alias Model =
    Int


initModel : Model
initModel =
    0


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
                [ width (px 800)
                , height (px 800)
                , borderWidth (px 1)
                , borderStyle solid
                , marginLeft auto
                , marginRight auto
                ]
    in
    { title = "Knight Knight"
    , body =
        [ toUnstyled <|
            div [ mainStyles ]
                [ h1 [] [ text "Hello World" ] ]
        ]
    }

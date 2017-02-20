module Main exposing (main)

import Html exposing (..)


main : Program Never Model Msg
main =
    beginnerProgram
        { model = model
        , view = view
        , update = update
        }


type alias Model =
    ()


model : Model
model =
    ()


type Msg
    = None


update : Msg -> Model -> Model
update msg model =
    model


view : Model -> Html Msg
view model =
    text "Hello World"

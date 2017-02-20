module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (type_, value)
import Html.Events
import Json.Decode
import Encounter exposing (..)


main : Program Never Model Msg
main =
    beginnerProgram
        { model = Model 0 0 Nothing
        , view = view
        , update = update
        }


type alias Model =
    { party : Int
    , monster : Int
    , difficulty : Maybe Difficulty
    }


type Msg
    = ChangeMonster String
    | ChangeParty String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeMonster value ->
            case String.toInt value of
                Ok value ->
                    { model
                    | monster = value
                    , difficulty =
                        getDifficulty [model.party] [value]
                    }
                Err _ ->
                    model

        ChangeParty value ->
            case String.toInt value of
                Ok value ->
                    { model
                    | party = value
                    , difficulty =
                        getDifficulty [value] [model.monster]
                    }
                Err _ ->
                    model


view : Model -> Html Msg
view model =
    section []
        [ h1 [] [ text "Encounter Calculator" ]
        , form []
            [ viewParty <| toString model.party
            , viewMonsters <| toString model.monster
            , viewDifficulty model.difficulty
            ]
        ]


viewParty party =
    div []
        [ h3 [] [ text "Party" ]
        , label []
            [ input
                [ type_ "number"
                , onChange ChangeParty
                , value party
                ]
                []
            ]
        ]
        -- TODO: dynamic party inputs
        -- TODO: show XP thresholds


viewMonsters monster =
    div []
        [ h3 [] [ text "Monsters" ]
        , label []
            [ input
                [ type_ "number"
                , onChange ChangeMonster
                , value monster
                ]
                []
            ]
        ]
        -- TODO: dynamic monster inputs
        -- TODO: show XP values (per monster/per encounter)


viewDifficulty difficulty =
    let
        difficultyString =
            case difficulty of
                Just x  -> difficultyToString x
                Nothing -> ""
    in
        div []
            [ text <| "Difficulty: " ++ difficultyString
            ]


onChange : (String -> Msg) -> Attribute Msg
onChange tagger =
    Html.Events.on "change" <|
        Json.Decode.map tagger <|
            Json.Decode.at [ "target" , "value" ] Json.Decode.string


difficultyToString : Difficulty -> String
difficultyToString difficulty =
    case difficulty of
        Easy   -> "Easy"
        Medium -> "Medium"
        Hard   -> "Hard"
        Deadly -> "Deadly"

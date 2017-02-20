module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (type_, value)
import Html.Events
import Json.Decode


main : Program Never Model Msg
main =
    beginnerProgram
        { model = Model "" ""
        , view = view
        , update = update
        }


type alias Model =
    { party : String
    , monster : String
    }


type Msg
    = ChangeMonster String
    | ChangeParty String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeMonster value ->
            { model | monster = value }

        ChangeParty value ->
            { model | party = value }


view : Model -> Html Msg
view model =
    section []
        [ h1 [] [ text "Encounter Calculator" ]
        , form []
            [ viewParty model.party
            , viewMonsters model.monster
            , text "Difficulty"
            ]
        ]


viewParty party =
    div []
        [ h3 [] [ text "Party" ]
        , label []
            [ input
                [ --type_ "number"
                 onChange ChangeParty
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
                [ --type_ "number"
                 onChange ChangeMonster
                , value monster
                ]
                []
            ]
        ]
        -- TODO: dynamic monster inputs
        -- TODO: show XP values (per monster/per encounter)


onChange : (String -> Msg) -> Attribute Msg
onChange tagger =
    Html.Events.on "change" <|
        Json.Decode.map tagger <|
            Json.Decode.at [ "target" , "value" ] Json.Decode.string

module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (type_, value)
import Html.Events
import Json.Decode
import Encounter exposing (..)


main : Program Never Model Msg
main =
    beginnerProgram
        { model = init
        , view = view
        , update = update
        }


type alias Model =
    { party : Party
    , monsters : Monsters
    , difficulty : Maybe Difficulty
    }


type alias Party =
    { first : (String, Int)
    , rest : List (String, Int)
    }


type alias Monsters =
    { first : (String, Int)
    , rest : List (String, Int)
    }


type Msg
    = ChangeMonster String
    | ChangeParty String


init : Model
init =
    Model
        (Party ("", 0) [])
        (Monsters ("", 0) [])
        Nothing


update : Msg -> Model -> Model
update msg model =
    let
        monsters = model.monsters
        party = model.party
    in
    case msg of
        ChangeMonster value ->
            case String.toInt value of
                Ok value ->
                    { model
                    | monsters = { monsters | first = ("", value) } 
                    , difficulty =
                        getDifficulty [Tuple.second party.first] [value]
                    }
                Err _ ->
                    model

        ChangeParty value ->
            case String.toInt value of
                Ok value ->
                    { model
                    | party = { party | first = ("", value) }
                    , difficulty =
                        getDifficulty [value] [Tuple.second monsters.first]
                    }
                Err _ ->
                    model


view : Model -> Html Msg
view model =
    section []
        [ h1 [] [ text "Encounter Calculator" ]
        , form []
            [ viewDifficulty model.difficulty
            , viewParty model.party
            , viewMonsters model.monsters
            ]
        ]


viewDifficulty difficulty =
    let
        difficultyString =
            case difficulty of
                Just x  -> difficultyToString x ++ " encounter"
                Nothing -> "Invalid encounter state"
    in
        div []
            [ h2 [] [ text difficultyString ]
            ]


viewParty : Party -> Html Msg
viewParty party =
    div []
        [ h3 [] [ text "Party" ]
        , ul [] <|
            viewEncounterMemberLi ChangeParty party.first ::
            List.map (viewEncounterMemberLi ChangeParty) party.rest
            ]
        -- TODO: dynamic party inputs
        -- TODO: show XP thresholds


viewMonsters : Monsters -> Html Msg
viewMonsters monsters =
    div []
        [ h3 [] [ text "Monsters" ]
        , ul [] <|
            viewEncounterMemberLi ChangeMonster monsters.first ::
            List.map (viewEncounterMemberLi ChangeMonster) monsters.rest
        ]
        -- TODO: dynamic monster inputs
        -- TODO: show XP values (per monster/per encounter)


viewEncounterMemberLi : (String -> Msg) -> (String, Int) -> Html Msg
viewEncounterMemberLi msg (name, value) =
    li []
        [ label []
            [ input
                [ type_ "number"
                , onChange msg
                , Html.Attributes.value <| toString value
                ]
                []
            ]
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

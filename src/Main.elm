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
    { party : Side
    , monsters : Side
    , difficulty : Maybe Difficulty
    }


type alias Side =
    { first : (String, Int)
    , rest : List (String, Int)
    }


type Msg
    = ChangeMonster String String
    | ChangeParty String String


init : Model
init =
    Model
        (Side ("", 0) [])
        (Side ("", 0) [])
        Nothing


update : Msg -> Model -> Model
update msg model =
    let
        monsters = model.monsters
        party = model.party
    in
        case msg of
            ChangeMonster id xp ->
                case String.toInt xp of
                    Ok xp ->
                        { model
                        | monsters = updateMember monsters id xp
                        , difficulty =
                            getDifficulty [Tuple.second party.first] [xp]
                        }

                    Err _ ->
                        model

            ChangeParty id level ->
                case String.toInt level of
                    Ok level ->
                        { model
                        | party = updateMember party id level
                        , difficulty =
                            getDifficulty [level] [Tuple.second monsters.first]
                        }

                    Err _ ->
                        model


updateMember : Side -> String -> Int -> Side
updateMember side key newValue =
    let
        updateRest (thisKey, thisValue) =
            if thisKey == key then
                (key, newValue)
            else
                (thisKey, thisValue)

    in
        if (Tuple.first side.first) == key then
            { side | first = (key, newValue) }
        else
            { side | rest = List.map updateRest side.rest }


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


viewParty : Side -> Html Msg
viewParty party =
    div []
        [ h3 [] [ text "Party" ]
        , ul [] <|
            viewSideMemberLi ChangeParty party.first ::
            List.map (viewSideMemberLi ChangeParty) party.rest
            ]
        -- TODO: dynamic party inputs
        -- TODO: show XP thresholds


viewMonsters : Side -> Html Msg
viewMonsters monsters =
    div []
        [ h3 [] [ text "Monsters" ]
        , ul [] <|
            viewSideMemberLi ChangeMonster monsters.first ::
            List.map (viewSideMemberLi ChangeMonster) monsters.rest
        ]
        -- TODO: dynamic monster inputs
        -- TODO: show XP values (per monster/per encounter)


viewSideMemberLi : (String -> String -> Msg) -> (String, Int) -> Html Msg
viewSideMemberLi tagger (id, value) =
    li []
        [ label []
            [ input
                [ type_ "number"
                , onChange <| tagger id
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

module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick)
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
    }


type alias Side =
    { first : ( String, Int )
    , rest : List ( String, Int )
    }


type Msg
    = AddMonster
    | AddPartyMember
    | ChangeMonster String String
    | ChangeParty String String
    | RemoveMonster
    | RemovePartyMember


init : Model
init =
    let
        party =
            case List.map defaultPartyMember <| List.range 0 3 of
                first :: rest ->
                    Side (first) (rest)

                [] ->
                    Side (defaultPartyMember 0) []

        monsters =
            case List.map defaultMonster <| List.range 0 3 of
                first :: rest ->
                    Side (first) (rest)

                [] ->
                    Side (defaultMonster 0) []
    in
        Model party monsters


defaultPartyMember : Int -> ( String, Int )
defaultPartyMember nextId =
    ( "party-" ++ toString nextId, 0 )


defaultMonster : Int -> ( String, Int )
defaultMonster nextId =
    ( "monster-" ++ toString nextId, 0 )


update : Msg -> Model -> Model
update msg model =
    let
        monsters =
            model.monsters

        party =
            model.party
    in
        case msg of
            AddMonster ->
                let
                    monsters =
                        model.monsters

                    nextId =
                        List.length model.monsters.rest + 1
                in
                    { model
                        | monsters =
                            { monsters
                                | rest =
                                    monsters.rest
                                        ++ [ defaultMonster nextId ]
                            }
                    }

            AddPartyMember ->
                let
                    party =
                        model.party

                    nextId =
                        List.length model.party.rest + 1
                in
                    { model
                        | party =
                            { party
                                | rest =
                                    party.rest
                                        ++ [ defaultPartyMember nextId ]
                            }
                    }

            ChangeMonster id xp ->
                case String.toInt xp of
                    Ok xp ->
                        { model
                            | monsters = updateMember monsters id xp
                        }

                    Err _ ->
                        model

            ChangeParty id level ->
                case String.toInt level of
                    Ok level ->
                        { model
                            | party = updateMember party id level
                        }

                    Err _ ->
                        model

            RemoveMonster ->
                let
                    monsters =
                        model.monsters
                in
                    { model
                        | monsters =
                            { monsters
                                | rest =
                                    List.take
                                        (List.length monsters.rest - 1)
                                        monsters.rest
                            }
                    }

            RemovePartyMember ->
                let
                    party =
                        model.party
                in
                    { model
                        | party =
                            { party
                                | rest =
                                    List.take
                                        (List.length party.rest - 1)
                                        party.rest
                            }
                    }


updateMember : Side -> String -> Int -> Side
updateMember side key newValue =
    let
        updateRest ( thisKey, thisValue ) =
            if thisKey == key then
                ( key, newValue )
            else
                ( thisKey, thisValue )
    in
        if (Tuple.first side.first) == key then
            { side | first = ( key, newValue ) }
        else
            { side | rest = List.map updateRest side.rest }


view : Model -> Html Msg
view model =
    section []
        [ h1 [] [ text "Encounter Calculator" ]
        , form []
            [ viewDifficulty model
            , viewParty model.party
            , viewMonsters model.monsters
            ]
        ]


viewDifficulty : Model -> Html Msg
viewDifficulty model =
    let
        levels =
            [ model.party.first ]
                ++ model.party.rest
                |> List.unzip
                |> Tuple.second

        xps =
            [ model.monsters.first ]
                ++ model.monsters.rest
                |> List.unzip
                |> Tuple.second

        difficultyString difficulty =
            case difficulty of
                Just x ->
                    difficultyToString x ++ " encounter"

                Nothing ->
                    "Invalid encounter state"
    in
        div []
            [ h2 [] [ text <| difficultyString <| getDifficulty levels xps ]
            ]


viewParty : Side -> Html Msg
viewParty party =
    div []
        [ h3 [] [ text "Party" ]
        , ul [] <|
            viewSideMemberLi ChangeParty party.first
                :: List.map (viewSideMemberLi ChangeParty) party.rest
        , button [ type_ "button", onClick AddPartyMember ] [ text "Add" ]
        , button [ type_ "button", onClick RemovePartyMember ] [ text "Remove" ]
        ]



-- TODO: show XP thresholds


viewMonsters : Side -> Html Msg
viewMonsters monsters =
    div []
        [ h3 [] [ text "Monsters" ]
        , ul [] <|
            viewSideMemberLi ChangeMonster monsters.first
                :: List.map (viewSideMemberLi ChangeMonster) monsters.rest
        , button [ type_ "button", onClick AddMonster ] [ text "Add" ]
        , button [ type_ "button", onClick RemoveMonster ] [ text "Remove" ]
        ]



-- TODO: show XP values (per monster/per encounter)


viewSideMemberLi : (String -> String -> Msg) -> ( String, Int ) -> Html Msg
viewSideMemberLi tagger ( id, value ) =
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
            Json.Decode.at [ "target", "value" ] Json.Decode.string


difficultyToString : Difficulty -> String
difficultyToString difficulty =
    case difficulty of
        Easy ->
            "Easy"

        Medium ->
            "Medium"

        Hard ->
            "Hard"

        Deadly ->
            "Deadly"

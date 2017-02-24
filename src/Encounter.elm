module Encounter exposing (..)

{-| This module contains functions used to build a combat encounter.
-}


type Difficulty
    = Easy
    | Medium
    | Hard
    | Deadly


type PartySize
    = Standard
    | Small
    | Large


{-| Returns the `Difficulty` of the encounter using the closest XP Threshold
that is equal or less than total monster XP.
-}
getDifficulty : List Int -> List Int -> Maybe Difficulty
getDifficulty partyLevels monsterXpValues =
    let
        partySize =
            getPartySize <| List.length partyLevels
    in
        case partySize of
            Nothing ->
                Nothing

            Just size ->
                let
                    monstersFinalXp =
                        monstersXp size monsterXpValues

                    difficulties =
                        [Deadly, Hard, Medium, Easy]

                    flipPartyThreshold : Difficulty -> Int
                    flipPartyThreshold =
                        (flip partyThreshold) partyLevels

                    diffThresholdPairs : List ( Difficulty, Int )
                    diffThresholdPairs =
                        difficulties
                            |> List.map flipPartyThreshold
                            |> List.map2 (,) difficulties

                    iterate : List ( Difficulty, Int ) -> Maybe Difficulty
                    iterate list =
                        case list of
                            [] -> Nothing

                            x::xs ->
                                let
                                    (diff, xp) = x
                                in
                                    if
                                        xp <= monstersFinalXp
                                        || diff == Easy
                                    then
                                        Just diff
                                    else
                                        iterate xs
                in
                    iterate diffThresholdPairs


getPartySize : Int -> Maybe PartySize
getPartySize count =
    if count > 5 then
        Just Large
    else if count >= 3 then
        Just Standard
    else if count > 0 then
        Just Small
    else
        Nothing


monstersXp : PartySize -> List Int -> Int
monstersXp partySize monsters =
    let
        validMonsters =
            List.filter ((>) 0) monsters

        coefficient = multiplier partySize <| List.length validMonsters
        sum = toFloat <| List.sum validMonsters
    in
        floor <| sum * coefficient


partyThreshold : Difficulty -> List Int -> Int
partyThreshold difficulty party =
    party
        |> List.map (Maybe.withDefault 0 << threshold difficulty)
        |> List.sum


{-| Return the XP Threshold by Difficulty for a given character level.
-}
threshold : Difficulty -> Int -> Maybe Int
threshold difficulty level =
    case difficulty of
        Easy ->
            case level of
                1  -> Just 25
                2  -> Just 50
                3  -> Just 75
                4  -> Just 125
                5  -> Just 250
                6  -> Just 300
                7  -> Just 350
                8  -> Just 450
                9  -> Just 550
                10 -> Just 600
                11 -> Just 800
                12 -> Just 1000
                13 -> Just 1100
                14 -> Just 1250
                15 -> Just 1400
                16 -> Just 1600
                17 -> Just 2000
                18 -> Just 2100
                19 -> Just 2400
                20 -> Just 2800
                _  -> Nothing

        Medium ->
            case level of
                1  -> Just 50
                2  -> Just 100
                3  -> Just 150
                4  -> Just 250
                5  -> Just 500
                6  -> Just 600
                7  -> Just 750
                8  -> Just 900
                9  -> Just 1100
                10 -> Just 1200
                11 -> Just 1600
                12 -> Just 2000
                13 -> Just 2200
                14 -> Just 2500
                15 -> Just 2800
                16 -> Just 3200
                17 -> Just 3900
                18 -> Just 4200
                19 -> Just 4900
                20 -> Just 5700
                _  -> Nothing

        Hard ->
            case level of
                1  -> Just 75
                2  -> Just 150
                3  -> Just 225
                4  -> Just 375
                5  -> Just 750
                6  -> Just 900
                7  -> Just 1100
                8  -> Just 1400
                9  -> Just 1600
                10 -> Just 1900
                11 -> Just 2400
                12 -> Just 3000
                13 -> Just 3400
                14 -> Just 3800
                15 -> Just 4300
                16 -> Just 4800
                17 -> Just 5900
                18 -> Just 6300
                19 -> Just 7300
                20 -> Just 8500
                _  -> Nothing

        Deadly ->
            case level of
                1  -> Just 100
                2  -> Just 200
                3  -> Just 400
                4  -> Just 500
                5  -> Just 1100
                6  -> Just 1400
                7  -> Just 1700
                8  -> Just 2100
                9  -> Just 2400
                10 -> Just 2800
                11 -> Just 3600
                12 -> Just 4500
                13 -> Just 5100
                14 -> Just 5700
                15 -> Just 6400
                16 -> Just 7200
                17 -> Just 8800
                18 -> Just 9500
                19 -> Just 10900
                20 -> Just 12700
                _  -> Nothing


{-| Return the XP multiplier by party size for a given amount of monsters.

Note: this function does not consider Challenge Rating.
-}
multiplier : PartySize -> Int -> Float
multiplier size monsters =
    if monsters <= 1 then
        case size of
            Small    -> 1.5
            Standard -> 1
            Large    -> 0.5
    else if monsters == 2 then
        case size of
            Small    -> 2
            Standard -> 1.5
            Large    -> 1
    else if monsters <= 6 then
        case size of
            Small    -> 2.5
            Standard -> 2
            Large    -> 1.5
    else if monsters <= 10 then
        case size of
            Small    -> 3
            Standard -> 2.5
            Large    -> 2
    else if monsters <= 14 then
        case size of
            Small    -> 4
            Standard -> 3
            Large    -> 2.5
    else -- monsters >= 15
        case size of
            Small    -> 5
            Standard -> 4
            Large    -> 3

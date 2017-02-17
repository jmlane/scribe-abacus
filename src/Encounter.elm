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
                                    if xp <= monstersFinalXp then
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
        coefficient = multiplier partySize <| List.length monsters
        sum = toFloat <| List.sum monsters
    in
        floor <| sum * coefficient


partyThreshold : Difficulty -> List Int -> Int
partyThreshold difficulty party =
    party
        |> List.map (threshold difficulty)
        |> List.sum


{-| Return the XP Threshold by Difficulty for a given character level.
-}
threshold : Difficulty -> Int -> Int
threshold difficulty level =
    case difficulty of
        Easy ->
            case level of
                1  -> 25
                2  -> 50
                3  -> 75
                4  -> 125
                5  -> 250
                6  -> 300
                7  -> 350
                8  -> 450
                9  -> 550
                10 -> 600
                11 -> 800
                12 -> 1000
                13 -> 1100
                14 -> 1250
                15 -> 1400
                16 -> 1600
                17 -> 2000
                18 -> 2100
                19 -> 2400
                20 -> 2800
                _  -> 0

        Medium ->
            case level of
                1  -> 50
                2  -> 100
                3  -> 150
                4  -> 250
                5  -> 500
                6  -> 600
                7  -> 750
                8  -> 900
                9  -> 1100
                10 -> 1200
                11 -> 1600
                12 -> 2000
                13 -> 2200
                14 -> 2500
                15 -> 2800
                16 -> 3200
                17 -> 3900
                18 -> 4200
                19 -> 4900
                20 -> 5700
                _  -> 0

        Hard ->
            case level of
                1  -> 75
                2  -> 150
                3  -> 225
                4  -> 375
                5  -> 750
                6  -> 900
                7  -> 1100
                8  -> 1400
                9  -> 1600
                10 -> 1900
                11 -> 2400
                12 -> 3000
                13 -> 3400
                14 -> 3800
                15 -> 4300
                16 -> 4800
                17 -> 5900
                18 -> 6300
                19 -> 7300
                20 -> 8500
                _  -> 0

        Deadly ->
            case level of
                1  -> 100
                2  -> 200
                3  -> 400
                4  -> 500
                5  -> 1100
                6  -> 1400
                7  -> 1700
                8  -> 2100
                9  -> 2400
                10 -> 2800
                11 -> 3600
                12 -> 4500
                13 -> 5100
                14 -> 5700
                15 -> 6400
                16 -> 7200
                17 -> 8800
                18 -> 9500
                19 -> 10900
                20 -> 12700
                _  -> 0


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

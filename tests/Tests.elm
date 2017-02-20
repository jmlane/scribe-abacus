module Tests exposing (..)

import Test exposing (..)
import Expect
import Encounter exposing (..)


all : Test
all =
    describe "Combat Encounters"
        [ describe "Party Size"
            [ describe "Small party size (1-2)"
                ([1, 2]
                    |> List.map
                        (\number ->
                            test (toString number ++ " members") <|
                                \() ->
                                    Expect.equal (Just Small) <|
                                        getPartySize number
                        )
                )
            , describe "Standard party size (3-5)"
                ([3, 4, 5]
                    |> List.map
                        (\number ->
                            test (toString number ++ " members") <|
                                \() ->
                                    Expect.equal (Just Standard) <|
                                        getPartySize number
                        )
                )
            , describe "Large party size (>5)"
                ([6, 7]
                    |> List.map
                        (\number ->
                            test (toString number ++ " members") <|
                                \() ->
                                    Expect.equal (Just Large) <|
                                        getPartySize number
                        )
                )
            , describe "Invalid party size (<1)"
                ([-1, 0]
                    |> List.map
                        (\number ->
                            test (toString number ++ " members") <|
                                \() ->
                                    Expect.equal Nothing <|
                                        getPartySize number
                        )
                )
            ]
        , describe "XP Thresholds by character level"
            [ describe "Easy encounter difficulty"
                ([ ( 1, 25 )
                 , ( 2, 50 )
                 , ( 3, 75 )
                 , ( 4, 125 )
                 , ( 5, 250 )
                 , ( 6, 300 )
                 , ( 7, 350 )
                 , ( 8, 450 )
                 , ( 9, 550 )
                 , ( 10, 600 )
                 , ( 11, 800 )
                 , ( 12, 1000 )
                 , ( 13, 1100 )
                 , ( 14, 1250 )
                 , ( 15, 1400 )
                 , ( 16, 1600 )
                 , ( 17, 2000 )
                 , ( 18, 2100 )
                 , ( 19, 2400 )
                 , ( 20, 2800 )
                 ]
                    |> List.map
                        (\( level, xp ) ->
                            test ("level " ++ toString level) <|
                                \() ->
                                    Expect.equal xp <|
                                        Encounter.threshold Easy level
                        )
                )
            , describe "Medium encounter difficulty"
                ([ ( 1, 50 )
                 , ( 2, 100 )
                 , ( 3, 150 )
                 , ( 4, 250 )
                 , ( 5, 500 )
                 , ( 6, 600 )
                 , ( 7, 750 )
                 , ( 8, 900 )
                 , ( 9, 1100 )
                 , ( 10, 1200 )
                 , ( 11, 1600 )
                 , ( 12, 2000 )
                 , ( 13, 2200 )
                 , ( 14, 2500 )
                 , ( 15, 2800 )
                 , ( 16, 3200 )
                 , ( 17, 3900 )
                 , ( 18, 4200 )
                 , ( 19, 4900 )
                 , ( 20, 5700 )
                 ]
                    |> List.map
                        (\( level, xp ) ->
                            test ("level " ++ toString level) <|
                                \() ->
                                    Expect.equal xp <|
                                        Encounter.threshold Medium level
                        )
                )
            , describe "Hard encounter difficulty"
                ([ ( 1, 75 )
                 , ( 2, 150 )
                 , ( 3, 225 )
                 , ( 4, 375 )
                 , ( 5, 750 )
                 , ( 6, 900 )
                 , ( 7, 1100 )
                 , ( 8, 1400 )
                 , ( 9, 1600 )
                 , ( 10, 1900 )
                 , ( 11, 2400 )
                 , ( 12, 3000 )
                 , ( 13, 3400 )
                 , ( 14, 3800 )
                 , ( 15, 4300 )
                 , ( 16, 4800 )
                 , ( 17, 5900 )
                 , ( 18, 6300 )
                 , ( 19, 7300 )
                 , ( 20, 8500 )
                 ]
                    |> List.map
                        (\( level, xp ) ->
                            test ("level " ++ toString level) <|
                                \() ->
                                    Expect.equal xp <|
                                        Encounter.threshold Hard level
                        )
                )
            , describe "Deadly encounter difficulty"
                ([ ( 1, 100 )
                 , ( 2, 200 )
                 , ( 3, 400 )
                 , ( 4, 500 )
                 , ( 5, 1100 )
                 , ( 6, 1400 )
                 , ( 7, 1700 )
                 , ( 8, 2100 )
                 , ( 9, 2400 )
                 , ( 10, 2800 )
                 , ( 11, 3600 )
                 , ( 12, 4500 )
                 , ( 13, 5100 )
                 , ( 14, 5700 )
                 , ( 15, 6400 )
                 , ( 16, 7200 )
                 , ( 17, 8800 )
                 , ( 18, 9500 )
                 , ( 19, 10900 )
                 , ( 20, 12700 )
                 ]
                    |> List.map
                        (\( level, xp ) ->
                            test ("level " ++ toString level) <|
                                \() ->
                                    Expect.equal xp <|
                                        Encounter.threshold Deadly level
                        )
                )
            ]
        , describe "Encounter multiplers"
            [ describe "Standard size party (3-5)"
                ([ ( 1, 1 )
                 , ( 2, 1.5 )
                 , ( 3, 2 )
                 , ( 4, 2 )
                 , ( 5, 2 )
                 , ( 6, 2 )
                 , ( 7, 2.5 )
                 , ( 8, 2.5 )
                 , ( 9, 2.5 )
                 , ( 10, 2.5 )
                 , ( 11, 3 )
                 , ( 12, 3 )
                 , ( 13, 3 )
                 , ( 14, 3 )
                 , ( 15, 4 )
                 ]
                    |> List.map
                        (\( monsters, multiplier ) ->
                            test (toString monsters ++ " monsters") <|
                                \() ->
                                    Expect.equal multiplier <|
                                        Encounter.multiplier Standard monsters
                        )
                )
            , describe "Larger than standard (>5)"
                ([ ( 1, 0.5 )
                 , ( 2, 1 )
                 , ( 3, 1.5 )
                 , ( 4, 1.5 )
                 , ( 5, 1.5 )
                 , ( 6, 1.5 )
                 , ( 7, 2 )
                 , ( 8, 2 )
                 , ( 9, 2 )
                 , ( 10, 2 )
                 , ( 11, 2.5 )
                 , ( 12, 2.5 )
                 , ( 13, 2.5 )
                 , ( 14, 2.5 )
                 , ( 15, 3 )
                 ]
                    |> List.map
                        (\( monsters, multiplier ) ->
                            test (toString monsters ++ " monsters") <|
                                \() ->
                                    Expect.equal multiplier <|
                                        Encounter.multiplier Large monsters
                        )
                )
            , describe "Smaller than standard (<3)"
                ([ ( 1, 1.5 )
                 , ( 2, 2 )
                 , ( 3, 2.5 )
                 , ( 4, 2.5 )
                 , ( 5, 2.5 )
                 , ( 6, 2.5 )
                 , ( 7, 3 )
                 , ( 8, 3 )
                 , ( 9, 3 )
                 , ( 10, 3 )
                 , ( 11, 4 )
                 , ( 12, 4 )
                 , ( 13, 4 )
                 , ( 14, 4 )
                 , ( 15, 5 )
                 ]
                    |> List.map
                        (\( monsters, multiplier ) ->
                            test (toString monsters ++ " monsters") <|
                                \() ->
                                    Expect.equal multiplier <|
                                        Encounter.multiplier Small monsters
                        )
                )
            ]
        , describe "Difficulty of Combat Encounter"
            [ describe "Standard party size (3-5)"
                [ test "1 ghoul is Medium difficulty for 4 level 1s" <|
                    -- 1x multiplier: Hard 300 > 200xp >= 200 Medium
                    \() ->
                        Expect.equal (Just Medium) <|
                            getDifficulty [1, 1, 1, 1] [200]

                , test "4 kobolds are Medium difficulty for 4 level 1s" <|
                    -- 2x multiplier: Hard 300 > 2(25xp x 4) >= 200 Medium
                    \() ->
                        Expect.equal (Just Medium) <|
                            getDifficulty [1, 1, 1, 1] [25, 25, 25, 25]
                ]
            , describe "Large party size (>5)"
                [ test "2 harpies are Medium difficulty for 5 level 2s" <|
                    -- 1x multiplier: Hard 750 > 200xp x 2 >= 500 Medium
                    \() ->
                        Expect.equal (Just Medium) <|
                            getDifficulty (List.repeat 5 2) [200, 200]

                , test "10 giant lizards are Medium difficulty for 5 level 2s"
                    <|
                    -- 2x multiplier: Hard 750 > 2(25xp x 10) >= 500 Mediun
                    \() ->
                        Expect.equal (Just Medium) <|
                            getDifficulty (List.repeat 5 2) (List.repeat 10 25)
                ]
            , describe "Small party size (1-2)"
                [ test "2 gnolls are Medium difficulty for 2 level 3s" <|
                    -- 2x multiplier: Hard 450 > 2(100xp x 2) >= 300 Medium
                    \() ->
                        Expect.equal (Just Medium) <|
                            getDifficulty (List.repeat 2 3) (List.repeat 2 100)

                , test "15 rugs of smothering are Deadly for 2 level 20s" <|
                    -- 5x multiplier: 5(450xp x 15) >= 25400 Deadly
                    \() ->
                        Expect.equal (Just Deadly) <|
                            getDifficulty (List.repeat 2 20) (List.repeat 15 450)
                            -- TODO: move suites to distinct files. line >80^

                , test "1 homunculus is Easy for 1 level 1" <|
                    -- 1.5x multiplier: Easy 25 > 1.5(16xp)
                    \() ->
                        Expect.equal (Just Easy) <|
                            getDifficulty [1] [10]
                ]
            ]
        ]

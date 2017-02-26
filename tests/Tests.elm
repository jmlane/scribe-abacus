module Tests exposing (..)

import Test exposing (..)
import Expect
import Encounter exposing (..)
import Fuzz exposing (int, list)


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
                ([ ( 0, Nothing )
                 , ( 1, Just 25 )
                 , ( 2, Just 50 )
                 , ( 3, Just 75 )
                 , ( 4, Just 125 )
                 , ( 5, Just 250 )
                 , ( 6, Just 300 )
                 , ( 7, Just 350 )
                 , ( 8, Just 450 )
                 , ( 9, Just 550 )
                 , ( 10, Just 600 )
                 , ( 11, Just 800 )
                 , ( 12, Just 1000 )
                 , ( 13, Just 1100 )
                 , ( 14, Just 1250 )
                 , ( 15, Just 1400 )
                 , ( 16, Just 1600 )
                 , ( 17, Just 2000 )
                 , ( 18, Just 2100 )
                 , ( 19, Just 2400 )
                 , ( 20, Just 2800 )
                 , ( 21, Nothing )
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
                ([ ( 0, Nothing )
                 , ( 1, Just 50 )
                 , ( 2, Just 100 )
                 , ( 3, Just 150 )
                 , ( 4, Just 250 )
                 , ( 5, Just 500 )
                 , ( 6, Just 600 )
                 , ( 7, Just 750 )
                 , ( 8, Just 900 )
                 , ( 9, Just 1100 )
                 , ( 10, Just 1200 )
                 , ( 11, Just 1600 )
                 , ( 12, Just 2000 )
                 , ( 13, Just 2200 )
                 , ( 14, Just 2500 )
                 , ( 15, Just 2800 )
                 , ( 16, Just 3200 )
                 , ( 17, Just 3900 )
                 , ( 18, Just 4200 )
                 , ( 19, Just 4900 )
                 , ( 20, Just 5700 )
                 , ( 21, Nothing )
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
                ([ ( 0, Nothing )
                 , ( 1, Just 75 )
                 , ( 2, Just 150 )
                 , ( 3, Just 225 )
                 , ( 4, Just 375 )
                 , ( 5, Just 750 )
                 , ( 6, Just 900 )
                 , ( 7, Just 1100 )
                 , ( 8, Just 1400 )
                 , ( 9, Just 1600 )
                 , ( 10, Just 1900 )
                 , ( 11, Just 2400 )
                 , ( 12, Just 3000 )
                 , ( 13, Just 3400 )
                 , ( 14, Just 3800 )
                 , ( 15, Just 4300 )
                 , ( 16, Just 4800 )
                 , ( 17, Just 5900 )
                 , ( 18, Just 6300 )
                 , ( 19, Just 7300 )
                 , ( 20, Just 8500 )
                 , ( 21, Nothing )
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
                ([ ( 0, Nothing )
                 , ( 1, Just 100 )
                 , ( 2, Just 200 )
                 , ( 3, Just 400 )
                 , ( 4, Just 500 )
                 , ( 5, Just 1100 )
                 , ( 6, Just 1400 )
                 , ( 7, Just 1700 )
                 , ( 8, Just 2100 )
                 , ( 9, Just 2400 )
                 , ( 10, Just 2800 )
                 , ( 11, Just 3600 )
                 , ( 12, Just 4500 )
                 , ( 13, Just 5100 )
                 , ( 14, Just 5700 )
                 , ( 15, Just 6400 )
                 , ( 16, Just 7200 )
                 , ( 17, Just 8800 )
                 , ( 18, Just 9500 )
                 , ( 19, Just 10900 )
                 , ( 20, Just 12700 )
                 , ( 21, Nothing )
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
        , describe "Total monsters' XP"
            ([ Small, Standard, Large ]
                |> List.map
                    (\size ->
                        describe (toString size)
                            [ fuzz (list int)
                                "should always be zero or positive"
                              <|
                                \xps ->
                                    Expect.atLeast 0 <|
                                        Encounter.monstersXp size xps
                            ]
                    )
            )
        ]

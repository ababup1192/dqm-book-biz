module Tests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main
    exposing
        ( By(..)
        , Dir(..)
        , HeaderFieldViewModel
        , HeaderViewModel
        , Monster
        , MonsterViewModel
        , Order(..)
        , infinity
        , monster2ViewModel
        , order2HeaderViewModel
        )
import Test exposing (..)


type alias TestCase =
    String


order2HeaderViewModelTest : TestCase -> Order -> HeaderViewModel -> Test
order2HeaderViewModelTest testCase order headerViewModel =
    test testCase <|
        \_ ->
            let
                actual =
                    order2HeaderViewModel order

                expected =
                    headerViewModel
            in
            Expect.equal actual expected


suite : Test
suite =
    describe "The Main module"
        [ describe "mosnter2ViewModel"
            -- Nest as many descriptions as you like.
            [ test "スライムはViewModelになった！" <|
                \_ ->
                    let
                        actual =
                            monster2ViewModel <| Monster "スライム" 8 0 9 4

                        expected =
                            MonsterViewModel "スライム" "8" "0" "9" "4"
                    in
                    Expect.equal actual expected
            , test "おおがらすはViewModelになった！" <|
                \_ ->
                    let
                        actual =
                            monster2ViewModel <| Monster "おおがらす" 9 0 10 6

                        expected =
                            MonsterViewModel "おおがらす" "9" "0" "10" "6"
                    in
                    Expect.equal actual expected
            , test "ゾーマはViewModelになった！" <|
                \_ ->
                    let
                        actual =
                            monster2ViewModel <| Monster "ゾーマ" 4700 infinity 360 80

                        expected =
                            MonsterViewModel "ゾーマ" "4700" "∞" "360" "80"
                    in
                    Expect.equal actual expected
            , test "デフォルトのとき、すべての項目が暗く矢印がすべて上を向いている" <|
                \_ ->
                    let
                        actual =
                            order2HeaderViewModel DefaultOrder

                        expected =
                            HeaderViewModel
                                (HeaderFieldViewModel "" "asc")
                                (HeaderFieldViewModel "" "asc")
                                (HeaderFieldViewModel "" "asc")
                                (HeaderFieldViewModel "" "asc")
                    in
                    Expect.equal actual expected
            , order2HeaderViewModelTest
                "HPが昇順のとき、HPの項目のみが明るくなっていて、矢印は、すべて上を向いている"
                (Order Hp Asc)
                (HeaderViewModel
                    (HeaderFieldViewModel "active" "asc")
                    (HeaderFieldViewModel "" "asc")
                    (HeaderFieldViewModel "" "asc")
                    (HeaderFieldViewModel "" "asc")
                )
            , order2HeaderViewModelTest
                "MPが昇順のとき、MPの項目のみが明るくなっていて、矢印は、すべて上を向いている"
                (Order Mp Asc)
                (HeaderViewModel
                    (HeaderFieldViewModel "" "asc")
                    (HeaderFieldViewModel "active" "asc")
                    (HeaderFieldViewModel "" "asc")
                    (HeaderFieldViewModel "" "asc")
                )
            , order2HeaderViewModelTest
                "Attackが昇順のとき、Attackの項目のみが明るくなっていて、矢印は、すべて上を向いている"
                (Order Attack Asc)
                (HeaderViewModel
                    (HeaderFieldViewModel "" "asc")
                    (HeaderFieldViewModel "" "asc")
                    (HeaderFieldViewModel "active" "asc")
                    (HeaderFieldViewModel "" "asc")
                )
            , order2HeaderViewModelTest
                "Agilityが昇順のとき、Agilityの項目のみが明るくなっていて、矢印は、すべて上を向いている"
                (Order Agility Asc)
                (HeaderViewModel
                    (HeaderFieldViewModel "" "asc")
                    (HeaderFieldViewModel "" "asc")
                    (HeaderFieldViewModel "" "asc")
                    (HeaderFieldViewModel "active" "asc")
                )
            ]
        ]

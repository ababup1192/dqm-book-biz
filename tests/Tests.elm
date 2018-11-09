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
        , changeOrder
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
                "HPが降順のとき、HPの項目のみが明るくなっていて、HPの矢印は上を向いていて、HP以外の矢印は上を向いている"
                (Order Hp Dsc)
                (HeaderViewModel
                    (HeaderFieldViewModel "active" "dsc")
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
                "MPが降順のとき、MPの項目のみが明るくなっていて、MPの矢印は上を向いていて、MP以外の矢印は上を向いている"
                (Order Mp Dsc)
                (HeaderViewModel
                    (HeaderFieldViewModel "" "asc")
                    (HeaderFieldViewModel "active" "dsc")
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
            , test "HPが与えられたら、デフォルトの並びのとき、HPが昇順になる。" <|
                \_ ->
                    let
                        actual =
                            DefaultOrder |> changeOrder Hp

                        expected =
                            Order Hp Asc
                    in
                    Expect.equal actual expected
            , test "HPが与えられたら、HPが昇順のとき、HPが降順になる。" <|
                \_ ->
                    let
                        actual =
                            Order Hp Asc |> changeOrder Hp

                        expected =
                            Order Hp Dsc
                    in
                    Expect.equal actual expected
            , test "MPが与えられたら、HPが昇順のとき、MPが昇順になる。" <|
                \_ ->
                    let
                        actual =
                            Order Hp Asc |> changeOrder Mp

                        expected =
                            Order Mp Asc
                    in
                    Expect.equal actual expected
            , test "HPが与えられたら、HPが降順のとき、デフォルトの並びになる。" <|
                \_ ->
                    let
                        actual =
                            Order Hp Dsc |> changeOrder Hp

                        expected =
                            DefaultOrder
                    in
                    Expect.equal actual expected
            , test "Mpが与えられたら、Mpが昇順のとき、Mpが降順になる。" <|
                \_ ->
                    let
                        actual =
                            Order Mp Asc |> changeOrder Mp

                        expected =
                            Order Mp Dsc
                    in
                    Expect.equal actual expected
            , test "Attackが与えられたら、Agilityが降順のとき、Attackが昇順になる。" <|
                \_ ->
                    let
                        actual =
                            Order Agility Dsc |> changeOrder Attack

                        expected =
                            Order Attack Asc
                    in
                    Expect.equal actual expected
            ]
        ]

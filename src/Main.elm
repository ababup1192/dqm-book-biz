module Main exposing
    ( By(..)
    , Dir(..)
    , HeaderFieldViewModel
    , HeaderViewModel
    , Model
    , Monster
    , MonsterViewModel
    , Msg(..)
    , Order(..)
    , changeOrder
    , infinity
    , monster2ViewModel
    , order2HeaderViewModel
    , update
    )

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)



---- MODEL ----


type By
    = Hp
    | Mp
    | Attack
    | Agility


type Dir
    = Asc
    | Dsc


type Order
    = DefaultOrder
    | Order By Dir


type alias Model =
    { monsters : List Monster }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { monsters =
            [ Monster "スライム" 8 0 9 4
            , Monster "おおがらす" 9 0 10 6
            , Monster "ゾーマ" 4700 infinity 360 80
            ]
      }
    , Cmd.none
    )


changeOrder : By -> Order -> Order
changeOrder targetBy order =
    case order of
        DefaultOrder ->
            Order targetBy Asc

        Order by dir ->
            if by /= targetBy then
                Order targetBy Asc

            else if by == targetBy && dir == Dsc then
                DefaultOrder

            else
                Order by Dsc



---- UPDATE ----


infinity : Float
infinity =
    1 / 0


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view { monsters } =
    let
        headerView =
            headerViewModel2HeaderView <|
                HeaderViewModel
                    (HeaderFieldViewModel "" "asc")
                    (HeaderFieldViewModel "" "asc")
                    (HeaderFieldViewModel "" "asc")
                    (HeaderFieldViewModel "" "asc")
    in
    table []
        [ thead []
            [ headerView
            ]
        , tbody []
            (monsters |> List.map monster2ViewModel |> List.map monsterFieldView)
        ]


type alias Monster =
    { name : String, hp : Int, mp : Float, attack : Int, agility : Int }


type alias MonsterViewModel =
    { name : String, hp : String, mp : String, attack : String, agility : String }


monsterFieldView : MonsterViewModel -> Html Msg
monsterFieldView { name, hp, mp, attack, agility } =
    tr []
        [ td [] [ text name ]
        , td [] [ text hp ]
        , td [] [ text mp ]
        , td [] [ text attack ]
        , td [] [ text agility ]
        ]


type alias HeaderFieldViewModel =
    { active : String, dir : String }


type alias HeaderViewModel =
    { hp : HeaderFieldViewModel
    , mp : HeaderFieldViewModel
    , attack : HeaderFieldViewModel
    , agility : HeaderFieldViewModel
    }


defaultOrderHeaderViewModel : HeaderViewModel
defaultOrderHeaderViewModel =
    HeaderViewModel
        (HeaderFieldViewModel "" "asc")
        (HeaderFieldViewModel "" "asc")
        (HeaderFieldViewModel "" "asc")
        (HeaderFieldViewModel "" "asc")


order2HeaderViewModel : Order -> HeaderViewModel
order2HeaderViewModel order =
    case order of
        DefaultOrder ->
            defaultOrderHeaderViewModel

        Order by dir ->
            let
                dirText =
                    case dir of
                        Asc ->
                            "asc"

                        Dsc ->
                            "dsc"
            in
            case by of
                Hp ->
                    { defaultOrderHeaderViewModel
                        | hp =
                            HeaderFieldViewModel "active" dirText
                    }

                Mp ->
                    { defaultOrderHeaderViewModel
                        | mp =
                            HeaderFieldViewModel "active" dirText
                    }

                Attack ->
                    { defaultOrderHeaderViewModel
                        | attack = HeaderFieldViewModel "active" dirText
                    }

                Agility ->
                    { defaultOrderHeaderViewModel
                        | agility = HeaderFieldViewModel "active" dirText
                    }


headerViewModel2HeaderView : HeaderViewModel -> Html Msg
headerViewModel2HeaderView { hp, mp, attack, agility } =
    tr []
        [ th []
            [ text "なまえ" ]
        , th [ class hp.active ]
            [ text "HP"
            , span [ class <| "arrow " ++ hp.dir ] []
            ]
        , th [ class mp.active ]
            [ text "MP"
            , span [ class <| "arrow " ++ mp.dir ] []
            ]
        , th [ class attack.active ]
            [ text "こうげきりょく"
            , span [ class <| "arrow " ++ attack.dir ] []
            ]
        , th [ class agility.active ]
            [ text "すばやさ"
            , span [ class <| "arrow " ++ agility.dir ] []
            ]
        ]


monster2ViewModel : Monster -> MonsterViewModel
monster2ViewModel { name, hp, mp, attack, agility } =
    let
        hpText =
            String.fromInt hp

        mpText =
            if isInfinite mp then
                "∞"

            else
                String.fromFloat mp

        attackText =
            String.fromInt attack

        agilityText =
            String.fromInt agility
    in
    { name = name, hp = hpText, mp = mpText, attack = attackText, agility = agilityText }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

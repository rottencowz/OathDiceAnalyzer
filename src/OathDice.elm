port module OathDice exposing (..)

import Array exposing (..)
import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random exposing (Generator)


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, generateAnalysis initialModel )



-- TODO : get from local storage


initialModel : Model
initialModel =
    let
        initialInputs =
            { attackingDice = 5
            , attackingTroops = 0
            , defendingDice = 5
            , defendingTroops = 0
            }
    in
    { input = initialInputs
    , roll = Nothing
    , index = Nothing
    , backgroundRolls = []
    }


type alias Model =
    { input : Inputs
    , roll : Maybe RollState
    , index : Maybe Int
    , backgroundRolls : List RollState
    }


type alias RollState =
    ( Inputs, List OffenseSides, List DefenseSides )



-- Todo: rename


type alias Inputs =
    { attackingDice : Int
    , attackingTroops : Int
    , defendingDice : Int
    , defendingTroops : Int
    }


type Msg
    = Modify Action Field Side
    | Roll
    | RollGenerated Int
    | AnalysisGenerated (List RollState)


type Field
    = Troops
    | Dice


type Side
    = Offense
    | Defense


type Action
    = Increment
    | Decrement


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Oath Dice Randomizer" ]
        , br [] []
        , viewInputState model
        , br [] []
        , button [ onClick Roll ] [ text "Roll" ]
        , br [] []
        , viewRollResult model
        , br [] []
        ]


viewInputState : Model -> Html Msg
viewInputState model =
    div [ class "mb-2" ]
        [ table []
            [ tr []
                [ th []
                    [ text "Attacker Dice" ]
                , th
                    []
                    [ text "Attacker Troops" ]
                , th
                    []
                    [ text "Defender Dice" ]
                , th
                    []
                    [ text "Defender Troops" ]
                ]
            , tr []
                [ viewField model.input.attackingDice Dice Offense
                , viewField model.input.attackingTroops Troops Offense
                , viewField model.input.defendingDice Dice Defense
                , viewField model.input.defendingTroops Troops Defense
                ]
            ]
        ]


viewField : Int -> Field -> Side -> Html Msg
viewField value field side =
    td
        []
        [ button [ onClick (Modify Increment field side) ] [ text "+" ]
        , text (String.fromInt value)
        , button [ onClick (Modify Decrement field side) ] [ text "-" ]
        ]


viewRollResult : Model -> Html Msg
viewRollResult model =
    case model.roll of
        Just ( inputs, offense, defense ) ->
            div []
                [ viewOffenseRoll offense
                , viewDefenseRoll defense
                ]

        Nothing ->
            br [] []


viewDefenseRoll : List DefenseSides -> Html Msg
viewDefenseRoll defense =
    let
        defenseValues =
            summarizeDefenseRoll defense

        defenseRoll =
            Tuple.first defenseValues * Basics.max 1 (Tuple.second defenseValues)
    in
    div
        []
        [ text "Defense roll value: "
        , defenseRoll
            |> String.fromInt
            |> text
        ]


summarizeDefenseRoll : List DefenseSides -> ( Int, Int )
summarizeDefenseRoll defense =
    List.foldl
        (\roll ( shield, multiplies ) ->
            case roll of
                Shield ->
                    ( shield + 1, multiplies )

                TwoShields ->
                    ( shield + 2, multiplies )

                Multiply ->
                    ( shield, multiplies + 1 )

                Empty ->
                    ( shield, multiplies )
        )
        ( 0, 0 )
        defense


viewOffenseRoll : List OffenseSides -> Html Msg
viewOffenseRoll offense =
    let
        offenseValues =
            summarizeOffenseResults offense

        damage =
            Tuple.first offenseValues
                |> Basics.floor

        selfDamage =
            Tuple.second offenseValues

        offenseString =
            String.fromInt damage ++ " damage, with " ++ String.fromInt selfDamage ++ " immediate loses."
    in
    div []
        [ text "Offense roll value: "
        , offenseString
            |> text
        ]


summarizeOffenseResults : List OffenseSides -> ( Float, Int )
summarizeOffenseResults offense =
    List.foldl
        (\roll ( value, skulls ) ->
            case roll of
                Single ->
                    ( value + 1, skulls )

                Hollow ->
                    ( value + 0.5, skulls )

                Skull ->
                    ( value + 2, skulls + 1 )
        )
        ( 0.0, 0 )
        offense


countGenereratedRolls : Int
countGenereratedRolls =
    10000


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Modify action field side ->
            ( { model | input = updateInput model.input ( action, field, side ) }
            , generateAnalysis model
            )

        Roll ->
            ( model, Random.generate RollGenerated (Random.int 0 (countGenereratedRolls - 1)) )

        RollGenerated index ->
            let
                roll =
                    Array.fromList model.backgroundRolls
                        |> Array.get index

                newModel =
                    { model | roll = roll }
            in
            ( newModel
            , passRollToPlotly
                index
            )

        AnalysisGenerated rolls ->
            ( { model | backgroundRolls = rolls }
            , passBatchedRollsToPlotly
                ( model.input
                , List.map
                    (\( inputs, offense, defense ) ->
                        { summarizedOffense = summarizeOffenseResults offense
                        , summarizedDefense = summarizeDefenseRoll defense
                        }
                    )
                    rolls
                )
            )


generateAnalysis : Model -> Cmd Msg
generateAnalysis model =
    Random.generate AnalysisGenerated (Random.list countGenereratedRolls (rollGenerator model.input))


type alias SummaryResult =
    { summarizedOffense : ( Float, Int )
    , summarizedDefense : ( Int, Int )
    }


port passRollToPlotly : Int -> Cmd msg


port passBatchedRollsToPlotly : ( Inputs, List SummaryResult ) -> Cmd msg


updateInput : Inputs -> ( Action, Field, Side ) -> Inputs
updateInput input desc =
    case desc of
        ( Increment, Dice, Offense ) ->
            { input | attackingDice = inc input.attackingDice }

        ( Decrement, Dice, Offense ) ->
            { input | attackingDice = dec input.attackingDice }

        ( Increment, Dice, Defense ) ->
            { input | defendingDice = inc input.defendingDice }

        ( Decrement, Dice, Defense ) ->
            { input | defendingDice = dec input.defendingDice }

        ( Increment, Troops, Offense ) ->
            { input | attackingTroops = inc input.attackingTroops }

        ( Decrement, Troops, Offense ) ->
            { input | attackingTroops = dec input.attackingTroops }

        ( Increment, Troops, Defense ) ->
            { input | defendingTroops = inc input.defendingTroops }

        ( Decrement, Troops, Defense ) ->
            { input | defendingTroops = dec input.defendingTroops }


inc : Int -> Int
inc i =
    i + 1


dec : Int -> Int
dec i =
    if i <= 0 then
        0

    else
        i - 1


type OffenseSides
    = Skull
    | Single
    | Hollow


type DefenseSides
    = Empty
    | Shield
    | TwoShields
    | Multiply


rollOffenseDice : Int -> Random.Generator (List OffenseSides)
rollOffenseDice count =
    Random.list count
        (Random.weighted
            ( 1, Skull )
            [ ( 2, Single )
            , ( 3, Hollow )
            ]
        )


rollDefenseDice : Int -> Random.Generator (List DefenseSides)
rollDefenseDice count =
    Random.list count
        (Random.weighted
            ( 2, Empty )
            [ ( 2, Shield )
            , ( 1, TwoShields )
            , ( 1, Multiply )
            ]
        )


rollGenerator : Inputs -> Generator RollState
rollGenerator input =
    let
        offense =
            rollOffenseDice input.attackingDice

        defense =
            rollDefenseDice input.defendingDice

        generator =
            Random.map2 (\a b -> ( input, a, b )) offense defense
    in
    generator


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

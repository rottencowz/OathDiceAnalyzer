module OathDice exposing (..)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


init : () -> ( Model, Cmd msg )
init _ =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    let
        initialInputs =
            { attackingDice = 0
            , attackingTroops = 0
            , defendingDice = 0
            , defendingTroops = 0
            }
    in
    { input = initialInputs
    , roll = Nothing
    }


type alias Model =
    { input : Inputs
    , roll : Maybe RollState
    }


type alias RollState =
    {}


type alias Inputs =
    { attackingDice : Int
    , attackingTroops : Int
    , defendingDice : Int
    , defendingTroops : Int
    }


type Msg
    = Modify Action Field Side
    | Roll


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
    text "roll results"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Modify action field side ->
            ( { model | input = updateInput model.input ( action, field, side ) }, Cmd.none )

        Roll ->
            ( { model | roll = Just (generateNewRoll model.input) }, Cmd.none )


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


generateNewRoll : Inputs -> RollState
generateNewRoll inputState =
    Debug.todo "Do this"


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

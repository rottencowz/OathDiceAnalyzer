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


type Location
    = Offense
    | Defense



--  TODO: Action


type Msg
    = IncrementDice Location
    | DecrementDice Location
    | IncrementTroops Location
    | DecrementTroops Location
    | Roll


type Field
    = Troops
    | Dice


type Side
    = Offensive
    | Defensive


type Action
    = Increment Field Side
    | Decrement Field Side


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
        [ viewFieldDice model.input.attackingDice Offense
        , viewFieldDice model.input.defendingDice Defense
        , viewFieldTroops model.input.attackingTroops Offense
        , viewFieldTroops model.input.defendingTroops Defense
        ]



-- TODO: dedupde the view fields


viewFieldTroops : Int -> Location -> Html Msg
viewFieldTroops troops location =
    let
        decrementHandler =
            if troops == 0 then
                [ onClick (DecrementTroops location) ]

            else
                []
    in
    div []
        [ button [ onClick (IncrementTroops location) ] [ text "+" ]
        , text (String.fromInt troops)
        , button decrementHandler [ text "-" ]
        ]


viewFieldDice : Int -> Location -> Html Msg
viewFieldDice dice location =
    let
        decrementHandler =
            if dice == 0 then
                [ onClick (DecrementDice location) ]

            else
                []
    in
    div []
        [ button [ onClick (IncrementDice location) ] [ text "+" ]
        , text (String.fromInt dice)
        , button decrementHandler [ text "-" ]
        ]


viewRollResult : Model -> Html Msg
viewRollResult model =
    text "roll results"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        modifiedInput =
            updateInput msg model.input
    in
    case msg of
        Roll ->
            ( { model | roll = Just (generateNewRoll model.input) }, Cmd.none )

        _ ->
            ( { model | input = modifiedInput }, Cmd.none )


updateInput : Msg -> Inputs -> Inputs
updateInput msg input =
    case msg of
        IncrementDice location ->
            case location of
                Offense ->
                    { input | attackingDice = inc input.attackingDice }

                Defense ->
                    { input | defendingDice = inc input.defendingDice }

        DecrementDice location ->
            case location of
                Offense ->
                    { input | attackingDice = dec input.attackingDice }

                Defense ->
                    { input | defendingDice = dec input.defendingDice }

        IncrementTroops location ->
            case location of
                Offense ->
                    { input | attackingTroops = inc input.attackingTroops }

                Defense ->
                    { input | defendingTroops = inc input.defendingTroops }

        DecrementTroops location ->
            case location of
                Offense ->
                    { input | attackingTroops = dec input.attackingTroops }

                Defense ->
                    { input | defendingTroops = dec input.defendingTroops }

        _ ->
            input


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

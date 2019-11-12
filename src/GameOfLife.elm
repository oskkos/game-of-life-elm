module GameOfLife exposing (main)

import Array exposing (Array)
import Browser
import CellToggle exposing (CellState(..), toggle)
import Html exposing (..)
import Html.Attributes exposing (class, style, type_, value)
import Html.Events exposing (onClick, onInput)
import NextTick exposing (tick)
import Time


rows grid =
    Array.toList (Array.indexedMap (\index row -> tr [] (cells index row)) grid)


cells rowIndex row =
    Array.toList (Array.indexedMap (\cellIndex val -> cell rowIndex cellIndex val) row)


cell : Int -> Int -> CellState -> Html Msg
cell rowIndex cellIndex val =
    let
        cls =
            case val of
                Alive ->
                    "bg-primary"

                Dead ->
                    ""
    in
    td [ class cls, style "width" "20px", style "height" "20px", onClick (Toggle rowIndex cellIndex) ] []


type alias Model =
    { grid : Array (Array CellState), running : Bool, speed: Int }


initialModel =
    { grid = Array.repeat 20 (Array.repeat 30 Dead)
    , running = False
    , speed = 100
    }


view : Model -> Html Msg
view model =
    let
        runningLabel =
            case model.running of
                True ->
                    "Stop"

                False ->
                    "Start"
    in
    div []
        [ table [ class "table table-bordered table-condensed", style "width" "initial" ] [ tbody [] (rows model.grid) ]
        , button [ class "btn btn-primary", onClick Reset ] [ text "Reset" ]
        , button [ class "btn btn-primary", onClick NextTick ] [ text "Next" ]
        , button [ class "btn btn-primary", onClick ToggleRunning ] [ text runningLabel ]
        , span [ style "padding" "5px"] [ text "Speed:"]
        , input [ class "form-control-inline", style "width" "50px", type_ "number", onInput (ToggleSpeed << Maybe.withDefault 100 << String.toInt), value (String.fromInt model.speed)] []
        ]


type Msg
    = Toggle Int Int
    | Reset
    | NextTick
    | ToggleRunning
    | ToggleSpeed Int


update msg model =
    case msg of
        Toggle x y ->
            ( { model | grid = toggle model.grid x y }, Cmd.none )

        Reset ->
            ( initialModel, Cmd.none )

        NextTick ->
            ( { model | grid = tick model.grid }, Cmd.none )

        ToggleRunning ->
            ( { model | running = not model.running }, Cmd.none )
        ToggleSpeed x ->
            ( { model | speed = x }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.running then
        Time.every (toFloat model.speed) (always NextTick)

    else
        Sub.none


main =
    Browser.element
        { init = \() -> ( initialModel, Cmd.none )
        , subscriptions = subscriptions
        , update = update
        , view = view
        }

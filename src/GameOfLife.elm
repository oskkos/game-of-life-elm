module GameOfLife exposing (main)

import Array exposing (Array)
import Browser
import CellToggle exposing (toggle)
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import NextTick exposing (tick)


rows grid =
    Array.toList (Array.indexedMap (\index row -> tr [] (cells index row)) grid)


cells rowIndex row =
    Array.toList (Array.indexedMap (\cellIndex val -> cell rowIndex cellIndex val) row)


cell rowIndex cellIndex val =
    let
        cls =
            case val of
                True ->
                    "bg-primary"

                False ->
                    ""
    in
    td [ class cls, style "width" "20px", style "height" "20px", onClick (Toggle rowIndex cellIndex) ] []


type alias Model =
    { grid : Array (Array Bool) }


initialModel =
    { grid = Array.repeat 20 (Array.repeat 30 False)
    }


view : Model -> Html Msg
view model =
    div []
        [ table [ class "table table-bordered table-condensed", style "width" "initial" ] [ tbody [] (rows model.grid) ]
        , button [ onClick Reset ] [ text "Reset" ]
        , button [ onClick NextTick ] [ text "Next" ]
        ]


type Msg
    = Toggle Int Int
    | Reset
    | NextTick


update msg model =
    case msg of
        Toggle x y ->
            { model | grid = toggle model.grid x y }

        Reset ->
            initialModel

        NextTick ->
            { model | grid = tick model.grid }


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

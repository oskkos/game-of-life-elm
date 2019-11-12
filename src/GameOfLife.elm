module GameOfLife exposing (main)

import Array exposing (Array)
import Browser
import CellToggle exposing (toggle)
import Html exposing (..)
import Html.Attributes exposing (class, style, type_, value)
import Html.Events exposing (onClick, onInput)
import NextTick exposing (tick)
import Time
import Types exposing (CellState(..), Grid, Row)



-- MAIN


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { grid : Grid, running : Bool, speed : Int }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Array.repeat 20 (Array.repeat 30 Dead)) False 100
    , Cmd.none
    )



-- UPDATE


type Msg
    = Toggle Int Int
    | Reset
    | NextTick
    | ToggleRunning
    | ToggleSpeed Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle x y ->
            ( { model | grid = toggle model.grid x y }, Cmd.none )

        Reset ->
            init ()

        NextTick ->
            ( { model | grid = tick model.grid }, Cmd.none )

        ToggleRunning ->
            ( { model | running = not model.running }, Cmd.none )

        ToggleSpeed x ->
            ( { model | speed = x }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.running then
        Time.every (toFloat model.speed) (always NextTick)

    else
        Sub.none



-- VIEW


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
        , span [ style "padding" "5px" ] [ text "Speed:" ]
        , input
            [ class "form-control-inline"
            , style "width" "50px"
            , type_ "number"
            , onInput speedToggle
            , value (String.fromInt model.speed)
            ]
            []
        ]


rows : Grid -> List (Html Msg)
rows grid =
    grid
        |> Array.indexedMap row
        |> Array.toList


row : Int -> Row -> Html Msg
row rowIndex rowData =
    let
        cellInRow =
            cell rowIndex
    in
    tr []
        (rowData
            |> Array.indexedMap cellInRow
            |> Array.toList
        )


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


speedToggle : String -> Msg
speedToggle val =
    ToggleSpeed (Maybe.withDefault 100 (String.toInt val))

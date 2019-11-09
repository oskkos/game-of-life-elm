module GameOfLife exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)


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


mapAt grid x y =
    let
        r =
            getRow grid x

        c =
            getCell r y

        r2 =
            Array.set y (not c) r

        grid2 =
            Array.set x r2 grid
    in
    grid2


getRow arr pos =
    let
        row =
            case Array.get pos arr of
                Just a ->
                    a

                Nothing ->
                    Array.fromList []
    in
    row


getCell arr pos =
    let
        cellValue =
            case Array.get pos arr of
                Just a ->
                    a

                Nothing ->
                    False
    in
    cellValue


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
        ]


type Msg
    = Toggle Int Int
    | Reset


update msg model =
    case msg of
        Toggle x y ->
            { model | grid = mapAt model.grid x y }

        Reset ->
            initialModel


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

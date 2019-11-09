module GameOfLife exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


rows grid =
    List.indexedMap (\index row -> tr [] (cells index row)) grid


cells rowIndex row =
    List.indexedMap (\cellIndex val -> cell rowIndex cellIndex val) row


cell rowIndex cellIndex val =
    let
        cls =
            case val of
                True ->
                    "success"

                False ->
                    "danger"
    in
    td [ class cls ]
        [ button [ class "btn", class ("btn-" ++ cls), onClick { description = "ClickedBox", row = rowIndex, col = cellIndex, val = not val } ] [ text (String.fromInt rowIndex ++ "-" ++ String.fromInt cellIndex) ]
        ]


type alias Model =
    { grid : List (List Bool) }


initialModel =
    { grid = List.repeat 20 (List.repeat 20 False)
    }


view : Model -> Html Msg
view model =
    table [ class "table table-bordered table-condensed" ] [ tbody [] (rows model.grid) ]


type alias Msg =
    { description : String
    , row : Int
    , col : Int
    , val : Bool
    }


update msg model =
    if msg.description == "ClickedBox" then
        { model | grid = List.repeat 20 (List.repeat 20 True) }

    else
        model


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

module GameOfLife exposing (main)

import Html exposing (..)
import Html.Attributes exposing (class, style)


rows rowcount cellcount =
    List.repeat rowcount (tr [] (cells cellcount))


cells cellcount =
    List.repeat cellcount (td [] [])


main =
    let
        rowcount =
            20

        cellcount =
            20

        width =
            String.fromInt (cellcount * 32) ++ "px"

        height =
            String.fromInt (rowcount * 32) ++ "px"
    in
    table [ class "table table-bordered table-condensed", style "width" width, style "height" height ] [ tbody [] (rows rowcount cellcount) ]

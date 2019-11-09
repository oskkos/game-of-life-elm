module GameOfLife exposing (main)

import Html exposing (..)


rows rowcount cellcount =
    List.repeat rowcount (tr [] (cells cellcount))


cells cellcount =
    List.repeat cellcount (td [] [ text "O" ])


main =
    table [] (rows 20 20)

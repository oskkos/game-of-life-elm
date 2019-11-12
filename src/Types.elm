module Types exposing (..)

import Array exposing (Array)


type alias Grid =
    Array Row


type alias Row =
    Array CellState


type CellState
    = Dead
    | Alive

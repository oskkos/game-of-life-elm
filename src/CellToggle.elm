module CellToggle exposing (CellState(..), getRow, toggle)

import Array exposing (Array)
type CellState
    = Dead
    | Alive

getCellState : Array (Array CellState) -> Int -> Int -> CellState
getCellState grid x y =
    getCell (getRow grid x) y


invert : Array (Array CellState) -> Int -> Int -> CellState
invert grid x y =
    let
        state =
            getCellState grid x y
    in
    if (state == Dead) then
        Alive
    else
        Dead

toggle grid x y =
    let
        row =
            Array.set y (invert grid x y) (getRow grid x)
    in
    Array.set x row grid


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


getCell : Array CellState -> Int -> CellState
getCell arr pos =
    let
        cellValue =
            case Array.get pos arr of
                Just a ->
                    a

                Nothing ->
                    Dead
    in
    cellValue

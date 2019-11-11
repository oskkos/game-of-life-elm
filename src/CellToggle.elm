module CellToggle exposing (CellState(..), getRow, isAlive, setCell, toggle)

import Array exposing (Array)


type CellState
    = Dead
    | Alive


isAlive : Array (Array CellState) -> Int -> Int -> Bool
isAlive grid x y =
    case getCell (getRow grid x) y of
        Alive ->
            True

        Dead ->
            False


toggle grid x y =
    if isAlive grid x y then
        setCell grid x y Dead

    else
        setCell grid x y Alive


setCell grid x y state =
    let
        row =
            Array.set y state (getRow grid x)
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

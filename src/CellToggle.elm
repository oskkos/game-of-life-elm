module CellToggle exposing (isAlive, toggle)

import Array exposing (Array)
import Types exposing (CellState(..), Grid, Row)


isAlive : Grid -> Int -> Int -> Bool
isAlive grid row cell =
    case getState grid row cell of
        Alive ->
            True

        Dead ->
            False


toggle : Grid -> Int -> Int -> Grid
toggle grid row cell =
    if isAlive grid row cell then
        setState grid row cell Dead

    else
        setState grid row cell Alive


getRow : Grid -> Int -> Row
getRow grid row =
    case Array.get row grid of
        Just a ->
            a

        Nothing ->
            Array.fromList []


getState : Grid -> Int -> Int -> CellState
getState grid row cell =
    case Array.get cell (getRow grid row) of
        Just a ->
            a

        Nothing ->
            Dead


setState : Grid -> Int -> Int -> CellState -> Grid
setState grid row cell state =
    let
        newRowArray =
            Array.set cell state (getRow grid row)
    in
    Array.set row newRowArray grid

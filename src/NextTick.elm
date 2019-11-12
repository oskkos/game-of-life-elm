module NextTick exposing (tick)

import Array exposing (Array)
import CellToggle exposing (CellState(..), isAlive)
import GameOfLife exposing (Grid)

tick: Grid -> Grid
tick grid =
    let
        tickRowForGrid = tickRow grid (Array.length grid)
    in
    Array.indexedMap tickRowForGrid grid

tickRow: Grid -> Int -> Int -> Array CellState -> Array CellState
tickRow grid rowCount rowNum row =
    let
        tickCellForGrid = tickCell grid rowCount (Array.length row) rowNum
    in

    Array.indexedMap tickCellForGrid row


tickRow: Grid -> Int -> Int -> Int -> Int -> CellState -> CellState
tickCell grid rowCount cellCount rowNum cellNum cellState =
    let
        neighbours =
            countAliveNeighbours grid rowNum cellNum rowCount cellCount
    in
    -- Solu muuttuu eläväksi, jos sen naapureista tasan kolme on eläviä.
    -- Solu pysyy elävänä, jos sen naapureista tasan 2 tai 3 on eläviä. Muuten solu kuolee.
    if (cellState == Dead) && (neighbours == 3) then
        Alive

    else if (cellState == Alive) && (neighbours == 2 || neighbours == 3) then
        Alive

    else
        Dead


countAliveNeighbours grid rowNum cellNum rowCount cellCount =
    List.length
        (List.filter (\a -> a)
            [ isAlive grid (modBy rowCount (rowNum - 1)) (modBy cellCount (cellNum - 1))
            , isAlive grid (modBy rowCount (rowNum - 1)) (modBy cellCount cellNum)
            , isAlive grid (modBy rowCount (rowNum - 1)) (modBy cellCount (cellNum + 1))
            , isAlive grid (modBy rowCount rowNum) (modBy cellCount (cellNum - 1))
            , isAlive grid (modBy rowCount rowNum) (modBy cellCount (cellNum + 1))
            , isAlive grid (modBy rowCount (rowNum + 1)) (modBy cellCount (cellNum - 1))
            , isAlive grid (modBy rowCount (rowNum + 1)) (modBy cellCount cellNum)
            , isAlive grid (modBy rowCount (rowNum + 1)) (modBy cellCount (cellNum + 1))
            ]
        )

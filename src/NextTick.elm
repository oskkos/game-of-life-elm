module NextTick exposing (tick)

import Array exposing (Array)
import CellToggle exposing (CellState(..), isAlive)


type alias Grid =
    Array (Array CellState)


tick : Grid -> Grid
tick grid =
    Array.indexedMap (tickRow grid (Array.length grid)) grid


tickRow : Grid -> Int -> Int -> Array CellState -> Array CellState
tickRow grid rowCount rowNum row =
    Array.indexedMap (tickCell grid rowCount (Array.length row) rowNum) row


tickCell : Grid -> Int -> Int -> Int -> Int -> CellState -> CellState
tickCell grid rowCount cellCount rowNum cellNum cellState =
    let
        neighbours =
            countAliveNeighbours grid rowNum cellNum rowCount cellCount
    in
    -- Any live cell with two or three neighbors survives.
    -- Any dead cell with three live neighbors becomes a live cell.
    -- All other live cells die in the next generation. Similarly, all other dead cells stay dead.
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

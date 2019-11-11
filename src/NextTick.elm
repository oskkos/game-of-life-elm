module NextTick exposing (tick)

import Array
import CellToggle exposing (CellState(..), getRow, isAlive, setCell)


tick grid =
    let
        rows =
            Array.length grid

        cells =
            Array.length (getRow grid 0)
    in
    tickRow grid grid 0 rows cells


tickRow oldgrid newgrid x rows cells =
    if x < rows then
        tickRow oldgrid (tickCell oldgrid newgrid x 0 rows cells) (x + 1) rows cells

    else
        newgrid


tickCell oldgrid newgrid x y rows cells =
    if y < cells then
        let
            newgrid2 =
                setCell newgrid x y (getNewState oldgrid x y rows cells)
        in
        tickCell oldgrid newgrid2 x (y + 1) rows cells

    else
        newgrid


getNewState grid x y rows cells =
    let
        neighbours =
            countAliveNeighbours grid x y rows cells
    in
    -- Solu muuttuu eläväksi, jos sen naapureista tasan kolme on eläviä.
    -- Solu pysyy elävänä, jos sen naapureista tasan 2 tai 3 on eläviä. Muuten solu kuolee.
    if (isAlive grid x y == False) && (neighbours == 3) then
        Alive

    else if isAlive grid x y && (neighbours == 2 || neighbours == 3) then
        Alive

    else
        Dead


countAliveNeighbours grid x y rows cells =
    List.length
        (List.filter (\a -> a)
            [ isAlive grid (modBy rows (x - 1)) (modBy cells (y - 1))
            , isAlive grid (modBy rows (x - 1)) (modBy cells y)
            , isAlive grid (modBy rows (x - 1)) (modBy cells (y + 1))
            , isAlive grid (modBy rows x) (modBy cells (y - 1))
            , isAlive grid (modBy rows x) (modBy cells (y + 1))
            , isAlive grid (modBy rows (x + 1)) (modBy cells (y - 1))
            , isAlive grid (modBy rows (x + 1)) (modBy cells y)
            , isAlive grid (modBy rows (x + 1)) (modBy cells (y + 1))
            ]
        )

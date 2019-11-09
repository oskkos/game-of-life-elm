module NextTick exposing (tick)

import Array
import CellToggle exposing (getRow, toggle)


tick grid =
    let
        rows =
            Array.length grid

        cells =
            Array.length (getRow grid 0)
    in
    tickRow grid 0 rows cells


tickRow grid x rows cells =
    if x < rows then
        tickRow (tickCell grid x 0 cells) (x + 1) rows cells

    else
        grid

tickCell grid x y cells =
    if y < cells then
        tickCell (toggle grid x y) x (y+1) cells
    else
        grid

module CellToggle exposing (getRow, isToggled, toggle)

import Array


isToggled grid x y =
    getCell (getRow grid x) y


invert grid x y =
    not (isToggled grid x y)


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


getCell arr pos =
    let
        cellValue =
            case Array.get pos arr of
                Just a ->
                    a

                Nothing ->
                    False
    in
    cellValue

module CellToggle exposing (toggle)

import Array


toggle grid x y =
    let
        r =
            getRow grid x

        c =
            getCell r y

        r2 =
            Array.set y (not c) r

        grid2 =
            Array.set x r2 grid
    in
    grid2


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

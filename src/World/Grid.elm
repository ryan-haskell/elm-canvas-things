module World.Grid exposing
    ( Grid
    , get
    , init
    , plant
    )

import Dict exposing (Dict)
import Random as R
import Set


type Grid comparable value
    = Grid (Data comparable value)


type alias Data comparable value =
    { seed : R.Seed
    , generator : R.Generator comparable
    , default : value
    , values : Dict comparable value
    }


init :
    Int
    -> value
    -> R.Generator comparable
    -> Grid comparable value
init seedNumber default generator =
    let
        ( spawn, seed ) =
            R.initialSeed seedNumber |> R.step generator
    in
    Grid
        { seed = seed
        , generator = generator
        , default = default
        , values = Dict.empty |> Dict.insert spawn default
        }



-- PLANTING
-- 1. Starts with `value depth`
-- 2. Spreads to neighbors with `chance`
-- 3. If success, plants `value depth`, recurses on neighbors
-- 4. If failure, plants `value (depth - 1)` and recurses if `depth > 0`.


type alias PlantingOptions =
    { depth : Int
    , chance : Float
    }


plant :
    (comparable -> List comparable)
    -> PlantingOptions
    -> (Int -> value)
    -> Grid comparable value
    -> Grid comparable value
plant neighborsOf options valueWith (Grid data) =
    let
        ( randomLocation, seed ) =
            R.step data.generator data.seed

        locations : List comparable
        locations =
            [ randomLocation ]
    in
    plant_
        (List.foldl insertTrue Dict.empty locations)
        locations
        neighborsOf
        options
        valueWith
        (Grid { data | seed = seed })


insertTrue : comparable -> Dict comparable Bool -> Dict comparable Bool
insertTrue key dict =
    Dict.insert key True dict


plant_ :
    Dict comparable Bool
    -> List comparable
    -> (comparable -> List comparable)
    -> PlantingOptions
    -> (Int -> value)
    -> Grid comparable value
    -> Grid comparable value
plant_ visited locations neighborsOf options valueWith (Grid data) =
    let
        unvisitedNeighbors : List comparable
        unvisitedNeighbors =
            locations
                |> List.map neighborsOf
                |> List.concat
                |> List.filter (\key -> not (Dict.member key visited))
                |> Set.fromList
                |> Set.toList

        ( depth, grid ) =
            locations
                |> List.foldl (plantBois options valueWith) ( options.depth, data.values, data.seed )
                |> (\( depth_, values, seed ) ->
                        ( depth_
                        , Grid { data | values = values, seed = seed }
                        )
                   )
    in
    if List.isEmpty unvisitedNeighbors then
        grid

    else
        plant_
            (List.foldl insertTrue visited unvisitedNeighbors)
            unvisitedNeighbors
            neighborsOf
            { options | depth = depth }
            valueWith
            grid


plantBois :
    PlantingOptions
    -> (Int -> value)
    -> comparable
    -> ( Int, Dict comparable value, R.Seed )
    -> ( Int, Dict comparable value, R.Seed )
plantBois options valueWith location ( depth, dict, seed ) =
    let
        ( randomPercent, newSeed ) =
            R.step (R.float 0 1) seed
    in
    if depth < 1 then
        ( 0
        , dict
        , newSeed
        )

    else if randomPercent < options.chance then
        ( depth
        , Dict.insert location (valueWith depth) dict
        , newSeed
        )

    else
        ( depth - 1
        , Dict.insert location (valueWith depth) dict
        , newSeed
        )


-- GET


get :
    comparable
    -> Grid comparable value
    -> value
get comparable (Grid { default, values }) =
    Dict.get comparable values
        |> Maybe.withDefault default

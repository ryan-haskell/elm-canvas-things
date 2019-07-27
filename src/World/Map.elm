module World.Map exposing
    ( Map
    , Tile(..)
    , get
    , init
    , neighbors
    )

import Random as R
import World.Grid as Grid exposing (Grid)


type Map
    = Map Int (Grid ( Int, Int ) Tile)


type Tile
    = Grass
    | Water
    | Tree
    | Village
    | Town


type alias Options =
    { seed : Int
    , size : Int
    , tile : Tile
    , tiles : List PlantOptions
    }


type alias PlantOptions =
    { count : Int
    , tile : Tile
    , depth : Int
    , chance : Float
    }


init : Options -> Map
init { seed, size, tile, tiles } =
    Map size
        (List.foldl
            (plant size)
            (Grid.init
                seed
                tile
                (coordinateGenerator size)
            )
            tiles
        )


plant : Int -> PlantOptions -> Grid ( Int, Int ) Tile -> Grid ( Int, Int ) Tile
plant size options grid =
    if options.count > 0 then
        plant
            size
            { options | count = options.count - 1 }
            (Grid.plant
                (neighbors size)
                { depth = options.depth, chance = options.chance }
                (always options.tile)
                grid
            )

    else
        grid


coordinateGenerator : Int -> R.Generator ( Int, Int )
coordinateGenerator size =
    R.map2 (\x y -> ( x, y ))
        (R.int 0 size)
        (R.int 0 size)


neighbors : Int -> ( Int, Int ) -> List ( Int, Int )
neighbors size ( x, y ) =
    let
        operator =
            if modBy 2 x == 1 then
                (+)

            else
                (-)
    in
    [ ( x, y - 1 )
    , ( x, y + 1 )
    , ( x - 1, y )
    , ( x - 1, operator y 1 )
    , ( x + 1, y )
    , ( x + 1, operator y 1 )
    ]
        |> List.map (Tuple.mapBoth (modBy size) (modBy size))


get : ( Int, Int ) -> Map -> Tile
get ( x, y ) (Map _ grid) =
    Grid.get ( x, y ) grid



-- midpoint


midpoint : Int -> ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
midpoint size loc1 loc2 =
    let
        manhattan : ( Int, Int ) -> ( Int, Int ) -> Int
        manhattan ( xa, ya ) ( xb, yb ) =
            (xb - xa) * (xb - xa) + (yb - ya) * (yb - ya)

        addSize =
            Tuple.mapBoth ((+) size) ((+) size)

        loc1Big =
            addSize loc1

        loc2Big =
            addSize loc2

        pairs =
            [ ( loc1, loc2 )
            , ( loc1Big, loc2 )
            , ( loc1, loc2Big )
            ]

        getMiddle a b =
            if a - b > 0 then
                (a - b) // 2 + b

            else
                (b - a) // 2 + a

        mp ( x1, y1 ) ( x2, y2 ) =
            Tuple.mapBoth
                (modBy size)
                (modBy size)
                ( getMiddle x1 x2
                , getMiddle y1 y2
                )
    in
    pairs
        |> List.map (\( a, b ) -> { pairs = ( a, b ), distance = manhattan a b })
        |> List.foldl
            (\current min ->
                if current.distance < min.distance then
                    current

                else
                    min
            )
            { pairs = ( loc1, loc2 ), distance = manhattan loc1 loc2 }
        |> (\winner ->
                mp (Tuple.first winner.pairs) (Tuple.second winner.pairs)
           )

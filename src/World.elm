module World exposing
    ( World
    , get
    , init
    )

import World.Map as Map exposing (Map)


type World
    = World Map


type alias Configuration =
    { seed : Int
    , size : Int
    }


type alias Settings =
    { count : Int
    , depth : Int
    , chance : Int
    }


percentify : Int -> Float
percentify chance =
    toFloat (Basics.max 0 (Basics.min 100 chance)) / 100


options =
    { water =
        { count = 75
        , depth = 4
        , chance = 75
        }
    , trees =
        { count = 75
        , depth = 12
        , chance = 75
        }
    , towns =
        { count = 6
        , depth = 32
        , chance = 65
        }
    , villages =
        { count = 12
        , depth = 4
        , chance = 65
        }
    }


init : Configuration -> World
init config =
    World
        (Map.init
            { seed = config.seed
            , size = config.size
            , tile = Map.Grass
            , tiles =
                [ { tile = Map.Tree
                  , count = round <| percentify options.trees.count * toFloat config.size
                  , depth = options.trees.depth
                  , chance = percentify options.trees.chance
                  }
                , { tile = Map.Water
                  , count = round <| percentify options.water.count * toFloat config.size
                  , depth = options.water.depth
                  , chance = percentify options.water.chance
                  }
                , { tile = Map.Town
                  , count = round <| percentify options.towns.count * toFloat config.size
                  , depth = options.towns.depth
                  , chance = percentify options.towns.chance
                  }
                , { tile = Map.Village
                  , count = round <| percentify options.villages.count * toFloat config.size
                  , depth = options.villages.depth
                  , chance = percentify options.villages.chance
                  }
                ]
            }
        )


get : ( Int, Int ) -> World -> String
get ( x, y ) (World map) =
    case Map.get ( x, y ) map of
        Map.Grass ->
            "#396"

        Map.Tree ->
            "#274"

        Map.Town ->
            "#765"

        Map.Village ->
            "#444"

        Map.Water ->
            "#49f"

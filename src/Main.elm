module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Dom exposing (..)
import Browser.Events
import Canvas exposing (Canvas, Drawable(..))
import Html exposing (..)
import Html.Attributes exposing (style, value)
import Html.Events as Events
import Json.Decode as D exposing (Decoder)
import Ports
import Process
import Task
import World exposing (World)


type alias Flags =
    ()


type alias Model =
    { seed : String
    , world : Maybe World
    , position : Position
    , player : Player
    , input : InputState
    , viewport : Maybe Viewport
    }


type alias Position =
    { x : Float
    , y : Float
    }


type alias Viewport =
    { width : Int
    , height : Int
    }


type Msg
    = SetViewport Viewport
    | UpdateSeed String
    | GenerateWorld String
    | KeyDown Key
    | KeyUp Key
    | OnTouchEvent Position
    | OnGamepadEvent Ports.GamepadInfo
    | OnAnimationFrame Float


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


worldFromSeed : String -> World
worldFromSeed =
    String.toList
        >> List.map Char.toCode
        >> List.sum
        >> (\seed -> World.init { seed = seed, size = worldSize })


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( Model
        "jangle"
        Nothing
        { x = 0, y = 0 }
        { direction = Right, status = Idle }
        { x = Nothing, y = Nothing }
        Nothing
    , Cmd.batch
        [ Browser.Dom.getViewport
            |> Task.map toViewport
            |> Task.perform SetViewport
        , delayCmd (GenerateWorld "jangle")
        ]
    )


delayCmd : msg -> Cmd msg
delayCmd msg =
    Process.sleep 1000
        |> Task.perform (\_ -> msg)


dimensions =
    { cols = 16
    , rows = 9
    }


worldSize =
    24


data : World -> Player -> { x : Float, y : Float } -> Viewport -> Canvas
data world player position viewport =
    Canvas
        viewport
        "#333"
        (hexGrid
            world
            { cols = worldSize
            , rows = worldSize
            }
            (sizeFor viewport)
            dimensions
            position
            ++ [ playerData viewport player ]
        )


type alias Player =
    { direction : DirectionX
    , status : PlayerState
    }


animationFrame : Player -> Int
animationFrame player =
    case player.status of
        Running frame _ ->
            frame

        Idle ->
            0


playerRunAnimation : Array Int
playerRunAnimation =
    -- Should have 60 elements
    Array.fromList <|
        List.concat
            [ List.repeat 6 0
            , List.repeat 9 1
            , List.repeat 6 0
            , List.repeat 9 2
            , List.repeat 6 0
            , List.repeat 9 1
            , List.repeat 6 0
            , List.repeat 9 2
            ]


playerData : Viewport -> Player -> Drawable
playerData viewport player =
    SpritesheetImage
        { url = "/public/running-dood.png"
        , x = toFloat viewport.width / 2 - (sizeFor viewport / 2)
        , y = toFloat viewport.height / 2 - (sizeFor viewport / 2)
        , width = sizeFor viewport
        , height = sizeFor viewport
        , subImage =
            { x =
                (Array.get (animationFrame player) playerRunAnimation
                    |> Maybe.map toFloat
                    |> Maybe.withDefault 0
                )
                    * 16
            , y =
                case player.direction of
                    Left ->
                        16

                    Right ->
                        0
            , width = 16
            , height = 16
            }
        }


sizeFor : Viewport -> Float
sizeFor { width, height } =
    max (toFloat width / dimensions.cols)
        (toFloat height / dimensions.rows)


type alias Dimensions =
    { cols : Int
    , rows : Int
    }



-- same for now, lel.


hexGrid : World -> Dimensions -> Float -> Dimensions -> { x : Float, y : Float } -> List Drawable
hexGrid world worldDimensions size { cols, rows } { x, y } =
    let
        width =
            2 * size

        height =
            sqrt 3 * size

        buffer =
            1

        ( playerXIndex, playerYIndex ) =
            ( floor (x / size / 1.5)
            , floor (y / height)
            )

        ( leftX, rightX ) =
            ( playerXIndex - buffer
            , playerXIndex + cols - 1 + buffer
            )

        ( leftY, rightY ) =
            ( playerYIndex - buffer
            , playerYIndex + rows - 1 + buffer
            )
    in
    -- TODO: Build in reverse with foldl and ::
    List.map
        (\yIndex ->
            List.map
                (\xIndex ->
                    let
                        xPos =
                            (toFloat xIndex * 0.75 * width) - x

                        yPos =
                            (if modBy 2 xIndex == 0 then
                                toFloat yIndex * height

                             else
                                toFloat yIndex * height + (0.5 * height)
                            )
                                - y
                    in
                    hexagon
                        { x = xPos
                        , y = yPos
                        , width = width
                        , height = height
                        }
                        (World.get
                            ( modBy worldSize xIndex
                            , modBy worldSize yIndex
                            )
                            world
                        )
                )
                (List.range leftX rightX)
        )
        (List.range leftY rightY)
        |> List.concat


hexagon : { x : Float, y : Float, width : Float, height : Float } -> String -> Drawable
hexagon { width, height, x, y } color =
    Polygon
        { color = color
        , path =
            [ ( 0, 0.5 )
            , ( 0.246, 0 )
            , ( 0.755, 0 )
            , ( 1.0, 0.5 )
            , ( 0.755, 1.01 )
            , ( 0.246, 1.01 )
            ]
                |> List.map
                    (Tuple.mapBoth
                        (\a -> x + a * width)
                        (\b -> y + b * height)
                    )
        }


toViewport { viewport } =
    { width = floor viewport.width
    , height = floor viewport.height
    }



-- UPDATE


{-| TODO: Store this in the model, and derive the InputState from it.
There's weird stuff happening now when you let go of WASD at the wrong time,
because we aren't keeping track of pressed keys.

type BetterInputState
= Keyboard KeyboardState
| Gamepad GamepadState
| TouchDevice TouchDeviceState

type alias KeyboardState =
List Key

type alias GamepadState =
{ joystick : Position
}

type alias TouchDeviceState =
{ dragging : Position
}
|

-}
type alias InputState =
    { x : Maybe DirectionX
    , y : Maybe DirectionY
    }


type PlayerState
    = Running Int ( Maybe DirectionX, Maybe DirectionY )
    | Idle


actionFromInput : InputState -> Player -> PlayerState
actionFromInput input player =
    case ( input.x, input.y ) of
        ( Nothing, Nothing ) ->
            Idle

        ( x, y ) ->
            Running (modBy 60 <| animationFrame player + 1) ( x, y )


type Key
    = ArrowUp
    | ArrowLeft
    | ArrowRight
    | ArrowDown
    | Space


type DirectionX
    = Left
    | Right


type DirectionY
    = Up
    | Down


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSeed seed ->
            ( { model | seed = seed, world = Nothing }
            , delayCmd (GenerateWorld seed)
            )

        GenerateWorld seed ->
            if model.seed == seed then
                ( { model | world = Just (worldFromSeed seed) }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        KeyDown datKey ->
            case datKey of
                Space ->
                    model.viewport
                        |> Maybe.map (\viewport -> render viewport model 16)
                        |> Maybe.withDefault ( model, Cmd.none )

                key ->
                    ( { model | input = updateKey True key model.input }
                    , Cmd.none
                    )

        KeyUp key ->
            ( { model | input = updateKey False key model.input }
            , Cmd.none
            )

        SetViewport viewport ->
            ( { model | viewport = Just viewport }
            , Cmd.none
            )

        OnTouchEvent position ->
            ( { model | input = inputFromPosition position }
            , Cmd.none
            )

        OnGamepadEvent gamepad ->
            ( { model
                | input = inputFromPosition gamepad
                , seed =
                    if gamepad.a then
                        gamepad.now

                    else
                        model.seed
              }
            , if gamepad.a then
                delayCmd (GenerateWorld gamepad.now)

              else
                Cmd.none
            )

        OnAnimationFrame ms ->
            model.viewport
                |> Maybe.map (\viewport -> render viewport model ms)
                |> Maybe.withDefault ( model, Cmd.none )


inputFromPosition : { a | x : Float, y : Float } -> InputState
inputFromPosition { x, y } =
    let
        threshold =
            0.7
    in
    { x =
        if x < -1 * threshold then
            Just Left

        else if x > threshold then
            Just Right

        else
            Nothing
    , y =
        if y < -1 * threshold then
            Just Up

        else if y > threshold then
            Just Down

        else
            Nothing
    }


render : Viewport -> Model -> Float -> ( Model, Cmd Msg )
render viewport model ms =
    ( { model
        | position = updatePosition (sizeFor viewport) ms model.input model.position
        , player = updatePlayer model.input model.player
      }
    , case model.world of
        Nothing ->
            Cmd.none

        Just world ->
            Cmd.batch
                [ model.viewport
                    |> Maybe.map (data world model.player model.position >> Ports.render)
                    |> Maybe.withDefault Cmd.none
                , Ports.requestGamepad
                ]
    )


updatePlayer : InputState -> Player -> Player
updatePlayer input player =
    { player
        | direction = input.x |> Maybe.withDefault player.direction
        , status = actionFromInput input player
    }


valueFor : Bool -> a -> Maybe a
valueFor bool x =
    if bool then
        Just x

    else
        Nothing


updateKey : Bool -> Key -> InputState -> InputState
updateKey wasPressed key input =
    case key of
        ArrowUp ->
            { input | y = valueFor wasPressed Up }

        ArrowDown ->
            { input | y = valueFor wasPressed Down }

        ArrowLeft ->
            { input | x = valueFor wasPressed Left }

        ArrowRight ->
            { input | x = valueFor wasPressed Right }

        Space ->
            input


updatePosition : Float -> Float -> InputState -> Position -> Position
updatePosition tileSize ms input position =
    let
        speed =
            ms * tileSize / 250
    in
    { position
        | x = position.x + (xFrom input.x * speed)
        , y = position.y + (yFrom input.y * speed)
    }


xFrom : Maybe DirectionX -> Float
xFrom dir =
    case dir of
        Just Left ->
            -1

        Just Right ->
            1

        Nothing ->
            0


yFrom : Maybe DirectionY -> Float
yFrom dir =
    case dir of
        Just Up ->
            -1

        Just Down ->
            1

        Nothing ->
            0



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize (\w h -> SetViewport (Viewport w h))
        , Browser.Events.onKeyDown (keydownDecoder KeyDown)
        , Browser.Events.onKeyUp (keydownDecoder KeyUp)
        , Browser.Events.onAnimationFrameDelta OnAnimationFrame
        , Ports.onTouchEvent OnTouchEvent
        , Ports.onGamepadEvent OnGamepadEvent
        ]


keydownDecoder : (Key -> msg) -> Decoder msg
keydownDecoder msg =
    D.field "key" D.string
        |> D.andThen
            (\key ->
                case key of
                    "w" ->
                        D.succeed ArrowUp

                    "a" ->
                        D.succeed ArrowLeft

                    "s" ->
                        D.succeed ArrowDown

                    "d" ->
                        D.succeed ArrowRight

                    " " ->
                        D.succeed Space

                    _ ->
                        D.fail ""
            )
        |> D.map msg



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ style "margin-bottom" "0.5rem" ]
            [ text
                (model.world
                    |> Maybe.map (always "Use WASD, a controller, or touch controls to move!")
                    |> Maybe.withDefault "Loading..."
                )
            ]
        , div []
            [ span [] [ text "Seed: " ]
            , input [ value model.seed, Events.onInput UpdateSeed ] []
            ]
        ]

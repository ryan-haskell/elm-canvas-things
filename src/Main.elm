module Main exposing (main)

import Browser
import Browser.Dom exposing (..)
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (style)
import Json.Decode as D exposing (Decoder)
import Ports
import RenderData exposing (Drawable(..), RenderData)
import Task


type alias Flags =
    ()


type alias Model =
    { position : Position
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
    | KeyDown Key
    | KeyUp Key
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


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( Model
        { x = 0, y = 0 }
        { x = Nothing, y = Nothing }
        Nothing
    , Browser.Dom.getViewport
        |> Task.map toViewport
        |> Task.perform SetViewport
    )


dimensions =
    { cols = 16
    , rows = 9
    }


data : { x : Float, y : Float } -> Viewport -> RenderData
data position viewport =
    RenderData
        viewport
        "#333"
        (hexGrid (sizeFor viewport) dimensions position)


sizeFor : Viewport -> Float
sizeFor { width, height } =
    min (toFloat width / dimensions.cols)
        (toFloat height / dimensions.rows)


type alias Dimensions =
    { cols : Int
    , rows : Int
    }



-- same for now, lel.


hexGrid : Float -> Dimensions -> { x : Float, y : Float } -> List Drawable
hexGrid size { cols, rows } { x, y } =
    let
        width =
            2 * size

        height =
            sqrt 3 * size
    in
    List.map
        (\yIndex ->
            List.map
                (\xIndex ->
                    hexagon ( width, height )
                        ( (-1 * x) + (toFloat xIndex * 0.75 * width)
                        , (-1 * y)
                            + (if modBy 2 xIndex == 0 then
                                toFloat yIndex * 1 * height

                               else
                                toFloat yIndex * 1 * height + (0.5 * height)
                              )
                        )
                        "#0c6"
                )
                (List.range 0 (cols - 1))
        )
        (List.range 0 (rows - 1))
        |> List.concat


hexagon : ( Float, Float ) -> ( Float, Float ) -> String -> Drawable
hexagon ( width, height ) ( x, y ) color =
    Polygon
        { color = color
        , path =
            [ ( 0, 0.5 )
            , ( 0.25, 0 )
            , ( 0.75, 0 )
            , ( 1, 0.5 )
            , ( 0.75, 1 )
            , ( 0.25, 1 )
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


type alias InputState =
    { x : Maybe DirectionX
    , y : Maybe DirectionY
    }


type PlayerAction
    = MovingIn ( Maybe DirectionX, Maybe DirectionY )
    | JustHangingOutIGuessIdkMaybeHesLazyOrSomething


actionFromInput : InputState -> PlayerAction
actionFromInput input =
    case ( input.x, input.y ) of
        ( Nothing, Nothing ) ->
            JustHangingOutIGuessIdkMaybeHesLazyOrSomething

        ( x, y ) ->
            MovingIn ( x, y )


type Key
    = ArrowUp
    | ArrowLeft
    | ArrowRight
    | ArrowDown


type DirectionX
    = Left
    | Right


type DirectionY
    = Up
    | Down


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown key ->
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

        OnAnimationFrame ms ->
            ( { model | position = updatePosition ms model.input model.position }
            , model.viewport
                |> Maybe.map (data model.position >> Ports.render)
                |> Maybe.withDefault Cmd.none
            )


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


updatePosition : Float -> InputState -> Position -> Position
updatePosition ms input position =
    let
        speed =
            1 * ms
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

                    _ ->
                        D.fail ""
            )
        |> D.map msg



-- VIEW


view : Model -> Html msg
view model =
    text ""

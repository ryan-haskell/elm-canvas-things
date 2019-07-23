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
    { offset : { x : Int, y : Int }
    , viewport : Maybe Viewport
    }


type alias Viewport =
    { width : Int
    , height : Int
    }


type Msg
    = SetViewport Viewport
    | MoveIn Direction
    | OnAnimationFrame


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
    ( Model { x = 0, y = 0 } Nothing
    , Browser.Dom.getViewport
        |> Task.map toViewport
        |> Task.perform SetViewport
    )


data : { x : Int, y : Int } -> Viewport -> RenderData
data offset viewport =
    RenderData
        viewport
        "#333"
        [ Rectangle
            { color = "#36f"
            , x = 50
            , y = 50
            , width = 100
            , height = 50
            }
        , Image
            { url = "https://images.unsplash.com/photo-1518791841217-8f162f1e1131?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&w=100&q=80"
            , x = 25 + toFloat offset.x
            , y = 25 + toFloat offset.y
            }
        ]


toViewport { viewport } =
    { width = floor viewport.width
    , height = floor viewport.height
    }



-- UPDATE


type Direction
    = Up
    | Down
    | Left
    | Right


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveIn direction ->
            ( case direction of
                Up ->
                    { model
                        | offset =
                            { x = model.offset.x
                            , y = model.offset.y - 1
                            }
                    }

                Down ->
                    { model
                        | offset =
                            { x = model.offset.x
                            , y = model.offset.y + 1
                            }
                    }

                Left ->
                    { model
                        | offset =
                            { x = model.offset.x - 1
                            , y = model.offset.y
                            }
                    }

                Right ->
                    { model
                        | offset =
                            { x = model.offset.x + 1
                            , y = model.offset.y
                            }
                    }
            , Cmd.none
            )

        SetViewport viewport ->
            ( { model | viewport = Just viewport }
            , Cmd.none
            )

        OnAnimationFrame ->
            ( model
            , model.viewport
                |> Maybe.map (data model.offset >> Ports.render)
                |> Maybe.withDefault Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize (\w h -> SetViewport (Viewport w h))
        , Browser.Events.onKeyDown (keydownDecoder MoveIn)
        , Browser.Events.onAnimationFrame (always OnAnimationFrame)
        ]


keydownDecoder : (Direction -> msg) -> Decoder msg
keydownDecoder msg =
    D.field "key" D.string
        |> D.andThen
            (\key ->
                case key of
                    "w" ->
                        D.succeed Up

                    "a" ->
                        D.succeed Left

                    "s" ->
                        D.succeed Down

                    "d" ->
                        D.succeed Right

                    _ ->
                        D.fail ""
            )
        |> D.map msg



-- VIEW


view : Model -> Html msg
view model =
    text ""

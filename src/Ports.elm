port module Ports exposing
    ( GamepadInfo
    , onGamepadEvent
    , onTouchEvent
    , render
    , requestGamepad
    )

import Canvas exposing (Canvas)
import Codec exposing (Codec, Value)
import Json.Encode as Json


port onTouch : ({ x : Float, y : Float } -> msg) -> Sub msg


port onGamepad : (GamepadInfo -> msg) -> Sub msg


port outgoing : OutgoingMsg -> Cmd msg


type alias OutgoingMsg =
    { action : String
    , payload : Value
    }


type alias GamepadInfo =
    { x : Float
    , y : Float
    , a : Bool
    , now : String
    }



-- TOUCH SUPPORT


onTouchEvent : ({ x : Float, y : Float } -> msg) -> Sub msg
onTouchEvent =
    onTouch



-- GAMEPAD SUPPORT


onGamepadEvent : (GamepadInfo -> msg) -> Sub msg
onGamepadEvent =
    onGamepad



-- CANVAS RENDERING


render : Canvas -> Cmd msg
render =
    Codec.encoder Canvas.codec >> OutgoingMsg "RENDER" >> outgoing


requestGamepad : Cmd msg
requestGamepad =
    outgoing <|
        { action = "REQUEST_GAMEPAD"
        , payload = Json.null
        }

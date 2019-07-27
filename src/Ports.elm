port module Ports exposing (onTouchEvent, render)

import Codec exposing (Codec, Value)
import RenderData exposing (RenderData)


port incoming : ({ x : Float, y : Float } -> msg) -> Sub msg


port outgoing : OutgoingMsg -> Cmd msg


type alias OutgoingMsg =
    { action : String
    , payload : Value
    }



-- TOUCH SUPPORT


onTouchEvent : ({ x : Float, y : Float } -> msg) -> Sub msg
onTouchEvent =
    incoming



-- CANVAS RENDERING


render : RenderData -> Cmd msg
render =
    Codec.encoder RenderData.codec >> OutgoingMsg "RENDER" >> outgoing

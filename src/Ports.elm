port module Ports exposing (render, withStorage)

import Codec exposing (Codec, Value)
import RenderData exposing (RenderData)


port outgoing : OutgoingMsg -> Cmd msg


type alias OutgoingMsg =
    { action : String
    , payload : Value
    }



-- CANVAS RENDERING


render : RenderData -> Cmd msg
render =
    Codec.encoder RenderData.codec >> OutgoingMsg "RENDER" >> outgoing



-- LOCAL STORAGE


store : Value -> OutgoingMsg
store =
    OutgoingMsg "STORE"


withStorage :
    (model -> Value)
    -> (msg -> model -> ( model, Cmd msg ))
    -> msg
    -> model
    -> ( model, Cmd msg )
withStorage encode update msg model =
    let
        ( model_, cmd_ ) =
            update msg model
    in
    ( model_
    , Cmd.batch
        [ outgoing (store (encode model_))
        , cmd_
        ]
    )

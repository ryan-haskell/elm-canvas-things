module RenderData exposing (Drawable(..), RenderData, codec)

import Codec exposing (Codec, Value)


type alias RenderData =
    { size : Viewport
    , background : String
    , items : List Drawable
    }


type alias Viewport =
    { width : Int
    , height : Int
    }


type Drawable
    = Rectangle RectangleData
    | Image ImageData


type alias ImageData =
    { url : String
    , x : Float
    , y : Float
    }


type alias RectangleData =
    { color : String
    , x : Float
    , y : Float
    , width : Float
    , height : Float
    }


codec : Codec RenderData
codec =
    Codec.object RenderData
        |> Codec.field "size" .size viewportCodec
        |> Codec.field "background" .background Codec.string
        |> Codec.field "items" .items (Codec.list drawableCodec)
        |> Codec.buildObject


viewportCodec : Codec Viewport
viewportCodec =
    Codec.object Viewport
        |> Codec.field "width" .width Codec.int
        |> Codec.field "height" .height Codec.int
        |> Codec.buildObject


drawableCodec : Codec Drawable
drawableCodec =
    Codec.custom
        (\fRectangle fImage value ->
            case value of
                Rectangle data ->
                    fRectangle data

                Image data ->
                    fImage data
        )
        |> Codec.variant1 "rectangle" Rectangle rectangleDataCodec
        |> Codec.variant1 "image" Image imageDataCodec
        |> Codec.buildCustom


rectangleDataCodec : Codec RectangleData
rectangleDataCodec =
    Codec.object RectangleData
        |> Codec.field "color" .color Codec.string
        |> Codec.field "x" .x Codec.float
        |> Codec.field "y" .y Codec.float
        |> Codec.field "width" .width Codec.float
        |> Codec.field "height" .height Codec.float
        |> Codec.buildObject


imageDataCodec : Codec ImageData
imageDataCodec =
    Codec.object ImageData
        |> Codec.field "url" .url Codec.string
        |> Codec.field "x" .x Codec.float
        |> Codec.field "y" .y Codec.float
        |> Codec.buildObject

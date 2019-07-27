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
    | Polygon PolygonData
    | Image ImageData
    | SpritesheetImage SpritesheetImageData


type alias ImageData =
    { url : String
    , x : Float
    , y : Float
    }


type alias SpritesheetImageData =
    { url : String
    , x : Float
    , y : Float
    , width : Float
    , height : Float
    , subImage : SubImageData
    }


type alias SubImageData =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type alias RectangleData =
    { color : String
    , x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type alias PolygonData =
    { color : String
    , path : List ( Float, Float )
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
        (\fRectangle fPolygon fImage fSpritesheetImage value ->
            case value of
                Rectangle data ->
                    fRectangle data

                Polygon data ->
                    fPolygon data

                Image data ->
                    fImage data

                SpritesheetImage data ->
                    fSpritesheetImage data
        )
        |> Codec.variant1 "rectangle" Rectangle rectangleDataCodec
        |> Codec.variant1 "polygon" Polygon polygonDataCodec
        |> Codec.variant1 "image" Image imageDataCodec
        |> Codec.variant1 "image" SpritesheetImage spritesheetImageDataCodec
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


polygonDataCodec : Codec PolygonData
polygonDataCodec =
    Codec.object PolygonData
        |> Codec.field "color" .color Codec.string
        |> Codec.field "path" .path (Codec.list (Codec.tuple Codec.float Codec.float))
        |> Codec.buildObject


imageDataCodec : Codec ImageData
imageDataCodec =
    Codec.object ImageData
        |> Codec.field "url" .url Codec.string
        |> Codec.field "x" .x Codec.float
        |> Codec.field "y" .y Codec.float
        |> Codec.buildObject


spritesheetImageDataCodec : Codec SpritesheetImageData
spritesheetImageDataCodec =
    Codec.object SpritesheetImageData
        |> Codec.field "url" .url Codec.string
        |> Codec.field "x" .x Codec.float
        |> Codec.field "y" .y Codec.float
        |> Codec.field "width" .width Codec.float
        |> Codec.field "height" .height Codec.float
        |> Codec.field "sprite" .subImage subImageDataCodec
        |> Codec.buildObject


subImageDataCodec : Codec SubImageData
subImageDataCodec =
    Codec.object SubImageData
        |> Codec.field "x" .x Codec.float
        |> Codec.field "y" .y Codec.float
        |> Codec.field "width" .width Codec.float
        |> Codec.field "height" .height Codec.float
        |> Codec.buildObject

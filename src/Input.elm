module Input exposing
    ( Input
    , Key(..)
    , fromGamepad
    , fromKeyboardMouse
    , fromTouchDevice
    )


type Input
    = Keyboard (List Key)
    | Gamepad { x : Float, y : Float }
    | TouchDevice { x : Float, y : Float }


type Key
    = ArrowUp
    | ArrowLeft
    | ArrowRight
    | ArrowDown
    | Space


fromGamepad : { x : Float, y : Float } -> Input
fromGamepad =
    Gamepad


fromTouchDevice : { x : Float, y : Float } -> Input
fromTouchDevice =
    TouchDevice


fromKeyboardMouse : List Key -> Input
fromKeyboardMouse =
    Keyboard

pressKey : Key -> Input -> Input
pressKey key input =
    case input of
        Keyboard keys ->
            if List.member key keys then
                Keyboard keys
            else
                Keyboard (key :: keys)
    
        option2 ->
            


type alias KeyboardState =
    List Key

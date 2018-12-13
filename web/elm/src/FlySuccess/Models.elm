module FlySuccess.Models exposing (ButtonState(..), TokenTransfer(..), hover, isClicked, isPending)


type ButtonState
    = Unhovered
    | Hovered
    | Clicked


type TokenTransfer
    = Pending
    | Success
    | Failure


hover : Bool -> ButtonState -> ButtonState
hover hovered buttonState =
    case buttonState of
        Clicked ->
            Clicked

        _ ->
            if hovered then
                Hovered
            else
                Unhovered


isClicked : ButtonState -> Bool
isClicked =
    (==) Clicked


isPending : TokenTransfer -> Bool
isPending =
    (==) Pending

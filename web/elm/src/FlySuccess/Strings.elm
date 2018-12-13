module FlySuccess.Strings exposing (..)

import FlySuccess.Models as Models exposing (ButtonState, TokenTransfer)


titleText : String
titleText =
    "login successful!"


loginSuccessDetails : String
loginSuccessDetails =
    "return to fly OR go back to the Concourse dashboard"


buttonText : ButtonState -> String
buttonText buttonState =
    if Models.isClicked buttonState then
        "token copied"
    else
        "copy token to clipboard"

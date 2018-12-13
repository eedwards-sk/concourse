module FlySuccess.Styles exposing (..)

import FlySuccess.Models exposing (ButtonState(..), isClicked)
import Colors


card : List ( String, String )
card =
    [ ( "background-color", Colors.flySuccessCard )
    , ( "padding", "30px" )
    , ( "width", "330px" )
    , ( "margin", "50px auto" )
    , ( "display", "flex" )
    , ( "flex-direction", "column" )
    , ( "align-items", "center" )
    , ( "text-align", "center" )
    ]


title : List ( String, String )
title =
    [ ( "font-size", "18px" )
    , ( "margin", "0" )
    ]


body : List ( String, String )
body =
    [ ( "font-size", "14px" )
    , ( "margin", "10px 0" )
    ]


button : ButtonState -> List ( String, String )
button buttonState =
    [ ( "border"
      , "1px solid "
            ++ if isClicked buttonState then
                Colors.flySuccessTokenCopied
               else
                Colors.text
      )
    , ( "margin", "10px 0" )
    , ( "padding", "10px 0" )
    , ( "width", "212px" )
    , ( "cursor"
      , if isClicked buttonState then
            "default"
        else
            "pointer"
      )
    , ( "text-align", "center" )
    , ( "background-color"
      , case buttonState of
            Unhovered ->
                Colors.flySuccessCard

            Hovered ->
                Colors.flySuccessButtonHover

            Clicked ->
                Colors.flySuccessTokenCopied
      )
    ]

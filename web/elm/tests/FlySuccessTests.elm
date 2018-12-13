module FlySuccessTests exposing
    ( Assertion
    , Property
    , Query
    , Setup
    , SetupSteps
    , all
    , allCases
    , almostWhite
    , authToken
    , blue
    , body
    , bodyNoButton
    , bodyPendingText
    , bodyPosition
    , bodyStyle
    , bodySuccessText
    , button
    , buttonPosition
    , buttonPromptText
    , buttonProperties
    , buttonSize
    , buttonStyle
    , cardBackground
    , cardLayout
    , cardPosition
    , cardProperties
    , cardSize
    , cardTests
    , darkGrey
    , darkerGrey
    , property
    , setup
    , setupDesc
    , steps
    , successCard
    , tests
    , title
    , titleProperties
    , titleStyle
    , titleTests
    , titleText
    , tokenCopied
    , tokenSendFailed
    , tokenSendSuccess
    , whenOnFlySuccessPage
    )

import DashboardTests exposing (defineHoverBehaviour)
import Expect exposing (Expectation)
import FlySuccess
import FlySuccess.Strings as Strings
import Html.Attributes as Attr
import Layout
import SubPage
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector
    exposing
        ( attribute
        , containing
        , id
        , style
        , tag
        , text
        )



-- CONSTANTS (might be able to remove this and refer to "configuration"-type
-- files like Colors.elm)


almostWhite : String
almostWhite =
    "#e6e7e8"


darkGrey : String
darkGrey =
    "#323030"


darkerGrey : String
darkerGrey =
    "#242424"


blue : String
blue =
    "#196AC8"


authToken : String
authToken =
    "some_auth_token"



-- SETUPS (i dunno, maybe use fuzzers?)


type alias SetupSteps =
    () -> Layout.Model


type alias Setup =
    ( String, SetupSteps )


setupDesc : Setup -> String
setupDesc =
    Tuple.first


steps : Setup -> SetupSteps
steps =
    Tuple.second


setup : String -> SetupSteps -> Setup
setup =
    (,)


whenOnFlySuccessPage : Setup
whenOnFlySuccessPage =
    setup "when on fly success page"
        (\_ ->
            Layout.init
                { turbulenceImgSrc = ""
                , notFoundImgSrc = ""
                , csrfToken = ""
                , authToken = authToken
                , pipelineRunningKeyframes = ""
                }
                { href = ""
                , host = ""
                , hostname = ""
                , protocol = ""
                , origin = ""
                , port_ = ""
                , pathname = "/fly_success"
                , search = ""
                , hash = ""
                , username = ""
                , password = ""
                }
                |> Tuple.first
        )


tokenSendSuccess : Setup
tokenSendSuccess =
    setup "when token successfully sent to fly"
        (steps whenOnFlySuccessPage
            >> Layout.update
                (Layout.SubMsg 1 <|
                    SubPage.FlySuccessMsg <|
                        FlySuccess.TokenSentToFly True
                )
            >> Tuple.first
        )


tokenSendFailed : Setup
tokenSendFailed =
    setup "when token failed to send to fly"
        (steps whenOnFlySuccessPage
            >> Layout.update
                (Layout.SubMsg 1 <|
                    SubPage.FlySuccessMsg <|
                        FlySuccess.TokenSentToFly False
                )
            >> Tuple.first
        )


tokenCopied : Setup
tokenCopied =
    setup "when token copied to clipboard"
        (steps tokenSendFailed
            >> Layout.update
                (Layout.SubMsg 1 <|
                    SubPage.FlySuccessMsg <|
                        FlySuccess.CopyToken
                )
            >> Tuple.first
        )


allCases : List Setup
allCases =
    [ whenOnFlySuccessPage
    , tokenSendFailed
    , tokenSendSuccess
    ]



-- QUERIES


type alias Query =
    Layout.Model -> Query.Single Layout.Msg


successCard : Query
successCard =
    Layout.view
        >> Query.fromHtml
        >> Query.find [ id "success-card" ]


title : Query
title =
    successCard >> Query.find [ id "success-card-title" ]


body : Query
body =
    successCard >> Query.find [ id "success-card-body" ]


button : Query
button =
    body >> Query.find [ id "copy-token" ]



-- PROPERTIES


type alias Assertion =
    Query.Single Layout.Msg -> Expectation


type alias Property =
    Setup -> Test


property : Query -> String -> Assertion -> Property
property query description assertion setup =
    test (setupDesc setup ++ ", " ++ description) <|
        steps setup
            >> query
            >> assertion


cardProperties : List Property
cardProperties =
    [ cardBackground
    , cardSize
    , cardPosition
    , cardLayout
    ]


cardBackground : Property
cardBackground =
    property successCard "card has dark grey background" <|
        Query.has [ style [ ( "background-color", darkGrey ) ] ]


cardSize : Property
cardSize =
    property successCard "is 330px wide with 30px padding" <|
        Query.has
            [ style
                [ ( "padding", "30px" )
                , ( "width", "330px" )
                ]
            ]


cardPosition : Property
cardPosition =
    property successCard "is centered 50px from the top of the document" <|
        Query.has [ style [ ( "margin", "50px auto" ) ] ]


cardLayout : Property
cardLayout =
    property successCard "lays out contents vertically and center aligned" <|
        Query.has
            [ style
                [ ( "display", "flex" )
                , ( "flex-direction", "column" )
                , ( "align-items", "center" )
                , ( "text-align", "center" )
                ]
            ]


titleText : Property
titleText =
    property title "has success text" <|
        Query.has [ text "login successful!" ]


titleStyle : Property
titleStyle =
    property title "has 18px font size" <|
        Query.has [ style [ ( "font-size", "18px" ) ] ]


titleProperties : List Property
titleProperties =
    [ titleText
    , titleStyle
    ]


bodyPendingText : Property
bodyPendingText =
    property body "has pending text" <|
        Query.has [ text "sending token to fly..." ]


bodyNoButton : Property
bodyNoButton =
    property body "has no 'copy token' button" <|
        Query.hasNot [ id "copy-token" ]


bodySuccessText : Property
bodySuccessText =
    property body "has no 'copy token' button" <|
        Query.has [ text Strings.loginSuccessDetails ]


bodyStyle : Property
bodyStyle =
    property body "has 14px font" <|
        Query.has [ style [ ( "font-size", "14px" ) ] ]


bodyPosition : Property
bodyPosition =
    property body "has 10px margin above and below" <|
        Query.has [ style [ ( "margin", "10px 0" ) ] ]


buttonStyle : Property
buttonStyle =
    property button "display inline and has almost-white border" <|
        Query.has
            [ tag "span"
            , style [ ( "border", "1px solid " ++ almostWhite ) ]
            ]


buttonSize : Property
buttonSize =
    property button "is 212px wide with 10px padding above and below" <|
        Query.has
            [ style
                [ ( "width", "212px" )
                , ( "padding", "10px 0" )
                ]
            ]


buttonPosition : Property
buttonPosition =
    property button "has 10px margin above and below" <|
        Query.has [ style [ ( "margin", "10px 0" ) ] ]


buttonPromptText : Property
buttonPromptText =
    property button "says 'copy token to clipboard'" <|
        Query.has [ text "copy token to clipboard" ]


buttonCursor : Property
buttonCursor =
    property button "has pointer cursor" <|
        Query.has [ style [ ( "cursor", "pointer" ) ] ]


buttonClipboardAttr : Property
buttonClipboardAttr =
    property button "has attribute that is readable by clipboard.js" <|
        Query.has
            [ attribute <|
                Attr.attribute
                    "data-clipboard-text"
                    authToken
            ]


buttonProperties : List Property
buttonProperties =
    [ buttonStyle
    , buttonSize
    , buttonPosition
    , buttonClipboardAttr
    ]



-- TESTS


tests : List Setup -> List Property -> List Test
tests setups properties =
    setups
        |> List.concatMap
            (\setup -> List.map ((|>) setup) properties)


cardTests : List Test
cardTests =
    tests allCases cardProperties


titleTests : List Test
titleTests =
    tests allCases titleProperties


all : Test
all =
    describe "Fly login success page"
        [ describe "card" cardTests
        , describe "title" titleTests
        , describe "body"
            [ describe "style" <|
                tests allCases
                    [ bodyStyle
                    , bodyPosition
                    ]
            , whenOnFlySuccessPage |> bodyPendingText
            , whenOnFlySuccessPage |> bodyNoButton
            , tokenSendSuccess |> bodySuccessText
            ]
        , describe "button"
            [ tokenSendFailed |> buttonPromptText
            , describe "when token sending failed" <|
                tests [ tokenSendFailed ]
                    buttonProperties
            , defineHoverBehaviour
                { name = "copy token button"
                , setup = steps tokenSendFailed ()
                , query = button
                , updateFunc =
                    \msg ->
                        Layout.update msg
                            >> Tuple.first
                , unhoveredSelector =
                    { description =
                        "same background as card"
                    , selector =
                        [ style
                            [ ( "background-color"
                              , darkGrey
                              )
                            ]
                        ]
                    }
                , mouseEnterMsg =
                    Layout.SubMsg 1 <|
                        SubPage.FlySuccessMsg <|
                            FlySuccess.CopyTokenButtonHover
                                True
                , mouseLeaveMsg =
                    Layout.SubMsg 1 <|
                        SubPage.FlySuccessMsg <|
                            FlySuccess.CopyTokenButtonHover
                                False
                , hoveredSelector =
                    { description = "darker background"
                    , selector =
                        [ style
                            [ ( "background-color"
                              , darkerGrey
                              )
                            ]
                        ]
                    }
                }
            ]
        ]



-- all : Test
-- all =
--     describe "Fly login success card" <|
--         [ describe "body" <|
--             [ describe "after token fails to send to fly" <|
--                 [ describe "copy token button" <|
--                     , test "clicking sends CopyToken msg" <|
--                         tokenSendFailed
--                             >> copyTokenButton
--                             >> Event.simulate Event.click
--                             >> Event.expect
--                                 (Layout.SubMsg 1 <|
--                                     SubPage.FlySuccessMsg <|
--                                         FlySuccess.CopyToken
--                                 )
--                     , describe "receiving CopyToken msg" <|
--                         let
--                             tokenCopied :
--                                 ()
--                                 -> ( Layout.Model, Cmd Layout.Msg )
--                             tokenCopied =
--                                 tokenSendFailed
--                                     >> Layout.update
--                                         (Layout.SubMsg 1 <|
--                                             SubPage.FlySuccessMsg <|
--                                                 FlySuccess.CopyToken
--                                         )
--                         in
--                             [ test
--                                 ("changes button background "
--                                     ++ "and border to blue"
--                                 )
--                               <|
--                                 tokenCopied
--                                     >> Tuple.first
--                                     >> copyTokenButton
--                                     >> Query.has
--                                         [ style
--                                             [ ( "background-color"
--                                               , blue
--                                               )
--                                             , ( "border"
--                                               , "1px solid "
--                                                     ++ blue
--                                               )
--                                             ]
--                                         ]
--                             , test
--                                 ("changes button text to "
--                                     ++ "say 'token copied'"
--                                 )
--                               <|
--                                 tokenCopied
--                                     >> Tuple.first
--                                     >> copyTokenButton
--                                     >> Expect.all
--                                         [ Query.has
--                                             [ text
--                                                 "token copied"
--                                             ]
--                                         , Query.hasNot
--                                             [ text <|
--                                                 "copy token "
--                                                     ++ "to clipboard"
--                                             ]
--                                         ]
--                             , test
--                                 "changes button cursor to default"
--                               <|
--                                 tokenCopied
--                                     >> Tuple.first
--                                     >> copyTokenButton
--                                     >> Query.has
--                                         [ style
--                                             [ ( "cursor"
--                                               , "default"
--                                               )
--                                             ]
--                                         ]
--                             ]
--                     ]
--                 ]
--             ]
--         ]
